
MODULE Control_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains control information for the entire problem as well as
!    providing allocation of work arrays for a variety of purposes.
!
!  History:
!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!    Paul W. Eslinger :  4 Feb 2002 : Add STA_CONCEN and DET_CONCEN
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 18 Jul 2006 : (SCR-1137) Change logic before onset of irrigation
!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Add QA counter
!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels, add BMRDOS logic
!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!    Paul W. Eslinger : 15 Aug 2007 : Add NEED variables
!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!
      CHARACTER(LEN=200) :: PTITLE ! Title for this program run
!
      CHARACTER(LEN=10) :: SDATE ! Start date of processing (reused)
      CHARACTER(LEN=10) :: STIME ! Start time of processing (reused)
      CHARACTER(LEN=10) :: EDATE ! End date of processing
      CHARACTER(LEN=10) :: ETIME ! End time of processing
!
      LOGICAL :: REPORT  ! Flag whether the report file is open
      LOGICAL :: FOODS   ! Flag whether any foods will be written
      LOGICAL :: EXECUT  ! Flag whether the problem is to be executed (.false.=error check only)
!
      LOGICAL :: USE_RADS  ! Flag whether any radionuclides are being used
      LOGICAL :: NEED_KD   ! Flag whether Kd (soil-water) values need to be read from a data file
      LOGICAL :: NEED_IRIG ! Flag whether irrigated locations are used
!
      INTEGER :: SEASON_START ! Start day for the growing season
      INTEGER :: SEASON_END   ! End day for the growing season
      REAL    :: SEASON_GROW  ! Fraction of the year represented by the growing season
      REAL    :: IRIG_AMT     ! Amount of irrigation water applied (cm) in a year
      INTEGER :: IRIG_START   ! Start year for irrigation
      INTEGER :: YEAR_START   ! Year that the simulation starts (ESD control)
!
      LOGICAL :: NEED_VAPOR   ! Flag whether vapor calculations are needed for aquatic locations
      REAL :: VAPORDILUTE     ! Optional dilution factor (unitless) for vapor concentrations 
!                               calculated from surface water
!
      INTEGER :: NUM_TP       ! Terrestrial plant species counter
      INTEGER :: NUM_QA       ! Aquatic animal species counter
!
      LOGICAL :: STA_USE     ! Flag whether any summary statistics are output
      LOGICAL :: STA_BURDEN  ! Flag whether the body burden summary statistics are output
      LOGICAL :: STA_CONCEN  ! Flag whether the concentration summary statistics are output
      LOGICAL :: STA_EHQ     ! Flag whether the tissue benchmark summary statistics are output
      LOGICAL :: STA_DOSRAD  ! Flag whether the rad dose summary statistics are output
      LOGICAL :: STA_DOSHAZ  ! Flag whether the hazardous dose summary statistics are output
      LOGICAL :: STA_DOSDER  ! Flag whether the dermal dose summary statistics are output
      LOGICAL :: STA_DOSINH  ! Flag whether the inhalation dose summary statistics are output
      LOGICAL :: STA_DOSING  ! Flag whether the ingestion dose summary statistics are output
      LOGICAL :: STA_SUMRAD  ! Flag whether the summed rad dose summary statistics are output
      LOGICAL :: STA_TFOODS  ! Flag whether the statistics on aquatic foods consumed by terrestrial species are output
      LOGICAL :: STA_BMRDOS  ! Flag whether the statistics on rad dose benchmarks are output
!
      LOGICAL :: DET_USE     ! Flag whether any detailed data are output
      LOGICAL :: DET_BURDEN  ! Flag whether the body burden detailed data are output
      LOGICAL :: DET_CONCEN  ! Flag whether the concentration detailed data are output
      LOGICAL :: DET_EHQ     ! Flag whether the tissue benchmark detailed data are output
      LOGICAL :: DET_DOSRAD  ! Flag whether the rad dose detailed data are output
      LOGICAL :: DET_DOSHAZ  ! Flag whether the hazardous dose detailed data are output
      LOGICAL :: DET_DOSDER  ! Flag whether the dermal dose detailed data are output
      LOGICAL :: DET_DOSINH  ! Flag whether the inhalation dose detailed data are output
      LOGICAL :: DET_DOSING  ! Flag whether the ingestion dose detailed data are output
      LOGICAL :: DET_SUMRAD  ! Flag whether the summed rad dose detailed data are output
      LOGICAL :: DET_TFOODS  ! Flag whether the detailed aquatic foods consumed by terrestrial species are output
      LOGICAL :: DET_BMRDOS  ! Flag whether the detailed rad dose benchmarks are output
!
      LOGICAL :: RIP_AQ_PAIRS ! Flag whether riparian and aquatic locations are required in pairs
!                               because the food web crosses over between regions
!
      INTEGER :: NREAL  ! Number of realizations to process
!
      INTEGER :: ECM_NUM_ANA ! Number of analytes in this run of ECEM
      INTEGER :: ECM_NUM_LOC ! Number of locations in this run of ECEM
      INTEGER :: ECM_NUM_TIM ! Number of times in this run of ECEM
      INTEGER :: ECM_NUM_SPC ! Number of species in this run of ECEM
!
!     Units conversion (multiplier) - Dimensions are analytes & media
      REAL, ALLOCATABLE :: CONVERT_UNITS(:,:)
!
!     Units conversion (multiplier) - Dimensions are analytes, media, stage
      CHARACTER(LEN=12), ALLOCATABLE :: CONVERT_LABELS(:,:,:)
!
!     Define a consistent set of labels for output solution labeling
      CHARACTER(LEN=8), PARAMETER :: UNITS_NONE = 'unitless'
!
!     Units for radioactive analytes
      CHARACTER(LEN=10), PARAMETER :: UNITS_RAD_BUR_TER = 'pCi/kg wet'
      CHARACTER(LEN= 7), PARAMETER :: UNITS_RAD_DOS_TER = 'rad/day'
!
      CHARACTER(LEN=10), PARAMETER :: UNITS_RAD_BUR_AQ  = 'pCi/kg dry'
      CHARACTER(LEN= 7), PARAMETER :: UNITS_RAD_DOS_AQ  = 'rad/day'
!
!     Units for stable (not organic) analytes
      CHARACTER(LEN= 9), PARAMETER :: UNITS_STA_BUR_TER = 'ug/kg wet'
      CHARACTER(LEN=13), PARAMETER :: UNITS_STA_DOS_TER = 'ug/kg/day wet'
!
      CHARACTER(LEN= 9), PARAMETER :: UNITS_STA_BUR_AQ  = 'ug/kg dry'
      CHARACTER(LEN=13), PARAMETER :: UNITS_STA_DOS_AQ  = 'ug/kg/day dry'
!
!     Units for organic (not radioactive) analytes
      CHARACTER(LEN= 9), PARAMETER :: UNITS_ORG_BUR_TER = 'ug/kg wet'
      CHARACTER(LEN=13), PARAMETER :: UNITS_ORG_DOS_TER = 'ug/kg/day wet'
!
      CHARACTER(LEN=10), PARAMETER :: UNITS_ORG_BUR_AQ  = 'ug/g lipid'
      CHARACTER(LEN=14), PARAMETER :: UNITS_ORG_DOS_AQ  = 'ug/g lipid/day'
!
!     Units for organic (radioactive) analytes
      CHARACTER(LEN=10), PARAMETER :: UNITS_ORG_RAD_BUR_TER = 'pCi/kg wet'
      CHARACTER(LEN= 7), PARAMETER :: UNITS_ORG_RAD_DOS_TER = 'rad/day'
!
      CHARACTER(LEN=11), PARAMETER :: UNITS_ORG_RAD_BUR_AQ  = 'pCi/g lipid'
      CHARACTER(LEN= 7), PARAMETER :: UNITS_ORG_RAD_DOS_AQ  = 'rad/day'
!
!     Radioactive benchmarks
      REAL :: TP_BMR               ! Terrestrial plant dose limit (rad/day)
      REAL :: TA_BMR               ! Terrestrial animal dose limit (rad/day)
      REAL :: QP_BMR               ! Aquatic plant dose limit (rad/day)
      REAL :: QA_BMR               ! Aquatic animal dose limit (rad/day)
      CHARACTER(LEN=7) :: TP_UNITS ! Units string for terrestrial plant dose limit
      CHARACTER(LEN=7) :: TA_UNITS ! Units string for terrestrial animal dose limit
      CHARACTER(LEN=7) :: QP_UNITS ! Units string for aquatic plant dose limit
      CHARACTER(LEN=7) :: QA_UNITS ! Units string for aquatic animal dose limit
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Control_Mod

MODULE Debug_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to optional debug outputs.
!
!  History:
!
!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 16 Aug 2006 : (SCR-1140) Add FCDA debug
!
      LOGICAL :: BGANAL ! Debug flag for analyte information
      LOGICAL :: BGCONC ! Debug flag for concentration data
      LOGICAL :: BGAIRC ! Debug flag for air concentration calculations
      LOGICAL :: BGCOST ! Debug flag for constant data
      LOGICAL :: BGFCDA ! Debug flag for food file information
      LOGICAL :: BGRADS ! Debug flag for detailed rad doses
      LOGICAL :: BGLOOP ! Debug flag for control looping
      LOGICAL :: BGSPEC ! Debug flag for species definition information
      LOGICAL :: BGSTOC ! Debug flag for stochastic variable generation
      LOGICAL :: BGVERB ! Debug flag for verbose screen outputs
      LOGICAL :: BGORQP ! Debug flag for equations in subroutine OR_QP
      LOGICAL :: BGORTP ! Debug flag for equations in subroutine OR_TP
      LOGICAL :: BGORQA ! Debug flag for equations in subroutine OR_QA
      LOGICAL :: BGORTA ! Debug flag for equations in subroutine OR_TA
      LOGICAL :: BGNRQP ! Debug flag for equations in subroutine NR_QP
      LOGICAL :: BGNRTP ! Debug flag for equations in subroutine NR_TP
      LOGICAL :: BGNRQA ! Debug flag for equations in subroutine NR_QA
      LOGICAL :: BGNRTA ! Debug flag for equations in subroutine NR_TA
      LOGICAL :: BGOSQP ! Debug flag for equations in subroutine OS_QP
      LOGICAL :: BGOSTP ! Debug flag for equations in subroutine OS_TP
      LOGICAL :: BGOSQA ! Debug flag for equations in subroutine OS_QA
      LOGICAL :: BGOSTA ! Debug flag for equations in subroutine OS_TA
      LOGICAL :: BGNSQP ! Debug flag for equations in subroutine NS_QP
      LOGICAL :: BGNSTP ! Debug flag for equations in subroutine NS_TP
      LOGICAL :: BGNSQA ! Debug flag for equations in subroutine NS_QA
      LOGICAL :: BGNSTA ! Debug flag for equations in subroutine NS_TA
      CHARACTER(LEN= 8) :: BGTYPE  ! Debug label for location type
      CHARACTER(LEN=24) :: BGWATER ! Debug label for water source
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Debug_Mod

MODULE ESD_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information from the ESD file that will be used in the ECEM code.
!    In addition, it contains some information defined in ECEM code that depends on location,
!    time, or analyte.
!
!  History:
!
!    Paul W. Eslinger : 19 Apr 2000 : Version 1.0
!    Paul W. Eslinger : 10 May 2002 : Add BMTISS values
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 26 Oct 2004 : Add BCFVEG and TRANSVAL
!    Paul W. Eslinger : 18 Nov 2004 : Add more terrestrial plant variables
!    Paul W. Eslinger :  6 Dec 2004 : Modify new terrestrial plant variables
!    Paul W. Eslinger : 15 Aug 2006 : (SCR-1140) Remove BAF variables
!
      INTEGER, PARAMETER :: ESD_NUM_SOI = 3 ! Number of upland soil types allowed
!
      INTEGER :: ESD_NUM_ANA ! Number of analytes in the ESD file
      INTEGER :: ESD_NUM_LOC ! Number of locations in the ESD file
      INTEGER :: ESD_NUM_TIM ! Number of times in the ESD file
      INTEGER :: ESD_NUM_SPC ! Number of species in the ESD file
      INTEGER :: ESD_NREAL   ! Number of realizations in the ESD file
!
      CHARACTER(LEN=72) :: ESD_TITLE ! Title in the ESD file
!
!     Type definition for ESD (and ECEM) time data
!
      TYPE ESD_TIM_TYPE
        INTEGER :: TIME ! Times where data are stored
        LOGICAL :: COMP ! Flag whether time is to be used in ECEM
      END TYPE ESD_TIM_TYPE
      TYPE (ESD_TIM_TYPE), ALLOCATABLE :: ESD_TIM(:) ! The ESD location variable
!
!     Type definition for ESD (and ECEM) location data
!
      TYPE ESD_LOC_TYPE
        CHARACTER(LEN= 6) :: ID        ! Identification number for a location
        CHARACTER(LEN=72) :: NAME      ! Descriptive name for a location
        CHARACTER(LEN= 8) :: TYPE      ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        LOGICAL :: COMP                ! Flag whether this location is to be used in ECEM
        LOGICAL :: OUTPUT              ! Flag whether aquatics are output at this location
        REAL :: APSD                   ! Aggregate particle size (mm)
        REAL :: COXYGEN                ! Oxygen concentration in surface water (g/liter)
        REAL :: EASTING                ! Easting coordinate in state plane projection (m)
        REAL :: FOC                    ! Fraction soil organic carbon content (unitless)
        REAL :: MSWIND                 ! Mean annual wind speed (m/sec)
        REAL :: MZWIND                 ! Mixing zone wind speed (m/sec)
        REAL :: NECF                   ! Nonerodible elements corr factor (unitless)
        REAL :: NORTHING               ! Northing coordinate in state plane projection (m)
        REAL :: POROSITY               ! Soil porosity (unitless)
        REAL :: RHOS                   ! Soil density (g/cm^3)
        REAL :: SRH                    ! Surface roughness height (m)
        REAL :: TEMP                   ! Temperature (degrees C)
        REAL :: VEGCOV                 ! Fraction of vegetation cover (unitless)
        INTEGER :: SECOND              ! Index for the second choice of concentrations
        CHARACTER(LEN= 6) :: SECOND_ID ! Identification number for secondary location
        LOGICAL :: GWAT                ! GWAT - Media active flag
        LOGICAL :: SEEP                ! SEEP - Media active flag
        LOGICAL :: SWAT                ! SWAT - Media active flag
        LOGICAL :: PWAT                ! PWAT - Media active flag
        LOGICAL :: SEDI                ! SEDI - Media active flag
        LOGICAL :: SORP                ! SORP - Media active flag
        LOGICAL :: SODR                ! SODR - Media active flag
        LOGICAL :: SOGW                ! SOGW - Media active flag
        LOGICAL :: SOSW                ! SOSW - Media active flag
        LOGICAL :: AIRC                ! AIRC - Media active flag
        LOGICAL :: AIRD                ! AIRD - Media active flag
      END TYPE ESD_LOC_TYPE
      TYPE (ESD_LOC_TYPE), ALLOCATABLE :: ESD_LOC(:) ! The ESD location variable
!
!     Type definition for ESD (and ECEM) analyte data
!
      TYPE ESD_ANA_TYPE ! Type definition for ESD (and ECEM) analyte data
        CHARACTER(LEN= 6) :: ID      ! Identification tag for an analyte
        CHARACTER(LEN= 6) :: ELEM_ID ! Element identification tag for an analyte
        CHARACTER(LEN=72) :: NAME    ! Descriptive name for an analyte
        CHARACTER(LEN= 2) :: TYPE    ! Analyte type
!                                        OS = organic, stable      OR = organic, radioactive,
!                                        NS = innorganic, stable   NR = inorganic, radioactive,
        LOGICAL :: COMP              ! Flag whether this analyte is to be used in ECEM
        LOGICAL :: THROTTLE          ! Flag whether this analyte will be deactivated after sampling occurs
        LOGICAL :: OUTPUT            ! Flag whether analyte results are output
        REAL :: HENRY                ! Henry's law coefficient (Pa-m^3/mole)
        REAL :: MOLDIFF              ! Molecular diffusivity (cm^2/sec)
        REAL :: DFIMM                ! Water immersion dose factor (mrad m^3/uCi/year)
        REAL :: DFSED                ! Sediment dose factor (Sv m^3/sec Bq)
        REAL :: GAMMA                ! Gamma energy (MeV/disintigration)
        REAL, POINTER :: KOW(:)      ! Log of the Octonal-water partition coefficient (unitless) by realization
        REAL :: PARTICLE             ! Fraction of air far-field concentration that is particulate
      END TYPE ESD_ANA_TYPE
      TYPE (ESD_ANA_TYPE), ALLOCATABLE :: ESD_ANA(:) ! The ESD analyte variable
!
!     Type definition for ESD (and ECEM) species data
!
      TYPE ESD_SPC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Species ID
        CHARACTER(LEN=48) :: NAME ! Species name
        INTEGER :: ORDER          ! Order in which to compute species
        CHARACTER(LEN= 2) :: TYPE ! Species type
!                                     QP = aquatic plant,  TP = terrestrial plant
!                                     QA = aquatic animal, TA = terrestrial, animal
        CHARACTER(LEN= 8) :: HABITAT ! Species habitat (aquatic, riparian, or upland)
        LOGICAL :: COMP      ! Flag whether a species results are computed
        LOGICAL :: OUTPUT    ! Flag whether a species results are output
        LOGICAL :: INTERNAL  ! Flag whether plant internal burdens should be passed as human foods
!                               .TRUE. = Yes, .FALSE. = No, use entire plant (Default = .FALSE.)
        LOGICAL :: EMERGENT  ! Flag whether a species is an emergent terrestrial plant
        LOGICAL :: TRANS     ! Flag whether an animal species uses a secondary transfer factor
        REAL :: AE           ! Assimilation efficiency (unitless)
        REAL :: OCAR         ! Organic carbon assimilation rate (g org carbon assim/g ingested)
        REAL :: AWD          ! Wet weight to dry weight ratio (g wet/g dry)
        REAL :: DIFFHT       ! Diffusion height (m)
        REAL :: ETWATER      ! Exposure time to water (hr/day)
        REAL :: FDW          ! Conversion rate dry weight to wet weight (kg dry/kg wet)
        REAL :: FLIPID       ! Fraction lipid (g lipid/g wet)
        REAL :: FMR          ! Metabolic rate (kcal/day)
        REAL :: FOC          ! Fraction organic carbon (g org carbon/g dry weight)
        REAL :: FPA          ! Volume fraction of plant tissue that is air (unitless)
        REAL :: FPL          ! Volume fraction of plant tissue that is lipid (unitless)
        REAL :: FPW          ! Volume fraction of plant tissue that is water (unitless)
        REAL :: FW           ! Water weight fraction of plant tissue (unitless)
        REAL :: FWATER       ! Fraction of body exposure to water (unitless)
        REAL :: GE           ! Gross energy (kcal/kg wet weight)
        REAL :: INHRATE      ! Resting 1inhalation rate (m^3/day)
        REAL :: LOSSRATE     ! Calculated loss rate (single realization)
        REAL :: PCS          ! Fraction of SA in contact with soil (1/day)
        REAL :: PCW          ! Fraction of SA available to water (unitless)
        REAL :: PSI          ! Seasonality factor (unitless)
        REAL :: RADIUS       ! Radius for calculating effective energy (cm)
        REAL :: RHOP         ! Plant tissue density (kg/m^3)
        REAL :: SA           ! Surface area (cm^2)
        REAL :: SADHER       ! Skin adherance factor (mg/cm**2)
        REAL :: SOILING      ! Soil ingestion rate (kg soil ing/kg dry weight)
        REAL :: SEDING       ! Sediment ingestion rate (kg soil ing/kg dry weight)
        REAL :: THETA        ! Area use factor (unitless)
        REAL :: WEIGHT       ! Body weight (kg wet weight)
        REAL :: GROWRATE     ! Species growth rate
        REAL :: RESPRATE     ! Species respiration rate
        REAL :: WBMASS       ! Wet body mass (grams)
        REAL :: WATERING     ! Water ingestion rate (L/day)
!
!       The following group of variables is also analyte dependent
        REAL, POINTER :: EFFENG(:) ! Effective Energy by species
!
!       The following group of variables is also realization dependent
        REAL, POINTER :: CHEM(:)       ! Chemical transfer efficiency (calculated from KOW)
        REAL, POINTER :: NIRTOT(:)     ! Normalized intake rate
        REAL, POINTER :: BPORE(:)      ! Relative exposure time to pore water (unitless)
        REAL, POINTER :: FABOVE(:)     ! Relative exposure time to above ground or above sediment radiation (unitless)
        REAL, POINTER :: KPS1(:)       ! Plant-soil partition coefficient (kg soil/kg plant wet weight)
        REAL, POINTER :: KPA2(:)       ! Plant-air partition coefficient (m^3/kg wet weight)
        REAL, POINTER :: BODYBURDEN(:) ! Body burden vector by realization (each species at a time)
        REAL, POINTER :: BIOMASS(:)    ! Standing biomass for terrestrial plants (kg/m^2 wet weight)
!
!       The following group of variables is also analyte and realization dependent
        REAL, POINTER :: KLIWET(:,:)    ! Transfer from wet leaf surface to plant internal for terrestrial plants (unitless)
        REAL, POINTER :: KLIDRY(:,:)    ! Transfer from dry leaf surface to plant internal for terrestrial plants (unitless)
        REAL, POINTER :: IFRACDRY(:,:)  ! Interception fraction for dry deposition on terrestrial plants (unitless)
        REAL, POINTER :: IFRACWET(:,:)  ! Interception fraction for wet deposition on terrestrial plants (unitless)
        REAL, POINTER :: LWEATHDRY(:,:) ! Weathering rate for dry deposition on terrestrial plants (1/yr)
        REAL, POINTER :: LWEATHWET(:,:) ! Weathering rate for wet deposition on terrestrial plants (1/yr)
        REAL, POINTER :: ALPHAIJ(:,:)   ! Chemical assimilation efficiency (g assimilated/g ingested)
        REAL, POINTER :: BCF(:,:)       ! Bioaccumulation factor (L/kg)
        REAL, POINTER :: BMTISS(:,:)    ! Tissue benchmark for nonrad analytes (ug/kg[wet=aquatic or dry=terrestrial] or ug/g lipid)
        REAL, POINTER :: DEPRATE(:,:)   ! Depuration rate (1/day)
        REAL, POINTER :: METBLOSS(:,:)  ! Metabolism loss rate (1/day)
        REAL, POINTER :: ALPHAING(:,:)  ! Ingestion absorption factor (unitless)
        REAL, POINTER :: ALPHAPAR(:,:)  ! Inhalation particulate absorption factor (unitless)
        REAL, POINTER :: ALPHAVAP(:,:)  ! Inhalation vapor absorption factor (unitless)
        REAL, POINTER :: ALPHADW(:,:)   ! Dermal absorption for water (cm/hr)
        REAL, POINTER :: ALPHADS(:,:)   ! Dermal permeability constant for soil (%)
        REAL, POINTER :: BCFVEG(:,:)    ! Bioconcentration factor, terrestrial plants (unitless)
        REAL, POINTER :: TRANSVAL(:,:)  ! Secondary transfer factor, terrestrial animals (unitless)
!
      END TYPE ESD_SPC_TYPE
      TYPE(ESD_SPC_TYPE), ALLOCATABLE :: ESD_SPC(:) ! Variable structure for species information
!
      CHARACTER(LEN=20), ALLOCATABLE :: KD_UNITS(:) ! Distribution coefficient units by analyte
      CHARACTER(LEN=20), ALLOCATABLE :: KD_MAP(:)   ! Distribution coefficient map by analyte (length of VLABEL)
      REAL, ALLOCATABLE :: KD(:,:)                  ! Distribution coefficient by analyte and realization
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE ESD_Mod

MODULE Files_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains file specific variable information.
!
!  History:
!
!    Paul W. Eslinger : 19 Apr 2000 : Version 1.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 26 Oct 2004 : Add IBCF and ITRN
!
      INTEGER, PARAMETER :: MAXFN=200 ! Length of file names
!
      INTEGER :: IESD               ! Unit number for the ESD keyword file
      CHARACTER(LEN=MAXFN) :: FNESD ! Name of the input ESD keyword file
!
      INTEGER :: IKEY               ! Unit number for the keyword file
      CHARACTER(LEN=MAXFN) :: FNKEY ! Name of the input keyword file
!
      INTEGER :: IRPT               ! Unit number for the report file
      CHARACTER(LEN=MAXFN) :: FNRPT ! Name of the output report file
!
      INTEGER :: ITHS               ! Unit number for the output threshold statistics
      CHARACTER(LEN=MAXFN) :: FNTHS ! Name of the output threshold statistics file
!
      INTEGER :: ISUM               ! Unit number for the output summary statistics
      CHARACTER(LEN=MAXFN) :: FNSUM ! Name of the output summary statistics file
!
      INTEGER :: IDET               ! Unit number for the output detailed values
      CHARACTER(LEN=MAXFN) :: FNDET ! Name of the output detailed values
!
      INTEGER :: IVLU               ! Unit number for the output generated statistical values
      CHARACTER(LEN=MAXFN) :: FNVAL ! Name of the output generated statistical values
!
      INTEGER :: IKDS               ! Unit number for the KdSoil data file
      CHARACTER(LEN=MAXFN) :: FNKDS ! File name for KdSoil data
!
      CHARACTER(LEN=MAXFN) :: FNMAP ! Name of the concentration record number map file
!
      INTEGER, ALLOCATABLE :: ICON(:)               ! Unit number for the ECDA concentration files
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FNCON(:) ! Name of input ECDA concentration files
!
      CHARACTER(LEN=MAXFN) :: FOODPATH ! Path for location of food files
      CHARACTER(LEN=MAXFN) :: FOODMAP  ! Name of the food map file (local name only)
!
      INTEGER, ALLOCATABLE :: IFOD(:,:)               ! Unit number for the food concentration files
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FNFOD(:,:) ! Names of food concentration files
!
      INTEGER :: IBCF  ! Unit number for BCFVEG scratch file
      INTEGER :: ITRN  ! Unit number for TRANSFER scratch file
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Files_Mod

MODULE Iden_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger : 19 Apr 2000 : Version 1.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger :  6 Dec 2004 : Change system date separator characters to /
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
      CHARACTER(LEN=10) :: SYSDAT ! System date in the format mm/dd/yyyy
      CHARACTER(LEN=12) :: SYSTIM ! System time in the format hh:mm:ss
      CHARACTER(LEN=16) :: USRNAM ! User name
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Iden_Mod

MODULE Kdsoil_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains information related to the KD (soil-water distribution coefficient)
!    data file
!
!  History:
!    Paul W. Eslinger : 10 Aug 2006 : Adapted from the Soil code
!
      CHARACTER(LEN=  8) :: KD_TYPE   ! The character string KDSOIL
      CHARACTER(LEN=200) :: KD_PTITLE ! Program modification date from KD generator
      CHARACTER(LEN= 10) :: KD_PRGNAM ! Program name from KD generator
      CHARACTER(LEN=  8) :: KD_PRGVER ! Program version number from KD generator
      CHARACTER(LEN= 12) :: KD_PRGDAT ! Program date from KD generator
      CHARACTER(LEN= 14) :: KD_CRUNID ! Run identification number from KD generator
      CHARACTER(LEN= 16) :: KD_USRNAM ! User name from KD generator
      INTEGER :: KD_NUM               ! Number of KD stochastic variables
      INTEGER :: KD_NITR              ! Number of KD stochastic iterations
      CHARACTER(LEN=20) :: KD_ID      ! KD stochastic ID
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Kdsoil_Mod

MODULE Media_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History:
!
!    Paul W. Eslinger : 17 Dec 1997 : Version 1.0
!    Paul W. Eslinger : 17 Apr 2000 : Version 2.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!
! *** Variable structure containing environmental concentrations
!
      TYPE MEDIA_TYPE
        REAL :: AIRC   ! Air concentration
        REAL :: AIRD   ! Air deposition
        REAL :: PWAT   ! Pore water concentration
        REAL :: SEDI   ! Sediment concentration
        REAL :: SOIL   ! (Generic) Soil concentration
        REAL :: SEEP   ! Seep (spring) water concentration
        REAL :: SWAT   ! Surface water concentration
        REAL :: VAPOR  ! Vapor concentration
        REAL :: PARTIC ! Particulate concentration
        REAL :: SORP   ! Riparial soil concentration
        REAL :: SODR   ! Dry (nonirrigated) soil concentration
        REAL :: SOGW   ! Goundwater irrigated soil concentration
        REAL :: SOSW   ! Surface water irrigated soil concentration
        REAL :: GWAT   ! Groundwater concentration
        REAL :: WATER  ! (Generic) water concentration for animals
      END TYPE MEDIA_TYPE
!
      TYPE(MEDIA_TYPE), ALLOCATABLE :: EC(:)
!
      REAL, ALLOCATABLE :: CVEC(:) ! Concentration work vector
!
      CHARACTER(LEN=4) :: SOIL_ID ! Soil type for write statements
      INTEGER :: SOIL_IDX         ! Soil dependent index for write statements
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Media_Mod

MODULE Param_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains constants needed throughout the code
!
!  History:
!
!    Paul W. Eslinger : 24 Mar 2000 : Version 2.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger :  4 Jun 2004 : SCR-1079 - Modify dose parameters
!    Paul W. Eslinger : 27 Jan 2006 : Change KASCONV
!
! *** Conversion Factors
!
      REAL, PARAMETER :: CFDERMS = 1.0E-06        ! Unit conversion for dermal soil dose (1E-6 kg/mg)
      REAL, PARAMETER :: CFDERMW = 1.0E-03        ! Unit conversion for dermal water dose (1 L/1,000 cm3)
      REAL, PARAMETER :: ENGCONV = 5.12E-08       ! Unit conversion for effective energy
!
      REAL, PARAMETER :: TIMMFAC = 1.140771E-10   ! Unit conversion for terrestrial immersion
!           meaning: (0.001 rad/mrad x 1E-6 `Ci/pCi x 1000 L/m3) / (24 hr/d x 365.25 d/yr)
!
      REAL, PARAMETER :: QIMMFAC = 2.737851E-9    ! Unit conversion for aquatic immersion
!           meaning: rad/1000 mrad x m^3/1000 L x year/365.25 day
!
      REAL, PARAMETER :: KASCONV = 1.01325E+5      ! Conversion factor for KAS (from standard atmosphere to Pascal)
!           Reference: NIST Guide to SI Units, Appendix B. Conversion factors
!           http://physics.nist.gov/Pubs/SP811/appenB.html
!
      REAL, PARAMETER :: CFABOVE = 5.12E+8  ! Conversion factor for above ground sediment rad dose
!           meaning : 1.6E3 kg/m3 x 86,400 sec/day x 3.7E-2 Bq/pCi x 100 rem/Sv x 1 rad/rem
!
      REAL, PARAMETER :: CFBELOW = 5.12E-8 ! Conversion factor for below ground sediment rad dose
!           meaning : 1E-12 Ci/pCi x 3.7E10 disinteg/sec/Ci * 86,400 sec/day x 1.602E-11 kg-rad/Mev
!
      REAL, PARAMETER :: CFSWD = 0.75 ! Conversion factor for sediment wet weight to dry weight
!
      REAL, PARAMETER :: CFMCF = 1.05 ! Medium correction factor for sediment exposure
!
      REAL, PARAMETER :: CFDSF = 0.5  ! Directional source factor for one-sided exposure
!
      REAL, PARAMETER :: CFDRF = 0.7  ! Dose rate reduction factor for ground roughness
!
! *** Other constants
!
      REAL, PARAMETER :: PI = 3.14159265 ! Mathematical constant PI
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Param_Mod

MODULE Radius_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This contains radius-specific library information for radioactive analytes.  The
!    first line of the library should contain the radius distance.  The other lines
!    should contain the energy values for each distance by analyte.
!
!  History:
!
!    Paul W. Eslinger : 24 Mar 2000 : Version 1.0
!
      INTEGER NHED_RLIB ! Number of header (radii length) lines in the radius arrary
      INTEGER NANA_RLIB ! Number of analytes in the radius library
      INTEGER NRAD_RLIB ! Number of radii in the radius library
!
      TYPE RLIB_RAD
        CHARACTER(LEN=6) :: ID      ! Library radius ID's
        REAL, POINTER :: RLIBVAL(:) ! Library analyte variable values (length NRAD_RLIB)
      END TYPE RLIB_RAD
      TYPE(RLIB_RAD), ALLOCATABLE :: RLIBANA(:) ! The library radius structure
!                                                 (dimension (NANA_RLIB+NHED_RLIB))
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Radius_Mod

MODULE Results_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables for work space and results
!
!  History:
!
!    Paul W. Eslinger : 24 Mar 2000 : Version 2.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Rename TBM to EHQ
!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Upgrade comments
!
!     Work vectors depending on realizations
      REAL, ALLOCATABLE :: CWORK(:)  ! Work vector for concentrations (body burdens)
      REAL, ALLOCATABLE :: FWORK(:)  ! Work vector for concentrations (food items)
      REAL, ALLOCATABLE :: TWORK(:)  ! Work vector
      REAL, ALLOCATABLE :: RDWORK(:) ! Work vector for rad dose for one analyte/species combination
!
!     Concentration arrays depending on realizations
      REAL, ALLOCATABLE :: EHQ(:) ! Environmental hazard quotient (work vector)
!
!     Concentration arrays depending on species and realizations
      REAL, ALLOCATABLE :: CPAT(:,:) ! Total terrestrial plant body burden (as consumed)
      REAL, ALLOCATABLE :: CPAI(:,:) ! Terrestrial internal plant body burden
      REAL, ALLOCATABLE :: CDER(:,:) ! Total terrestrial animal dermal body burden
      REAL, ALLOCATABLE :: CINH(:,:) ! Total terrestrial animal inhalation body burden
      REAL, ALLOCATABLE :: CING(:,:) ! Total terrestrial animal ingestion body burden
!
!     Dose arrays depending on realizations
      REAL, ALLOCATABLE :: DDER(:) ! Total terrestrial animal dermal dose
      REAL, ALLOCATABLE :: DINH(:) ! Total terrestrial animal inhalation dose
      REAL, ALLOCATABLE :: DING(:) ! Total terrestrial animal ingestion dose
      REAL, ALLOCATABLE :: DHAZ(:) ! Total terrestrial animal dose
!
!     Dose arrays depending on species and realizations
      REAL, ALLOCATABLE :: RADSUM_AQ(:,:)   ! Sum of rad dose (over analytes) for aquatic species
      REAL, ALLOCATABLE :: RADSUM_RP(:,:)   ! Sum of rad dose (over analytes) for riparian species
      REAL, ALLOCATABLE :: RADSUM_UP(:,:,:) ! Sum of rad dose (over analytes) for upland species
!                                             by soil type
!
!     Statistics used (and reused) in several locations
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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Results_Mod

MODULE Seeds_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History:
!
!    Paul W. Eslinger : 22 Oct 1997 : Version 1.0
!
     REAL(KIND=8) :: SDSTOC ! Random seed for all generated stochastic variables
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Seeds_Mod

MODULE Species_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History:
!    Paul W. Eslinger :  2 Apr 1998 : Version 1.0
!    Paul W. Eslinger : 28 Jul 2004 : (SCR-1063) Remove TUPTAKE
!    Paul W. Eslinger : 21 Jan 2005 : Add food consumption calulations
!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Standardize species use flags
!
!  Variable structure containing species information
!
      INTEGER, ALLOCATABLE :: ISPCTMP(:) ! Temporary integer vector
!
      LOGICAL :: NEED_TP ! Flag for calculating terrestrial plants
      LOGICAL :: NEED_TA ! Flag for calculating terrestrial animals
      LOGICAL :: NEED_QA ! Flag for calculating aquatic animals
      LOGICAL :: NEED_QP ! Flag for calculating aquatic plants
!
      REAL, ALLOCATABLE :: PREDATE(:,:) ! Predation matrix for all species (size based on the
!                                         number of species in the food web)
!
      INTEGER :: NUM_CHEM_EFF               ! Number of entries in the table for chemical
!                                             assimilation efficiencies
      INTEGER, PARAMETER :: MAXCHM = 15     ! Maximum number of entries in the chemical
!                                             assimilation lookup table
      REAL, DIMENSION(MAXCHM,2) :: CHEM_EFF ! Table of chemical efficiencies
!
      REAL :: GROWTH_DELTA  ! Growth rate multiplier
      REAL :: GROWTH_BETA   ! Growth rate exponent
      REAL :: RESPIRE_PHI   ! Respiration rate multiplier
      REAL :: RESPIRE_GAMMA ! Respiration rate exponent
!
!     Block of variables for terrestrial plants
      REAL :: AREA    ! Area of contamination (m^2) (not the area on the ESD location keyword)
      REAL :: COWHDFX ! Cowherd function (unitless)
      REAL :: EXPOSI  ! Exposure interval (seconds)
      REAL, PARAMETER :: GASCON = 8.314 ! Universal gas constant (Pa-m^3/mol-K)
      REAL :: LENGTH  ! Length of side of contaminated area (meters)
      REAL :: RFRAC   ! Respirable fraction (g/m^2-hour)
!      PW Eslinger - SCR-1063 change TUPTAKE to FW
!      REAL :: TUPTAKE ! Fraction root uptake for tritium (unitless)
!
      REAL, ALLOCATABLE :: FOOD(:) ! Temporary vector for food intake
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Species_Mod

MODULE Threshold_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains information specific to thresholds
!
!  History:
!    Paul W. Eslinger :  4 Mar 1999 : Version 1.1
!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Change to using EHQ label
!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Eliminate EHQ
!
      PARAMETER (MAXTYP=6)
      CHARACTER(LEN=6), DIMENSION(MAXTYP) :: THRESHLBL
!       Index 1 = BURDEN, Index 2 = DOSDER, Index 3 = DOSING, Index 4 = DOSINH
!       Index 5 = DOSRAD, Index 6 = SUMRAD
!
      TYPE THRESH_TYPE ! Type definition for thresholds
        LOGICAL, POINTER :: EXIST1(:,:,:) ! Flag whether threshold 1 results are output
        LOGICAL, POINTER :: EXIST2(:,:,:) ! Flag whether threshold 2 results are output
        REAL, POINTER :: LIMIT1(:,:,:)    ! Threshold limit 1
        REAL, POINTER :: LIMIT2(:,:,:)    ! Threshold limit 2
        LOGICAL, POINTER :: SREXIST1(:)   ! Flag whether threshold 1 SUMRAD results are output
        LOGICAL, POINTER :: SREXIST2(:)   ! Flag whether threshold 2 SUMRAD results are output
        REAL, POINTER :: SRLIMIT1(:)      ! SUMRAD threshold limit 1
        REAL, POINTER :: SRLIMIT2(:)      ! SUMRAD threshold limit 2
      END TYPE THRESH_TYPE
!
      TYPE (THRESH_TYPE) :: THRESHOLD ! The threshold variable structure
!
      LOGICAL :: THS_USE ! Flag that at least one threshold is desired
!
      REAL :: TL1     ! Threshold limit 1 value
      REAL :: PR1     ! Probability of exceeding threshold 1
      LOGICAL :: EX1  ! Flag whether threshold 1 exists
!
      REAL :: TL2     ! Threshold limit 2 value
      REAL :: PR2     ! Probability of exceeding threshold 2
      LOGICAL :: EX2  ! Flag whether threshold 2 exists
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Threshold_Mod

      PROGRAM ECEM
!!********************************************************************************************************
!!
!!                       ECEM - Ecological Contaminant Exposure Model
!!                     Toolkit for Integrated Impact Assessments (TIIA)
!!                     Battelle Memorial Institute, Richland, Washington
!!
!!********************************************************************************************************
!!
!!                      Copyright c Battelle Memorial Institute, 2007.
!!                                 All Rights Reserved.
!!
!!********************************************************************************************************
!!
!!  ECEM is the top level routine for the ecological impacts portion of the TIIA, Vesion 1.  This code
!!  performs a stochastic analysis of the risk to ecological species from contamination purportedly
!!  released into the envirnment.  In addition, it can calulate the concentrations in food products
!!  used by HUMAN (human risk module) for TIIA Rev. 1.
!!
!! Reference:
!!
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!********************************************************************************************************
!!
!!  History:
!!
!!    This version of the ECEM code draws heavily from the version developed as a follow-on for
!!    the Columbia River Comprehensive Impact Assessment project.  The ECEM original version
!!    development team was Paul W. Eslinger, Terri B. Miley, Amoret Bunn and Charlie Brandt.
!!
!!    Paul W. Eslinger :  2 Apr 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 1999 : Version 1.1
!!    Paul W. Eslinger : 17 Mar 1999 : Version 1.2
!!    Paul W. Eslinger : 10 May 2000 : Version 2.0 : SAC Modifications
!!    Paul W. Eslinger : 25 Jun 2001 : Version 2.0 : Add error trap for negative concentrations
!!    Paul W. Eslinger : 17 Aug 2001 : Add BGVERB and change SACVIEW header
!!    Paul W. Eslinger : 25 Apr 2002 : Minor bug fixes and comment changes
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS values, Replace equivalent water concentrations
!!    Paul W. Eslinger :  3 Jun 2002 : Move sediment out of the predation for aquatic animals.
!!                                     Modify sediment ingestion equations and debug writes.
!!    Paul W. Eslinger : 13 Mar 2003 : SAC Rev. 1.  Add food calculations for HUMAN and TCERM
!!                                     Access soil concentrations by irrigation type
!!                                     Add far-field air concentration effects
!!                                     Change predation matrix inputs to consume inputs
!!                                     Allow river-only and upland-only food webs
!!    Paul W. Eslinger : 17 Jun 2003 : SCR 1005: Data changes on the LOCATION keywords.  Change
!!                                     reading TYPE data (analyte and species) to upper case
!!                                     convention. Renumber errors.
!!    Paul W. Eslinger :  8 Sep 2003 : SCR 1025: Add error check for SURFACE data
!!    Paul W. Eslinger : 21 Oct 2003 : Move reads on ECEM keywords, add TELLTIME
!!    Paul W. Eslinger : 20 Jan 2004 : SCR-1046: Move reads on ECEM keywords, change BCFVEG and KOW
!!                                     to element specific rather than isotope specific data entry.
!!    Paul W. Eslinger :  3 May 2004 : SCR-1055: Don't require BCFVEG for tritium
!!                                     because TUPTAKE is used instead.
!!    Paul W. Eslinger : 28 Jul 2004 : SCR-1063: Replace TUPTAKE with FW in subroutines NR_TP and NS_TP
!!                                     and revise wet basis in NR_QP and NS_QP for tritium.
!!    Paul W. Eslinger : 10 Aug 2004 : SCR-1065: Revise SACVIEW header file
!!    Paul W. Eslinger : 26 Oct 2004 : SCR-1070: Change BCFVEG to be plant and analyte dependent
!!                                     Add RETURN on an error trap in KEY_ECEM_2
!!                                     Add terrestrial animal secondary transfer factor
!!    Paul W. Eslinger : 18 Nov 2004 : SCR-1070: Add more terrestrial plant variables
!!    Paul W. Eslinger :  6 Dec 2004 : Change dependencies for IFRACDRY, IFRACWET,
!!                                     LWEATHDRY, and LWEATHWET.  Change length of
!!                                     VLABEL and VNAME fom 20 to 24.
!!    Paul W. Eslinger : 13 Jan 2005 : SCR-1070: Change KOW dependency to only terrestrial animals, add
!!                                     info message on skipped species, make RADIUS not required when no
!!                                     radioactive analytes are used. Fix riparian/aquatic index pairing
!!    Paul W. Eslinger : 20 Jan 2005 : Adjust leaf deposition totals for terrestrial plants
!!    Paul W. Eslinger : 21 Jan 2005 : Add food consumption calulations
!!    Paul W. Eslinger :  2 Feb 2005 : Change species reporting for debug SPECIES
!!    Paul W. Eslinger :  9 Feb 2005 : Adjust leaf deposition totals again
!!    Paul W. Eslinger : 11 May 2005 : Change growing season checks
!!    Paul W. Eslinger : 20 May 2005 : SCR-1079: Change KOW to the log of the octonal/water
!!                                     partition coefficient, change FORALL logic
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079: Modify external dose equations
!!    Paul W. Eslinger : 15 Jun 2005 : SCR-1080: Change HUMAN food options
!!    Paul W. Eslinger : 29 Jun 2005 : Rename SUMBAF to SEDBAF to enhance clarity
!!    Paul W. Eslinger : 30 Jun 2005 : Error checking on energy intake for terrestrial animal subroutines
!!    Paul W. Eslinger :  1 Jul 2005 : Revise Kd equation in COMPUTE_VAPOR
!!    Paul W. Eslinger :  6 Jul 2005 : Remove BCFVEG for organics
!!    Paul W. Eslinger :  7 Jul 2005 : Revise Kd equation in COMPUTE_VAPOR again
!!    Paul W. Eslinger : 24 Aug 2005 : SCR-1081: Implement THROTTLE keyword
!!    Paul W. Eslinger :  2 Sep 2005 : Change debug outputs and enhance error checking
!!    Paul W. Eslinger :  6 Jan 2006 : Change RDBLK routines (SCR-1100), allow longer input keyword lines
!!    Paul W. Eslinger : 27 Jan 2006 : SCR-1103: Correct KPA1 and KPS2 equations for KOW being in log
!!                                     units.  Remove H3 logic in NS_TP - H3 is not a stable analyte.
!!                                     Correct conversion factor KASCONV (standard atmosphere to Pascal)
!!    Paul W. Eslinger :  1 Feb 2006 : SCR-1103: Change calls to WRITE_DETAILS in subroutine CON_DATA
!!                                     to use concentration vector instead of a structure element
!!                                     (different performance under Linux as compared to Windows)
!!    Paul W. Eslinger : 18 Jul 2006 : SCR-1137: Change assignment of water concentrations for
!!                                     upland species based on the time of onset of irrigation
!!    Paul W. Eslinger :  6 Sep 2006 : SCR-1140: Read Kd's from a data library
!!                                     Reformat debug outputs and make body burden outputs consistent
!!                                     Restructure aquatic animal equations to simplify flow
!!    Paul W. Eslinger : 13 Sep 2006 : SCR-1141: Update statistics routines
!!    Paul W. Eslinger :  7 Sep 2006 : SCR-1140: Fix SOILING and SEDING error checks
!!    Paul W. Eslinger : 10 Nov 2006 : SCR-1149: Make consistent set of output labels
!!                                     Implement EHQ and BMRDOS upgrades
!!    Paul W. Eslinger :  6 Jun 2007 : Revise for TIAA
!!    Paul W. Eslinger : 15 Aug 2007 : Add NEED variables for rads, chemicals and irrigation
!!    Paul W. Eslinger :  9 Jul 2012 : (SCR-0001) Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger : 13 Nov 2013 : Change output formats to support 2500 realizations
!!    Paul W. Eslinger : 22 Jan 2014 : Upgrade error trapping
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0002) Add inhalation term to the body burden equation for 
!!                                     aquatic animals
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!    Paul W. Eslinger : 12 Jun 2014 : (SCR-0002) Fix some logic with air inhalation term
!!
!!********************************************************************************************************
!!
!!  Notes:
!!
!!    1. This code applies one definition of ecological parameters in a single run. The code applies
!!       the same definition to multiple locations and handles multiple contaminants.
!!
!!    2. This code provides stochastic results.  It also provides deterministic results in that a run
!!       of one realization is allowed.
!!
!!    3. Error checking on the stochastic variables is done in the routine used to generate the values.
!!
!!  Language:
!!
!!    This program was written in Fortran 95 (free source form)
!!
!!  Fortran Langage Extensions:
!!
!!    No language extensions are used.
!!    The Lahey service routines NARGS and GETARG are used to get command line arguments.
!!
!!********************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Species_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Debug_Mod
      USE Threshold_Mod, ONLY: THS_USE
      USE Esd_Mod
      USE Results_Mod
      USE Files_Mod
      USE Stats_Mod, ONLY: INDSTO, INDTBL, BG_STOC_VALU
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Declare local variables
      CHARACTER(LEN=4) :: CALLER = 'ECEM' ! Name of this program
      CHARACTER(LEN=7) :: CYEAR           ! Character year
!
      INTEGER :: IERR ! Integer error flag
      INTEGER :: IANA ! Analyte index
      INTEGER :: ILOC ! Location index
      INTEGER :: ITIM ! Time index
      INTEGER :: IECD ! Unit number for ECDA map file
      INTEGER :: ISPC ! Species index
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
!-----------------------------------------------------------------------
! *** Program, run identification, and initialization
!-----------------------------------------------------------------------
!
      CALL IDEN_SET( )
      CALL INIT( )
!
!-----------------------------------------------------------------------
! *** Keyword file and problem set-up operations
!-----------------------------------------------------------------------
!
! *** Get the ECEM keyword file name and open the file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the keyword file'
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
! *** Print the opening banner page
      CALL BANNER_1(  )
!
! *** Read the first pass of ECEM keywords to get the report file
!     open, user name defined, and array dimension information collected
      CALL TELLTIME( 'Reading ECEM keywords - Pass #1', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ECEM_1( IERR )
      IF( IERR .EQ. 666 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Check the identification keywords
      CALL CHECK_IDEN( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Finish writing the banner page to the report file
      CALL BANNER_2( )
!
! *** Open the environmental settings definition (ESD) keyword file
      CALL OPEN_ESD( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Read the first pass of environmental settings keywords to
!     collect array dimension information
      CALL TELLTIME( 'Reading ESD keywords  - Pass #1', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_1( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Check the problem size definition ESD keywords
      CALL CHECK_ESD( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Set up memory for stochastic variables and initialize
!     other stochastic information
      IF( BGVERB ) CALL TELLTIME( 'Allocating Memory - ESD', 'SCREEN', .FALSE., IRPT )
      CALL STOCH_MEMORY( INDSTO, INDTBL, NREAL, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
      CALL INIT_STOCH( )
!
      IF( BGVERB ) CALL TELLTIME( 'Allocating Memory - Stochastics', 'SCREEN', .FALSE., IRPT )
      CALL ESD_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Initialize environmental settings data and the "use" information
      CALL ESD_INIT(  )
!
! *** Read the second pass of environmental keywords
!     Save time, analyte, location information for ecological impacts
!     Save concentration file names
      CALL TELLTIME( 'Reading ESD keywords  - Pass #2', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Set up the dynamic memory for all other arrays
      IF( BGVERB ) CALL TELLTIME( 'Allocating Memory - ECEM', 'SCREEN', .FALSE., IRPT )
      CALL ECEM_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Initialize ECEM-type things
      CALL ECEM_INIT(  )
!
! *** Read the keywords
      CALL TELLTIME( 'Reading ECEM keywords - Pass #2', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ECEM_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Error checks on the input ECEM keyword data
      CALL CHECK_ECEM( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Echo the first part of the problem setup to the report file
      CALL ECHO1( )
!
! *** Check deterministic species parameters for validity
!     Check all species before exiting with an error
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT.ESD_SPC(ISPC)%COMP ) CYCLE
        CALL CHECK_SPECIES( ISPC, IERR )
      END DO
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Set the analyte-dependent radionuclide energies only if they are needed
!     Can't decide if they are needed until KEY_ECEM_2 and CHECK_ECEM are finished
      IF( USE_RADS ) THEN
        IF( BGVERB ) CALL TELLTIME( 'Allocating memory for the RADIUS library', 'SCREEN', .FALSE., IRPT )
        CALL RADMEMORY( IERR )
        IF( IERR .NE. 0 ) GO TO 999
        CALL TELLTIME( 'Reading ESD keywords  - Pass #3', 'SCREEN', .FALSE., IRPT )
        CALL KEY_ESD_3( IERR )
        IF( IERR .NE. 0 ) GO TO 999
        IF( BGVERB ) CALL TELLTIME( 'Extracting data from RADIUS library', 'SCREEN', .FALSE., IRPT )
        CALL RADENERGY( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
!-----------------------------------------------------------------------
!     Start execution of the problem after reading keywords and
!     performing error checks on the problem definition
!-----------------------------------------------------------------------
!
! *** Elapsed time message
      CALL ELAPSE( 1, IRPT, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Set the species growth and respiration rates
      CALL SET_GROWTH_RESPIRE( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** More error checks on the input keyword data
      CALL CHECK_ECEM_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Check the predation matrix
      IF( BGVERB ) CALL TELLTIME( 'Checking the Predation Matrix', 'SCREEN', .FALSE., IRPT )
      CALL PRED_CHECK( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Read the Kd data for inorganic analytes with Henry's coefficient>0
      IF( NEED_KD ) THEN
        IF( BGVERB ) CALL TELLTIME( 'Reading Kd library data', 'SCREEN', .FALSE., IRPT )
        CALL OPEN_KDSOIL( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
! *** Echo the second part of the problem setup to the report file
      CALL ECHO2( )
!
!-----------------------------------------------------------------------
! *** Generate stochastic variables
!-----------------------------------------------------------------------
!
      IF( BGVERB ) CALL TELLTIME( 'Generating Stochastic Variables', 'SCREEN', .FALSE., IRPT )
!
! *** Open the output file for generated values
      IF( BG_STOC_VALU ) THEN
        CALL OPEN_VALUE( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
! *** Generate stochastic variables that depend only on species
      CALL STOGEN_SPECIES( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Generate stochastic variables that depend only on analyte
      CALL STOGEN_ANALYTES( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Generate stochastic variables that depend on both analyte and species
      CALL STOGEN_ANALYTES_SPECIES( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
      IF( .NOT. EXECUT ) THEN
        MESSAG(1) = 'Execution not requested'
        MESSAG(2) = 'Use the EXECUTE card'
        CALL PRTERR( 0, CALLER, 2 )
        GO TO 1000
      END IF
      IF( BGVERB ) CALL TELLTIME( 'Starting execution', 'SCREEN', .FALSE., IRPT )
!
! *** Determine whether the analyte computation set is to be reduced after
!     the random sampling has been completed
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        IF( ESD_ANA(IANA)%THROTTLE ) THEN
          ESD_ANA(IANA)%COMP = .FALSE.
          WRITE(IRPT,1200) ESD_ANA(IANA)%ID
 1200     FORMAT('Analyte "',A,'" deactivated using the THROTTLE keyword')
        END IF
      END DO
!
!-----------------------------------------------------------------------
! *** Open the output files and write headers
!-----------------------------------------------------------------------
!
      IF( BGVERB ) CALL TELLTIME( 'File Handling', 'SCREEN', .FALSE., IRPT )
!
      IF( DET_USE ) THEN
        CALL OPEN_DETAIL( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
      IF( STA_USE ) THEN
        CALL OPEN_SUMMARY( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
      IF( THS_USE ) THEN
        CALL OPEN_THRESHOLD( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
!-----------------------------------------------------------------------
! *** ECDA Concentration file processing
!-----------------------------------------------------------------------
!
! *** Get a unit number
      IECD = GET_UNIT_NUMBER(  )
      IF( IECD .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for ECDA map file'
        CALL PRTERR( IERR, 'GET_UNIT_NUMBER', 1 )
        GO TO 999
      END IF
!
! *** Get the record number map for concentration data
      IF( BGVERB ) CALL TELLTIME( 'Reading ECDA index map data', 'SCREEN', .FALSE., IRPT )
      CALL ECDA_MAPREAD( FNMAP, IECD, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Echo the concentration index data header information to the report file
      IF( BGVERB ) CALL TELLTIME( 'Echoing ECDA index map data', 'SCREEN', .FALSE., IRPT )
      CALL ECDA_MAPECHO( IRPT )
!
! *** Open all of the concentration files needed
      CALL OPEN_CON( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Report the units conversion values
      IF( BGCOST ) CALL REPORT_UNITS( )
!
! *** Report additional analyte information
      IF( BGANAL ) CALL REPORT_ANAL( )
!
!-----------------------------------------------------------------------
! *** Food (FCDA) concentration file processing
!-----------------------------------------------------------------------
!
      IF( FOODS ) THEN
!
        CALL TELLTIME( 'FCDA file handling', 'SCREEN', .FALSE., IRPT )
!
! ***   Assign the name to the path
        FOODMAP = TRIM(FOODPATH)//TRIM(FOODMAP)
!
! ***   Generate the index map for foods
        IF( BGVERB ) CALL TELLTIME( 'Generating FCDA index map data', 'SCREEN', .FALSE., IRPT )
        CALL FCDA_MAPGEN( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
! ***   Echo the food index data header information to the report file
        IF( BGVERB ) CALL TELLTIME( 'Echoing FCDA index map data', 'SCREEN', .FALSE., IRPT )
        CALL FCDA_MAPECHO( IRPT, IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
! ***   Write the index map for foods
        IF( BGVERB ) CALL TELLTIME( 'Writing FCDA index map data', 'SCREEN', .FALSE., IRPT )
        CALL FCDA_MAPWRITE( FOODMAP, IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
! ***   Create all of the food concentration files needed
        IF( BGVERB ) CALL TELLTIME( 'Opening food files', 'SCREEN', .FALSE., IRPT )
        CALL OPEN_FOODS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
      END IF
!
!----------------------------------------------------------------------------------------------------------------
!     Loop over all times, locations, and analytes and calculate the species results
!----------------------------------------------------------------------------------------------------------------
!
      TIME_LOOP: DO ITIM = 1, ESD_NUM_TIM
!
!       Skip times from the ESD keyword file not requested i nthis scenario
        IF( .NOT.ESD_TIM(ITIM)%COMP ) CYCLE
!
        CYEAR = ' '
        WRITE(CYEAR(1:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL TELLTIME( 'Computing year '//TRIM(CYEAR), 'SCREEN', .FALSE., IRPT )
!
        LOCATION_LOOP: DO ILOC = 1, ESD_NUM_LOC
!
!         Skip locations from the ESD keyword file not requested in this scenario
          IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
!
          IF( BGLOOP ) CALL TELLTIME( 'Computing year '//TRIM(CYEAR)//' for '//ESD_LOC(ILOC)%ID, 'BOTH', .FALSE., IRPT )
!
! ***     Initialize for the sum of radioactive doses over all analytes for a species
!         The assignment is in matrix form
          RADSUM_AQ = 0.0
          RADSUM_RP = 0.0
          RADSUM_UP = 0.0
!
! ***     Loop over all analytes
          ANALYTE_LOOP: DO IANA = 1, ESD_NUM_ANA
!
!           Skip analytes from the ESD keyword file not requested in this scenario
            IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
            IF( BGLOOP ) WRITE(IRPT,1280) IANA, ESD_ANA(IANA)%ID, ESD_LOC(ILOC)%ID, ESD_TIM(ITIM)%TIME
 1280       FORMAT(/'Processing Analyte: ',I3,' "',A,'" for location: ',A,' and time: ',I0)
!
! ***       Read the concentration data for one analyte, all realizations,
!           for this time slice and this location
            CALL CON_DATA( ITIM, ILOC, IANA, IERR )
            IF( IERR .NE. 0 ) GO TO 999
!
! ***       Calculate all species impacts at upland locations without irrigation
            IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SODR ) THEN
              BGTYPE  = 'UPLAND'
              BGWATER = 'Dry - No irrigation'
              CALL UPLAND_SPECIES( 'SODR', ITIM, ILOC, IANA, IERR )
              IF( IERR .NE. 0 ) GO TO 999
            END IF
!
! ***       Calculate all species impacts at upland locations with groundwater irrigation
            IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SOGW ) THEN
              BGTYPE  = 'UPLAND'
              BGWATER = 'Irrigated - ground water'
              CALL UPLAND_SPECIES( 'SOGW', ITIM, ILOC, IANA, IERR )
              IF( IERR .NE. 0 ) GO TO 999
            END IF
!
! ***       Calculate all species impacts at upland locations with surface water irrigation
            IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SOSW ) THEN
              BGTYPE  = 'UPLAND'
              BGWATER = 'Irrigated - surface water'
              CALL UPLAND_SPECIES( 'SOSW', ITIM, ILOC, IANA, IERR )
              IF( IERR .NE. 0 ) GO TO 999
            END IF
!
! ***       Calculate all species for aquatic locations
            IF( ESD_LOC(ILOC)%TYPE .EQ. 'AQUATIC' ) THEN
              BGTYPE  = 'AQUATIC'
              BGWATER = 'River'
              CALL AQUATIC_SPECIES( ITIM, ILOC, IANA, IERR )
              IF( IERR .NE. 0 ) GO TO 999
            END IF
!
! ***       Calculate all species for riparian locations (no irrigation - soil is wetted by seeps)
            IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
              BGTYPE  = 'RIPARIAN'
              BGWATER = 'Seep'
              CALL RIPARIAN_SPECIES( ITIM, ILOC, IANA, IERR )
              IF( IERR .NE. 0 ) GO TO 999
            END IF
!
          END DO ANALYTE_LOOP
!
! ***     Process the sum of the radionuclides
          CALL RADSUM_STATS( ITIM, ILOC, IERR )
          IF( IERR .NE. 0 ) GO TO 1000
!
        END DO LOCATION_LOOP
!
      END DO TIME_LOOP
!
! *** Normal completion of the program
      GO TO 1000
!
!---------------------------------------------------------------------------------------------------
!     Fatal errors trapped after report file is available return to this point for termination.
!     Errors trapped before the report file is available terminate at the point of the error trap.
!---------------------------------------------------------------------------------------------------
!
  999 CONTINUE
      IF( REPORT ) THEN
        MESSAG(1) = 'Error encountered in a lower-level routine.'
        MESSAG(2) = 'Execution halted because of the above errors.'
        CALL PRTERR( IERR, CALLER, 2 )
        CALL ELAPSE( 2, IRPT, IERR )
      ELSE
        WRITE(*,*) 'Error encountered in a lower-level routine.'
        WRITE(*,*) 'Execution halted because of the above errors.'
        WRITE(*,*) 'Program stop in ' // TRIM(CALLER)
      END IF
      CALL TELLTIME( 'Error Termination', 'SCREEN', .FALSE., IRPT )
      STOP
!
 1000 CONTINUE
!
! *** Elapsed time message
      CALL ELAPSE( 2, IRPT, IERR )
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, CALLER, 1 )
      CALL TELLTIME( 'Normal Termination', 'SCREEN', .FALSE., IRPT )
!
      END PROGRAM ECEM

      SUBROUTINE AQUATIC_SPECIES( ITIM, ILOC, IANA, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine schedules impact calculations for all species at aquatic locations.
!!
!!  History:
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 13 Jan 2005 : Optional outputs in aquatic routines
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!**************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Debug_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Media_Mod, ONLY: SOIL_ID, SOIL_IDX
      USE Results_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR              ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'AQUATIC_SPECIES' ! Name of this subroutine
!
      CHARACTER(LEN=5) :: CTMP ! Selection variable for impacts routine
      INTEGER :: ISPC ! Species index
      INTEGER :: IREL ! realization looping index
      INTEGER :: IDX  ! Species loop indexing variable
      LOGICAL :: SKIP_OUTPUTS ! Logical whether skip all data outputs in aquatic subroutines
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      IF( BGLOOP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID
 1000   FORMAT(/'In subroutine: ',A,' : Time = ',I0,' : Location = ',A,' : Analyte = ',A)
      END IF
!
! *** Set the soil type for use in output to details or statistics
      SOIL_ID = 'NONE'
      SOIL_IDX = 0
!
! *** Flag to support riparian/aquatic pairing outputs
      SKIP_OUTPUTS = .FALSE.
!
! *** Compute vapor concentrations from surface water concentrations if needed
      IF( NEED_VAPOR ) THEN
        CALL COMPUTE_VAPOR_WATER( ILOC, IANA, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
! *** Loop over all species
      SPECIES_LOOP: DO IDX = 1, ESD_NUM_SPC
!
! ***   Compute the species in the permuted order
!       Skip the species not used in this scenario
!
        ISPC = ESD_SPC(IDX)%ORDER
        IF( ISPC .LT. 0 ) CYCLE ! Species not used at all
        IF( ESD_SPC(ISPC)%HABITAT .NE. 'AQUATIC' ) CYCLE ! Species is not aquatic
!
        IF( BGLOOP ) WRITE(IRPT,1010) IDX, ISPC, ESD_SPC(ISPC)%ID
 1010   FORMAT(' Processing Species: Loop Index = ',I0,' Species index = ',I0,' Species ID = ',A)
!
        CTMP = ESD_ANA(IANA)%TYPE // '_' // ESD_SPC(ISPC)%TYPE
!        IF( BGLOOP ) WRITE(IRPT,'(A)') 'CTMP = '//CTMP
        SELECT CASE( CTMP )
!
          CASE('OR_QP')
! ***       Equations for ORGANIC RADIOACTIVE analytes and AQUATIC PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OR_QP for '//ESD_SPC(ISPC)%ID
            CALL OR_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_AQ(ISPC,IREL) = RADSUM_AQ(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('OR_QA')
! ***       Equations for ORGANIC RADIOACTIVE analytes and AQUATIC ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OR_QA for '//ESD_SPC(ISPC)%ID
            CALL OR_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_AQ(ISPC,IREL) = RADSUM_AQ(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('OS_QP')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and AQUATIC PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_QP for '//ESD_SPC(ISPC)%ID
            CALL OS_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('OS_QA')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and AQUATIC ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_QA for '//ESD_SPC(ISPC)%ID
            CALL OS_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('NR_QP')
! ***       Equations for NONORGANIC RADIOACTIVE analytes and AQUATIC PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_QP for '//ESD_SPC(ISPC)%ID
            CALL NR_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_AQ(ISPC,IREL) = RADSUM_AQ(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('NR_QA')
! ***       Equations for NONORGANIC RADIOACTIVE analytes AQUATIC ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_QA for '//ESD_SPC(ISPC)%ID
            CALL NR_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_AQ(ISPC,IREL) = RADSUM_AQ(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('NS_QP')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and AQUATIC PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_QP for '//ESD_SPC(ISPC)%ID
            CALL NS_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('NS_QA')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and AQUATIC ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_QA for '//ESD_SPC(ISPC)%ID
            CALL NS_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE DEFAULT
! ***       Other types
!
        END SELECT
!
      END DO SPECIES_LOOP
!
      RETURN
      END SUBROUTINE AQUATIC_SPECIES

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.
!!
!!  History:
!!    Paul W. Eslinger :  4 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise for TIIA
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger :  7 Feb 2014 : Modify format statements
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
 1000 FORMAT(/)
!
      WRITE(IRPT,1010) 'EEEEEEE   CCCCC   EEEEEEE  MM   MM'
      WRITE(IRPT,1010) 'E        C     C  E        M M M M'
      WRITE(IRPT,1010) 'E        C        E        M  M  M'
      WRITE(IRPT,1010) 'EEEEE    C        EEEEE    M  M  M'
      WRITE(IRPT,1010) 'E        C        E        M     M'
      WRITE(IRPT,1010) 'E        C     C  E        M     M'
      WRITE(IRPT,1010) 'EEEEEEE   CCCCC   EEEEEEE  M     M'
 1010 FORMAT(23X,A)
!
      WRITE(IRPT,1020) PRGNAM, TRIM(PRGVER), PRGDAT
 1020 FORMAT(//27X,A,' Version ',A/26X,'Last Modified on ',A)
!
      RETURN
      END SUBROUTINE BANNER_1

      SUBROUTINE BANNER_2( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints banner information to the report file.
!!
!!  History:
!!    Paul W. Eslinger :  9 May 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger :  7 Feb 2014 : Modify format statements
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
!---- First executable code --------------------------------------------
!
! *** Identification information
!
      WRITE(IRPT,1000) CRUNID, USRNAM
 1000 FORMAT(/10X,'Current Run ID = ',A14,'  User Name = ',A16)
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
      END SUBROUTINE BANNER_2

      SUBROUTINE CHECK_ECEM( IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the keyword cards
!!    looking for problem definition problems.  The error checking
!!    for stochastic variables is done at the point of generation
!!    instead of in this routine.
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 1999 : Version 1.1
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 11 Jan 2005 : Move RADIUS error check to ECEM
!!    Paul W. Eslinger : 13 Jan 2005 : Add riparian/aquatic food web checks
!!    Paul W. Eslinger : 11 May 2005 : Change growing season checks
!!    Paul W. Eslinger : 27 Sep 2005 : Change MOLDIFF checks
!!    Paul W. Eslinger : 18 Jul 2006 : (SCR-1137) Check simulation start date
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Change comments and QA counter
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger : 15 Aug 2007 : Add irrigation use variables
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Terminal error encountered
!!
!!********************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Debug_Mod
      USE Errors_Mod
      USE Radius_Mod
      USE Seeds_Mod
      USE Species_Mod
      USE Threshold_Mod
      USE Esd_Mod
      USE Stats_Mod, ONLY: BG_STOC_VALU
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error numnber indocator
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_ECEM' ! Name of this subroutine
!
      INTEGER :: ISPC  ! Species looping index
      INTEGER :: ILOC  ! Location looping index
      INTEGER :: IANA  ! Analyte looping index
      INTEGER :: ERRC  ! Error counter
      INTEGER :: SLOC  ! Secondary location index
!
!---- First executable code --------------------------------------------
!
      IERR = 0
      ERRC = 0
!
! *** File name errors
!
      IF( DET_USE .AND. (FNDET.EQ.' ') ) THEN
        IERR = 1
        ERRC = ERRC + 1
        MESSAG(1) = 'Name of output detailed data file not entered'
        MESSAG(2) = 'Enter it using the FILE keyword'
        MESSAG(3) = 'Use the DETAILS modifier'
        MESSAG(4) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
!
      IF( STA_USE .AND. FNSUM.EQ.' ' ) THEN
        IERR = 2
        ERRC = ERRC + 1
        MESSAG(1) = 'Name of output summary statistics file not entered'
        MESSAG(2) = 'Enter it using the FILE keyword'
        MESSAG(3) = 'Use the STATISTI modifier'
        MESSAG(4) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
!
      IF( THS_USE .AND. FNTHS.EQ.' ' ) THEN
        IERR = 4
        ERRC = ERRC + 1
        MESSAG(1) = 'Name of output threshold statistics file not entered'
        MESSAG(2) = 'Enter it using the FILE keyword'
        MESSAG(3) = 'Use the THRESHOL modifier'
        MESSAG(4) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
!
      IF( BG_STOC_VALU .AND. (FNVAL.EQ.' ') ) THEN
        IERR = 5
        ERRC = ERRC + 1
        MESSAG(1) = 'Name of output detailed generated data file not entered'
        MESSAG(2) = 'Enter it using the FILE keyword'
        MESSAG(3) = 'Use the VALUE modifier'
        MESSAG(4) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
!
!----------------------------------------------------------------------C
!     Control errors
!----------------------------------------------------------------------C
!
      IF( NREAL .LE. 0 ) THEN
        IERR = 6
        ERRC = ERRC + 1
        MESSAG(1) = 'At least one realization must be run'
        MESSAG(2) = 'Modify the REALIZATION card'
        MESSAG(3) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( NREAL .GT. ESD_NREAL ) THEN
        IERR = 7
        ERRC = ERRC + 1
        MESSAG(1) = 'Too many realizations are requested'
        MESSAG(2) = 'Modify the REALIZATION card'
        MESSAG(3) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( SDSTOC .LE. 1.0D0 ) THEN
        IERR = 8
        ERRC = ERRC + 1
        MESSAG(1) = 'The random seed must be greater than 1.0D0'
        MESSAG(2) = 'Modify the SEED card'
        MESSAG(3) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
!----------------------------------------------------------------------C
!     Analyte-related errors
!----------------------------------------------------------------------C
!
      IF( ECM_NUM_ANA .EQ. 0 ) THEN
        IERR = 9
        ERRC = ERRC + 1
        MESSAG(1) = 'At least one analyte must be requested'
        MESSAG(2) = 'Modify the ANALYTE card'
        MESSAG(3) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Air particulate fractions
!
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        IF( ESD_ANA(IANA)%PARTICLE.LT.0.0 .OR. ESD_ANA(IANA)%PARTICLE.GT.1.0 ) THEN
          IERR = 24
          MESSAG(1) = 'Invalid air particulate fraction - proper range is 0 to 1'
          MESSAG(2) = 'Analyte '//ESD_ANA(IANA)%ID//' : '//TRIM(ESD_ANA(IANA)%NAME)
          MESSAG(3) = 'Modify the value with the PARTICLE modifier on the ANALYTE keyword'
          MESSAG(4) = 'Problem with the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 4 )
        END IF
      END DO
!
! *** Air vapor calculations
!       MOLDIFF must be >0 when HENRY>0
!       Check if Kd data are needed for inorganic analytes
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
        IF( ESD_ANA(IANA)%HENRY .LE. 0.0 ) CYCLE
!
        IF( ESD_ANA(IANA)%MOLDIFF .LE.0.0 ) THEN
          IERR = 25
          MESSAG(1) = 'Invalid molecular diffusion value - Value greater than 0 is required'
          MESSAG(2) = 'Analyte '//ESD_ANA(IANA)%ID//' : '//TRIM(ESD_ANA(IANA)%NAME)
          MESSAG(3) = 'Modify the value with the MOLDIFF modifier on the ANALYTE keyword'
          MESSAG(4) = 'Problem with the ESD keyword file'
          CALL PRTERR( IERR, CALLER, 4 )
        END IF
!
!       Keed Kd data for inorganic analytes with Henry's > 0
        IF( ESD_ANA(IANA)%TYPE(1:1) .EQ. 'N' ) THEN
          NEED_KD = .TRUE.
        END IF
!
      END DO
!
!----------------------------------------------------------------------C
!     Species-related errors
!----------------------------------------------------------------------C
!
      IF( ESD_NUM_SPC .EQ. 0 ) THEN
        IERR = 10
        ERRC = ERRC + 1
        MESSAG(1) = 'At least one species must be requested'
        MESSAG(2) = 'Modify the SPECIES keywords'
        MESSAG(3) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Checks for the species growth rate for aquatic species
!     First determine if any aquatic species are being computed
!
      NUM_QA = 0
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        IF( ESD_SPC(ISPC)%TYPE(1:2) .EQ. 'QA' ) NUM_QA = NUM_QA + 1
      END DO
!
      IF( NUM_QA .GT. 0 ) THEN
!
        IF( GROWTH_DELTA .LE. 0.0 ) THEN
          IERR = 11
          ERRC = ERRC + 1
          MESSAG(1) = 'Species growth DELTA must be greater than 0'
          MESSAG(2) = 'Modify the GROWTH card, DELTA modifier'
          MESSAG(3) = 'Problem in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
        IF( GROWTH_BETA .LE. 0.0 ) THEN
          IERR = 12
          ERRC = ERRC + 1
          MESSAG(1) = 'Species growth BETA must be greater than 0'
          MESSAG(2) = 'Modify the GROWTH card, BETA modifier'
          MESSAG(3) = 'Problem in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
! ***   Checks for the species respiration rate
!
        IF( RESPIRE_PHI .LE. 0.0 ) THEN
          IERR = 13
          ERRC = ERRC + 1
          MESSAG(1) = 'Species respiration PHI must be greater than 0'
          MESSAG(2) = 'Modify the RESPIRE card, PHI modifier'
          MESSAG(3) = 'Problem in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
        IF( RESPIRE_GAMMA .LE. 0.0 ) THEN
          IERR = 14
          ERRC = ERRC + 1
          MESSAG(1) = 'Species respiration GAMMA must be greater than 0'
          MESSAG(2) = 'Modify the RESPIRE card, GAMMA modifier'
          MESSAG(3) = 'Problem in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
      END IF
!
!----------------------------------------------------------------------C
!    Location-related errors
!----------------------------------------------------------------------C
!
      IF( ECM_NUM_LOC .EQ. 0 ) THEN
        IERR = 15
        ERRC = ERRC + 1
        MESSAG(1) = 'At least one location must be computed'
        MESSAG(2) = 'Modify the LOCATION card'
        MESSAG(3) = 'Problem in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SOGW ) NEED_IRIG = .TRUE.
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SOSW ) NEED_IRIG = .TRUE.
      END DO
!
!----------------------------------------------------------------------C
!     Check the growing season definition
!----------------------------------------------------------------------C
!
! *** Determine if any terrestrial plant species are being computed
!
      NUM_TP = 0
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) NUM_TP = NUM_TP + 1
      END DO
!
! *** Check the values only if terrestrial plants are needed
!
      IF( NUM_TP .GT. 0 ) THEN
!
        IF( SEASON_START.LT.1 .OR. SEASON_START.GT.365 ) THEN
          IERR = 16
          ERRC = ERRC + 1
          MESSAG(1) = 'The growing season start date is invalid'
          MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
          MESSAG(3) = 'Value entered was '
          WRITE(MESSAG(3)(19:),'(I0)') SEASON_START
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
        IF( SEASON_END.LT.1 .OR. SEASON_END.GT.365 ) THEN
          IERR = 17
          ERRC = ERRC + 1
          MESSAG(1) = 'The growing season end date is invalid'
          MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
          MESSAG(3) = 'Value entered was '
          WRITE(MESSAG(3)(19:),'(I0)') SEASON_END
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
        IF( SEASON_END .LT. SEASON_START ) THEN
          IERR = 18
          ERRC = ERRC + 1
          MESSAG(1) = 'The growing season end date is before the start date'
          MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
!
!       Set the fraction of the year that is in the growing season
        SEASON_GROW = ( SEASON_END - SEASON_START + 1 ) / 365.0
        IF( SEASON_GROW .LE. 0.0 ) THEN
          IERR = 19
          ERRC = ERRC + 1
          MESSAG(1) = 'The fraction of the year that is growing season is 0 or negative'
          MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
!
      END IF
!
      IF( YEAR_START .LT. 0 ) THEN
        IERR = 24
        ERRC = ERRC + 1
        MESSAG(1) = 'Invalid start year for the simulation'
        MESSAG(2) = 'Check the modifier START on the PERIOD keyword in the ESD keyword file'
        MESSAG(3) = 'Value = '
        WRITE(MESSAG(3)(9:),'(I0)') YEAR_START
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
!----------------------------------------------------------------------C
!     Check on RADIUS information for radioactive analytes
!----------------------------------------------------------------------C
!
      USE_RADS = .FALSE.
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        IF( ESD_ANA(IANA)%TYPE(2:2) .EQ. 'R' ) USE_RADS = .TRUE.
      END DO
!
! *** If rads are needed check on RADIUS information
!
      IF( USE_RADS ) THEN
!
! ***   Variables for the radionuclide radius data
!
        IF( NRAD_RLIB .LT. 1 ) THEN
          IERR = 20
          ERRC = ERRC + 1
          MESSAG(1) = 'Invalid value for NRAD_RLIB'
          MESSAG(2) = 'Suggest modifying the ESD RADIUS keywords'
          MESSAG(3) = 'Not enough distances on the RADIUS keywords'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
        NANA_RLIB = NANA_RLIB - NHED_RLIB
        IF( NANA_RLIB .LT. 1 ) THEN
          IERR = 21
          ERRC = ERRC + 1
          MESSAG(1) = 'Invalid value for NANA_RLIB'
          MESSAG(2) = 'Suggest modifying the ESD RADIUS keywords'
          MESSAG(3) = 'Not enough analytes on the RADIUS keywords'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
      END IF
!
!----------------------------------------------------------------------------------------------------------
!     Check whether locations for riparian animals eating aquatic foods have an associated aquatic location
!----------------------------------------------------------------------------------------------------------
!
!     The food web checks in KEY_ECEM_2 determine whether further checks are needed here
!
      IF( RIP_AQ_PAIRS ) THEN
        DO ILOC = 1, ESD_NUM_LOC
!
!         Skip locations not requested in this scenario
          IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
!
!         Check that riparian locations are paired with aquatic locations
          IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
            IF( ESD_LOC(ILOC)%SECOND .EQ. 0 ) THEN
              IERR = 22
              ERRC = ERRC + 1
              MESSAG(1) = 'The food web requires a aquatic location to be associated with every'
              MESSAG(2) = 'riparian location.  This is not true for location '// ESD_LOC(ILOC)%ID
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              SLOC = ESD_LOC(ILOC)%SECOND
              IF( ESD_LOC(SLOC)%TYPE .NE. 'AQUATIC' ) THEN
                IERR = 23
                ERRC = ERRC + 1
                MESSAG(1) = 'The food web requires a aquatic location to be associated with every'
                MESSAG(2) = 'riparian location.  This is not true for location '// ESD_LOC(ILOC)%ID
                MESSAG(3) = 'The associated location with ID '//ESD_LOC(SLOC)%ID//' is of tpye '//ESD_LOC(SLOC)%TYPE
                CALL PRTERR( IERR, CALLER, 3 )
              END IF
            END IF
          END IF
!
        END DO
      END IF
!
!----------------------------------------------------------------------C
!     Ending message if errors encountered
!----------------------------------------------------------------------C
!
      IF( IERR .GT. 0 ) THEN
        MESSAG(1) = 'Ending consistency scan of inputs with III errors.'
        MESSAG(2) = 'Problems encountered in the ECEM keyword file'
        WRITE(MESSAG(1)(40:42),'(I3)') ERRC
        CALL PRTERR( 0, CALLER, 2 )
      END IF
!
      RETURN
      END SUBROUTINE CHECK_ECEM

      SUBROUTINE CHECK_ECEM_2( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine finishes scaning the information from the keyword cards looking for 
!!    problem definition problems.  Some of the checks done here can be done only after the 
!!    libraries of constant values have been read and processed.
!!
!!  History:
!!    Paul W. Eslinger :  4 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 11 May 2005 : Define species use variables
!!    Paul W. Eslinger : 18 Jul 2006 : Add check on start of irrigation
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Add rad benchmark solution
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!  Call List Variables:
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Terminal error encountered
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Seeds_Mod
      USE Species_Mod
      USE ESD_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** External functions
      LOGICAL, EXTERNAL :: STRCOMP
!
! *** Call list variables
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'CHECK_ECEM_2' ! Name of this subroutine
      INTEGER :: ERRC ! Error counter
      INTEGER :: ISPC ! Species index
      INTEGER :: ILOC ! Location index
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
!----------------------------------------------------------------------C
!     Analyte-related errors
!----------------------------------------------------------------------C
!
      IF( ECM_NUM_ANA .EQ. 0 ) THEN
        IERR = 1
        ERRC = ERRC + 1
        MESSAG(1) = 'At least one analyte must be requested'
        MESSAG(2) = 'Modify the ANALYTE card'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
!----------------------------------------------------------------------C
!     Species-related errors
!----------------------------------------------------------------------C
!
! *** Determine what kind of species are to be calculated
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT.ESD_SPC(ISPC)%COMP ) CYCLE
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) NEED_TP = .TRUE.
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'TA' ) NEED_TA = .TRUE.
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'QA' ) NEED_QA = .TRUE.
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'QP' ) NEED_QP = .TRUE.
      END DO
!
! *** Items needed for both terrestrial plants and animals
!
      IF( NEED_TA .OR. NEED_TP ) THEN
!
        IF( AREA .LE. 0.0 ) THEN
          IERR = 2
          ERRC = ERRC + 1
          MESSAG(1) = 'Exposure area must be greater than zero'
          CALL PRTERR( IERR, CALLER, 1 )
        END IF
!
        IF( COWHDFX .LE. 0.0 ) THEN
          IERR = 3
          ERRC = ERRC + 1
          MESSAG(1) = 'Cowherds function must be greater than zero'
          CALL PRTERR( IERR, CALLER, 1 )
        END IF
!
        IF( EXPOSI .LE. 0.0 ) THEN
          IERR = 4
          ERRC = ERRC + 1
          MESSAG(1) = 'Exposure interval must be greater than zero'
          CALL PRTERR( IERR, CALLER, 1 )
        END IF
!
        IF( LENGTH .LE. 0.0 ) THEN
          IERR = 5
          ERRC = ERRC + 1
          MESSAG(1) = 'Location exposure length must be greater than zero'
          CALL PRTERR( IERR, CALLER, 1 )
        END IF
!
        IF( RFRAC .LE. 0.0 ) THEN
          IERR = 6
          ERRC = ERRC + 1
          MESSAG(1) = 'Respirable fraction must be greater than zero'
          CALL PRTERR( IERR, CALLER, 1 )
        END IF
!
        IF( NEED_IRIG .AND. IRIG_START .LT. YEAR_START ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'The first year of irrigation is before the simulation start year'
          MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
!
      END IF
!
! *** Items needed for terrestrial plants (none)
!
! *** Species definitions
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT.(ESD_SPC(ISPC)%TYPE.EQ.'TP' .OR. ESD_SPC(ISPC)%TYPE.EQ.'TA' .OR. &
                  ESD_SPC(ISPC)%TYPE.EQ.'QP' .OR. ESD_SPC(ISPC)%TYPE.EQ.'QA') ) THEN
          IERR = 8
          ERRC = ERRC + 1
          MESSAG(1) = 'Species type is not TP, TA, QP, or QA'
          MESSAG(2) = 'Species ID = "'//TRIM(ESD_SPC(ISPC)%ID)//'"'
          MESSAG(3) = 'Species TYPE = "'//TRIM(ESD_SPC(ISPC)%TYPE)//'"'
          MESSAG(4) = 'Modify the SPECIES keyword in the ESD keyword file'
          CALL PRTERR( IERR, CALLER, 4 )
        END IF
      END DO
!
! *** Rad benchmark values
!
      IF( NEED_TP .AND. DET_BMRDOS ) THEN
        IF( TP_BMR .LT. 0.0 ) THEN
          IERR = 9
          ERRC = ERRC + 1
          MESSAG(1) = 'Terrestrial plant rad benchmark is not greater than zero'
          MESSAG(2) = 'Modify the TP_BMR modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
        IF( .NOT.STRCOMP(TP_UNITS,'rad/day',7) ) THEN
          IERR = 10
          ERRC = ERRC + 1
          MESSAG(1) = 'Units for terrestrial plant rad benchmark is not rad/day'
          MESSAG(2) = 'Modify the TP_UNITS modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
      IF( NEED_QP .AND. DET_BMRDOS ) THEN
        IF( QP_BMR .LT. 0.0 ) THEN
          IERR = 11
          ERRC = ERRC + 1
          MESSAG(1) = 'Aquatic plant rad benchmark is not greater than zero'
          MESSAG(2) = 'Modify the QP_BMR modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
        IF( .NOT.STRCOMP(QP_UNITS,'rad/day',7) ) THEN
          IERR = 12
          ERRC = ERRC + 1
          MESSAG(1) = 'Units for aquatic plant rad benchmark is not rad/day'
          MESSAG(2) = 'Modify the QP_UNITS modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
      IF( NEED_TA .AND. DET_BMRDOS ) THEN
        IF( TA_BMR .LT. 0.0 ) THEN
          IERR = 13
          ERRC = ERRC + 1
          MESSAG(1) = 'Terrestrial animal rad benchmark is not greater than zero'
          MESSAG(2) = 'Modify the TA_BMR modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
        IF( .NOT.STRCOMP(TA_UNITS,'rad/day',7) ) THEN
          IERR = 14
          ERRC = ERRC + 1
          MESSAG(1) = 'Units for terrestrial animal rad benchmark is not rad/day'
          MESSAG(2) = 'Modify the TA_UNITS modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
      IF( NEED_QP .AND. DET_BMRDOS ) THEN
        IF( QP_BMR .LT. 0.0 ) THEN
          IERR = 15
          ERRC = ERRC + 1
          MESSAG(1) = 'Aquatic plant rad benchmark is not greater than zero'
          MESSAG(2) = 'Modify the QP_BMR modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
        IF( .NOT.STRCOMP(QP_UNITS,'rad/day',7) ) THEN
          IERR = 16
          ERRC = ERRC + 1
          MESSAG(1) = 'Units for aquatic plant rad benchmark is not rad/day'
          MESSAG(2) = 'Modify the QP_UNITS modifier for the VARIABLE keyword in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check that temperature is defined for land locations
      DO ILOC = 1, ESD_NUM_LOC
!       Skip locations from the ESD keyword file not requested in this scenario
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
!       Aquatic locations need temperatures under some conditions
        IF( ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC ' .AND. NEED_VAPOR ) THEN
          IF( ESD_LOC(ILOC)%TEMP .LT. -40.0 ) THEN
            IERR = 18
            ERRC = ERRC + 1
            MESSAG(1) = 'Location with ID "'//ESD_LOC(ILOC)%ID//'" does not have a valid temperature (degrees C)'
            MESSAG(2) = 'Use the TEMP modifier on the LOCATION keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
        END IF
      END DO
!
!----------------------------------------------------------------------C
!     Ending message if errors encountered
!----------------------------------------------------------------------C
!
      IF( IERR .GT. 0 ) THEN
        MESSAG(1) = 'Ending consistency scan of inputs with III errors.'
        WRITE(MESSAG(1)(40:42),'(I3)') ERRC
        CALL PRTERR( 0, CALLER, 1 )
      END IF
!
      RETURN
      END SUBROUTINE CHECK_ECEM_2

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
!!    Paul W. Eslinger : 21 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 11 Jan 2005 : Move RADIUS error check to ECEM
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Esd_Mod
      USE Errors_Mod
      USE Radius_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Output integer error flag
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'CHECK_ESD' ! Name of this subroutine
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Number of analytes in the ESD file
      IF( ESD_NUM_ANA .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least one analyte required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Number of ecological locations in the ESD file
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) = 'At least one ECOLOGICAL location required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Number of ecological times in the ESD file
      IF( ESD_NUM_TIM .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'At least one ECOLOGICAL time required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Number of realizations in the ESD file
      IF( ESD_NREAL .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'At least one realization required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Number of species in the ESD file
      IF( ESD_NUM_SPC .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'At least one species required in the ESD keyword file'
        MESSAG(2) = 'The ESD file must be modified'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
      RETURN
      END SUBROUTINE CHECK_ESD

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
!!    Paul W. Eslinger : 14 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      INTEGER :: IERR ! Error number (0=no errors)
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_IDEN' ! Name of this subroutine
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Identification errors
!
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 1
        MESSAG(1) = 'A user name must be entered'
        MESSAG(2) = 'Use the keyword USER in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'A problem title must be entered'
        MESSAG(2) = 'Use the keyword TITLE in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      RETURN
      END SUBROUTINE CHECK_IDEN

      SUBROUTINE CHECK_SPECIES( ISPC, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine checks deterministic parameters defined for each species looking for 
!!    incorrect specifications.  A flag is set if any of the aquatic species have inhalation rate
!!    greater than zero (implying that vapor calculations from surface water are needed.)
!!
!!  Note:
!!    Do not initialize IERR to zero on entry.  This allows checking all species
!!    before exiting to the calling routine if one or more errors are found.
!!
!!  History:
!!    Paul W. Eslinger :  1 Sep 2005 : Original source
!!    Paul W. Eslinger :  6 Sep 2006 : Change comments
!!    Paul W. Eslinger : 14 Sep 2006 : Add SOILING error check
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger : 15 Aug 2007 : Replace NEED_RD with USE_RADS
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Debug_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ISPC ! Species index for reporting
      INTEGER :: IERR             ! Error flag
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'CHECK_SPECIES' ! Name of this routine
      INTEGER :: IANA    ! Analyte looping index
      LOGICAL :: NEED_OR ! Organic analytes are being computed
!
!---- First executable code --------------------------------------------
!
! *** Check whether organic analytes are needed
!
      NEED_OR = .FALSE.
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        IF( ESD_ANA(IANA)%TYPE(1:1) .EQ. 'O' ) NEED_OR = .TRUE.
      END DO
!
! *** The checks depend on the species type
!
      SELECT CASE( ESD_SPC(ISPC)%TYPE )
!
        CASE('QA') ! Aquatic Animals -----------------------------------------------------------
!
          IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
            NEED_VAPOR = .TRUE.
          END IF
!
          IF( ESD_SPC(ISPC)%AWD .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'AWD : wet-to-dry weight ratio'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FOC .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FOC : fraction organic carbon'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%OCAR .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'OCAR : organic carbon assim. rate'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%WBMASS .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'WBMASS : wet body mass'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%AE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'AE : assimilation efficiency'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%GE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'GE : gross energy'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%SEDING .LT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'SEDING : Sediment ingestion rate'
            MESSAG(3) = 'Value must be greater than or equal to zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( USE_RADS ) THEN
            IF( ESD_SPC(ISPC)%RADIUS .LE. 0.0 ) THEN
              IERR = IERR + 1
              MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(2) = 'Definition problem for '//'RADIUS : radius'
              MESSAG(3) = 'Value must be greater than zero'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
          IF( NEED_OR ) THEN
            IF( ESD_SPC(ISPC)%FLIPID.LT.0.0 .OR. ESD_SPC(ISPC)%FLIPID.GT.1.0 ) THEN
              IERR = IERR + 1
              MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(2) = 'Definition problem for '//'FLIPID : fraction lipid'
              MESSAG(3) = 'Value must be in the range of 0 to 1'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
        CASE('QP') ! Aquatic Plants ------------------------------------------------------------
!
          IF( ESD_SPC(ISPC)%AWD .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'AWD : wet-to-dry weight ratio'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FOC .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FOC : fraction organic carbon'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%WBMASS .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'WBMASS : wet body mass'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%AE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'AE : assimilation efficiency'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%GE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'GE : gross energy'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( USE_RADS ) THEN
            IF( ESD_SPC(ISPC)%RADIUS .LE. 0.0 ) THEN
              IERR = IERR + 1
              MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(2) = 'Definition problem for '//'RADIUS : radius'
              MESSAG(3) = 'Value must be greater than zero'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
          IF( NEED_OR ) THEN
            IF( ESD_SPC(ISPC)%FLIPID.LT.0.0 .OR. ESD_SPC(ISPC)%FLIPID.GT.1.0 ) THEN
              IERR = IERR + 1
              MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(2) = 'Definition problem for '//'FLIPID : fraction lipid'
              MESSAG(3) = 'Value must be in the range of 0 to 1'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
        CASE('TA') ! Terrestrial Animals -------------------------------------------------------
!
          IF( ESD_SPC(ISPC)%AE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'AE : assimilation efficiency'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%GE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'GE : gross energy'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%DIFFHT .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'DIFFHT : diffusion height'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%ETWATER.LT.0.0 .OR. ESD_SPC(ISPC)%ETWATER.GT.24.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'ETWATER : exposure time to water'
            MESSAG(3) = 'Value must be in the range of 0 to 24'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FMR .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FMR : metabolic rate of predator'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FWATER .LT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FWATER : frac exposure to water'
            MESSAG(3) = 'Value must be zero or greater'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FDW .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FDW : conversion dry weight to wet weight'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%INHRATE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'INHRATE : resting inhalation rate'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%PCS .LT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'PCS : fraction of SA in contact w/soil'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%PCW .LT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'PCW : fraction of SA available to water'
            MESSAG(3) = 'Value must be zero or greater'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%PSI.LT.0.0 .OR. ESD_SPC(ISPC)%PSI.GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'PSI : seasonality factor'
            MESSAG(3) = 'Value must be in the range of 0 to 1'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%SA .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'SA : surface area'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%SOILING .LT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'SOILING : Soil ingestion rate'
            MESSAG(3) = 'Value must be greater than or equal to zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%SADHER .LT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'SADHER : skin adherance factor'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%THETA.LT.0.0 .OR. ESD_SPC(ISPC)%THETA.GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'THETA : area use factor'
            MESSAG(3) = 'Value must be in the range of 0 to 1'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%WATERING .LT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'WATERING : water ingestion rate'
            MESSAG(3) = 'Value must be zero or greater'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%WEIGHT .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'WEIGHT : body weight'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( USE_RADS ) THEN
            IF( ESD_SPC(ISPC)%RADIUS .LE. 0.0 ) THEN
              IERR = IERR + 1
              MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(2) = 'Definition problem for '//'RADIUS : radius'
              MESSAG(3) = 'Value must be greater than zero'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
        CASE('TP') ! Terrestrial Plants --------------------------------------------------------
!
          IF( ESD_SPC(ISPC)%AE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'AE : assimilation efficiency'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%GE .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'GE : gross energy'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%DIFFHT .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'DIFFHT : diffusion height'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FPA.LT.0.0 .OR. ESD_SPC(ISPC)%FPA.GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FPA : volume fraction of plant tissue air'
            MESSAG(3) = 'Value must be in the range of 0 to 1'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FPW.LT.0.0 .OR. ESD_SPC(ISPC)%FPW.GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FPW : volume fraction of plant tissue water'
            MESSAG(3) = 'Value must be in the range of 0 to 1'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FW.LT.0.0 .OR. ESD_SPC(ISPC)%FW.GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FW : water weight fraction of plant tissue'
            MESSAG(3) = 'Value must be in the range of 0 to 1'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%FWATER.LT.0.0 .OR. ESD_SPC(ISPC)%FWATER.GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'FWATER : fraction exposure to water'
            MESSAG(3) = 'Value must be in the range of 0 to 1'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%RHOP .LE. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'RHOP : plant tissue density'
            MESSAG(3) = 'Value must be greater than zero'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( ESD_SPC(ISPC)%ETWATER.LT.0.0 .OR. ESD_SPC(ISPC)%ETWATER.GT.24.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(2) = 'Definition problem for '//'ETWATER : exposure time to water'
            MESSAG(3) = 'Value must be in the range of 0 to 24'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
!
          IF( NEED_OR ) THEN
            IF( ESD_SPC(ISPC)%FPL.LT.0.0 .OR. ESD_SPC(ISPC)%FPL.GT.1.0 ) THEN
              IERR = IERR + 1
              MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(2) = 'Definition problem for '//'FPL : volume fraction of plant tissue lipid'
              MESSAG(3) = 'Value must be in the range of 0 to 1'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
          IF( USE_RADS ) THEN
            IF( ESD_SPC(ISPC)%RADIUS .LE. 0.0 ) THEN
              IERR = IERR + 1
              MESSAG(1) = 'Species "'//TRIM(ESD_SPC(ISPC)%ID)//'" : "'//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(2) = 'Definition problem for '//'RADIUS : radius'
              MESSAG(3) = 'Value must be greater than zero'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
        CASE DEFAULT
! ***     Other types are not defined
!
      END SELECT
!
      RETURN
      END SUBROUTINE CHECK_SPECIES

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
!!    Paul W. Eslinger : 11 May 2005 : Original source
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      END SUBROUTINE COMMAND_LINE_HELP

      SUBROUTINE COMPUTE_PARTIC( ILOC, ISPC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes air particulate concentrations from soil
!!    concentrations based on a soil erosion model for a single location
!!    and species combination.
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Sep 2006 : (SCR-1140) Upgrade debug outputs
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Debug_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Media_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER :: IERR              ! Error flag
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'COMPUTE_PARTIC' ! Name of this subroutine
      INTEGER :: IREL ! Realization looping variable
!
      REAL :: TFV  ! Threshold friction velocity (m/sec)
      REAL :: UT   ! Erosion threshold wind speed at 10 m (m/sec)
      REAL :: PEF  ! Particulate emmision factor (m^3/kg)
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      IF( BGAIRC ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID
 1000   FORMAT(//,A,/'Airborne Particulate Calculations'/ &
          '---------------------------------'/ &
          'Location: ',A / &
          'Species : ',A / &
          '---------------------------------')
      END IF
!
! *** Compute the air (particulate) concentration
!
      IF( BGAIRC ) THEN
        WRITE(IRPT,1010) &
          ESD_LOC(ILOC)%NECF, ESD_LOC(ILOC)%APSD, ESD_LOC(ILOC)%MZWIND, &
          ESD_LOC(ILOC)%VEGCOV, ESD_LOC(ILOC)%MSWIND, ESD_LOC(ILOC)%SRH, &
          ESD_SPC(ISPC)%DIFFHT, LENGTH, AREA, RFRAC, COWHDFX
 1010   FORMAT(1P/ &
            '      Value       Units      Variable         Description'/ &
            3X,12('-'),3X,8('-'),3X,14('-'),3X,40('-')  /&
            3X,E12.5,' : unitless : ESD_LOC%NECF   : Nonerodible elements correction factor'/&
            3X,E12.5,' : mm       : ESD_LOC%APSD   : Aggregate particle size distribution'/ &
            3X,E12.5,' : m/sec    : ESD_LOC%MZWIND : Wind speed in mixing zone'/ &
            3X,E12.5,' : unitless : ESD_LOC%VEGCOV : Fraction of vegetative cover'/ &
            3X,E12.5,' : m/sec    : ESD_LOC%MSWIND : Mean annual wind speed'/ &
            3X,E12.5,' : m        : ESD_LOC%SRH    : Surface roughness height'/ &
            3X,E12.5,' : m        : ESD_SPC%DIFFHT : Diffusion height for species'/ &
            3X,E12.5,' : m        : LENGTH         : Length of side of contaminated area'/ &
            3X,E12.5,' : m^2      : AREA           : Contaminated area'/ &
            3X,E12.5,' : g/m^2-hr : RFRAC          : Respirable fraction'/ &
            3X,E12.5,' : unitless : COWHDFX        : Cowherd function (Cowherd et al. 1985)')
      END IF
!
! *** Loop over realizations
!
      DO IREL = 1, NREAL
!
!       Threshold friction velocity (m/sec)
        TFV = ESD_LOC(ILOC)%NECF * (64.0+0.0055*ESD_LOC(ILOC)%APSD*1000.0) / 100.0
!
!       Erosion threshold wind speed at 10 m (m/sec)
        UT  = TFV * LOG(10.0/ESD_LOC(ILOC)%SRH) / 0.4
        IF( UT .LE. 0.0 ) THEN
          IERR = 1
          MESSAG(1) = 'Local variable UT=0, cannot divide by 0'
          MESSAG(2) = 'UT = Erosion threshold wind speed at 10 m (m/sec)'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       Particulate emmision factor (m^3/kg)
        PEF = ( LENGTH * ESD_LOC(ILOC)%MZWIND * ESD_SPC(ISPC)%DIFFHT * &
          3600.0 / AREA ) * (1000.0 / &
          ( RFRAC * (1.0-ESD_LOC(ILOC)%VEGCOV) * &
          (ESD_LOC(ILOC)%MSWIND/UT)**3 * COWHDFX ) )
        IF( PEF .LE. 0.0 ) THEN
          IERR = 2
          MESSAG(1) = 'Local variable PEF=0, cannot divide by 0'
          MESSAG(2) = 'PEF = Particulate emmision factor (m^3/kg)'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       Particulate concentration (pCi/m^3 or ug/m^3)
        EC(IREL)%PARTIC = EC(IREL)%SOIL / PEF
!
        IF( BGAIRC ) THEN
          WRITE(IRPT,1020) IREL, EC(IREL)%SOIL, TFV, UT, PEF, EC(IREL)%PARTIC
 1020     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units          : Variable  : Description'/ &
            3X,12('-'),3X,14('-'),3X,9('-'),3X,37('-')  /&
            3X, E12.5,' : (pCi or ug)/kg : EC%SOIL   : Soil concentration'/&
            3X, E12.5,' : m/sec          : TFV       : Threshold friction velocity'/&
            3X, E12.5,' : m/sec          : UT        : Erosion threshold wind speed at 10 m'/&
            3X, E12.5,' : m^3/kg         : PEF       : Particulate emmision factor'/&
            3X, E12.5,' : (pCi or ug)/kg : EC%PARTIC : Particulate concentration')
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE COMPUTE_PARTIC

      SUBROUTINE COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
!!*********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes air vapor concentrations from soil
!!    concentrations for a single location, analyte, species combination.
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 24 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  1 Jul 2005 : Revise Kd equation
!!    Paul W. Eslinger :  7 Jul 2005 : Revise Kd equation again
!!    Paul W. Eslinger : 27 Jan 2006 : Revise Kd equation again (derivation)
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Input Kd for inorganic analytes
!!    Paul W. Eslinger :  6 Sep 2006 : (SCR-1140) Upgrade debug outputs
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Rename COMPUTE_VAPOR to COMPUTE_VAPOR_SOIL
!!
!!*********************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Debug_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Media_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=18) :: CALLER = 'COMPUTE_VAPOR_SOIL' ! Name of this subroutine
!
      INTEGER :: IREL ! Realization index
      REAL :: TMPKD  ! Temporary soil-water partition coefficient (cm^3/g)
      REAL :: TMPKAS ! Temporary soil/air partition coefficient (g soil/cm^3 air)
      REAL :: TMP    ! Temporary value for 10^Kow
      REAL :: DEI    ! Temporary effective diffusivity (cm^2/s)
      REAL :: ZETA   ! Temporary effective diffusion rate (cm^2/s)
      REAL :: TMP1   ! Temporary intermediate calculation
      REAL :: TMP2   ! Temporary intermediate calculation
      REAL :: TMP3   ! Temporary intermediate calculation
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
      IF( BGAIRC ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID
 1000   FORMAT(//,A,/'Local vapor Calculations'/ &
          '------------------------'/ &
          'Location: ',A / &
          'Analyte : ',A / &
          'Species : ',A / &
          '------------------------')
      END IF
!
! *** Compute the air (vapor) concentration
!
      DO IREL = 1, NREAL
!
        IF( ESD_ANA(IANA)%HENRY > 0.0 ) THEN
!
!         Calculate the Kd for organic analytes using a functional relationship
          IF( ESD_ANA(IANA)%TYPE(1:1) .EQ. 'O' ) THEN
!
!           Baker, J.R., J.R. Mihelcic, D.C. Luehrs, and J.P. Hickey.  1997.  Evaluation of estimation methods
!           for organic carbon normalized sorption coefficients.  Water Environmental Research 69:136-144.
!
!           Kd = Foc*(10^0.094)*(Kow^0.903)
            TMP = 10.0**ESD_ANA(IANA)%KOW(IREL)
!
!           Temporary soil-water partition coefficient (cm^3/g)
            TMPKD = ESD_LOC(ILOC)%FOC * (10.0**0.094) * (TMP**0.903)
!
            IF( TMPKD .LE. 0.0 ) THEN
              IERR = 1
              MESSAG(1) = 'Temporary Kd value for an organic analyte is zero or negative - calculations cannot continue'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID//' : '//TRIM(ESD_LOC(ILOC)%NAME)
              MESSAG(3) = 'Analyte  = '//ESD_ANA(IANA)%ID//' : '//TRIM(ESD_ANA(IANA)%NAME)
              MESSAG(4) = 'Species  = '//ESD_SPC(ISPC)%ID//' : '//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'("Realization=",I0,"  KOW=",1P,E12.5,"  FOC=",E12.5,"  TMPKD=",E12.5)') IREL, &
                ESD_ANA(IANA)%KOW(IREL), ESD_LOC(ILOC)%FOC, TMPKD
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
          ELSE
!
!           Reference the user supplied Kd for inorganic analytes with Henry's coefficient > 0
            TMPKD = KD(IANA,IREL)
!
            IF( TMPKD .LE. 0.0 ) THEN
              IERR = 2
              MESSAG(1) = 'Kd value for an inorganic analyte is zero or negative - calculations cannot continue'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID//' : '//TRIM(ESD_LOC(ILOC)%NAME)
              MESSAG(3) = 'Analyte  = '//ESD_ANA(IANA)%ID//' : '//TRIM(ESD_ANA(IANA)%NAME)
              MESSAG(4) = 'Species  = '//ESD_SPC(ISPC)%ID//' : '//TRIM(ESD_SPC(ISPC)%NAME)
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'("Realization=",I0,"  TMPKD=",E12.5)') IREL, TMPKD
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
          END IF
!
!         Temporary soil/air partition coefficient (g soil/cm^3 air)
          TMPKAS = ( ESD_ANA(IANA)%HENRY * 41.0 ) / ( TMPKD*KASCONV )
!
!         Effective diffusivity (cm^2/s)
          DEI = ESD_ANA(IANA)%MOLDIFF * ESD_LOC(ILOC)%POROSITY**(0.33)
!
!         ZETA: Temporary effective diffusion rate (cm^2/s)
          ZETA = (DEI * ESD_LOC(ILOC)%POROSITY ) / &
            ( ESD_LOC(ILOC)%POROSITY + ( ESD_LOC(ILOC)%RHOS*(1.0-ESD_LOC(ILOC)%POROSITY)/TMPKAS ) )
!
          TMP1 = LENGTH * ESD_LOC(ILOC)%MZWIND * ESD_SPC(ISPC)%DIFFHT / AREA
          TMP2 = SQRT( PI * ZETA * EXPOSI )
          TMP3 = 2.0 * DEI * ESD_LOC(ILOC)%POROSITY * TMPKAS/1000.0
          IF( TMP1.LE.0.0 .OR. TMP2.LE.0.0 .OR. TMP3.LE.0.0 ) THEN
            IERR = 3
            MESSAG(1) = 'Temporary computed value is zero or negative - calculations cannot continue'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID//' : '//TRIM(ESD_LOC(ILOC)%NAME)
            MESSAG(3) = 'Analyte  = '//ESD_ANA(IANA)%ID//' : '//TRIM(ESD_ANA(IANA)%NAME)
            MESSAG(4) = 'Species  = '//ESD_SPC(ISPC)%ID//' : '//TRIM(ESD_SPC(ISPC)%NAME)
            MESSAG(5) = ' '
            WRITE(MESSAG(5),'("Realization=",I0,"  TMP1=",1P,E12.5,"  TMP2=",E12.5,"  TMP3=",E12.5)') IREL, TMP1, TMP2, TMP3
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Vapor concentration
          EC(IREL)%VAPOR = EC(IREL)%SOIL * 10000.0 / (TMP1*TMP2/TMP3)
!
          IF( BGAIRC ) THEN
            WRITE(IRPT,1010) IREL, ESD_ANA(IANA)%HENRY, ESD_ANA(IANA)%MOLDIFF, ESD_ANA(IANA)%KOW(IREL), ESD_LOC(ILOC)%FOC, &
              ESD_LOC(ILOC)%POROSITY, ESD_LOC(ILOC)%RHOS, ESD_LOC(ILOC)%MZWIND, ESD_SPC(ISPC)%DIFFHT, TMP1, TMP2, TMP3, &
              DEI, ZETA, KASCONV, TMPKD, TMPKAS, LENGTH, AREA, EXPOSI, EC(IREL)%SOIL, EC(IREL)%VAPOR
 1010       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value       Units                       Variable           Description'/ &
              3X,12('-'),3X,25('-'),3X,16('-'),3X,40('-')  /&
              3X,E12.5,' : Pa-m^3/mol                : ESD_ANA%HENRY    : Henrys law constant'/&
              3X,E12.5,' : cm^2/sec                  : ESD_ANA%MOLDIFF  : Molecular diffusivity'/ &
              3X,E12.5,' : unitless                  : ESD_ANA%KOW      : Log base 10 of octanol-water partition coefficient'/&
              3X,E12.5,' : g organic carbon/g dry wt : ESD_SPC%FOC      : Fraction organic carbon'/ &
              3X,E12.5,' : unitless                  : ESD_LOC%POROSITY : Soil porosity'/ &
              3X,E12.5,' : g/cm^3                    : ESD_LOC%RHOS     : Soil density'/ &
              3X,E12.5,' : m/sec                     : ESD_LOC%MZWIND   : Wind speed in mixing zone'/ &
              3X,E12.5,' : m                         : ESD_SPC%DIFFHT   : Diffusion height for species'/ &
              3X,E12.5,' : NA                        : TMP1             : Temporary value'/ &
              3X,E12.5,' : NA                        : TMP2             : Temporary value'/ &
              3X,E12.5,' : NA                        : TMP3             : Temporary value'/ &
              3X,E12.5,' : cm^2/sec                  : DEI              : Effective diffusivity'/ &
              3X,E12.5,' : cm^2/sec                  : ZETA             : Effective diffusion rate'/ &
              3X,E12.5,' : Pascal/atmosphere         : KASCONV          : Conversion factor'/ &
              3X,E12.5,' : cm^3/g                    : TMPKD            : Soil-water partition coefficient'/ &
              3X,E12.5,' : g soil/cm^3 air           : TMPKAS           : Soil/air partition coefficient for contaminant'/ &
              3X,E12.5,' : m                         : LENGTH           : Length of side of contaminated area'/ &
              3X,E12.5,' : m^2                       : AREA             : Contaminated area'/ &
              3X,E12.5,' : sec                       : EXPOSI           : Exposure interval'/ &
              3X,E12.5,' : (pCi or ug)/kg            : EC%SOIL          : Soil concentration'/&
              3X,E12.5,' : (pCi or ug)/kg            : EC%VAPOR         : Vapor concentration')
          END IF
!
        ELSE ! When Henry's coefficient is zero
!
          EC(IREL)%VAPOR = 0.0
!
          IF( BGAIRC ) THEN
            WRITE(IRPT,1020) IREL, ESD_ANA(IANA)%HENRY, EC(IREL)%VAPOR
 1020       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value       Units            Variable        Description'/ &
              3X,12('-'),3X,14('-'),3X,13('-'),3X,40('-')  /&
              3X,E12.5,' : Pa-m^3/mol     : ESD_ANA%HENRY : Henrys law constant'/&
              3X,E12.5,' : (pCi or ug)/kg : EC%VAPOR      : Vapor concentration')
          END IF
!
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE COMPUTE_VAPOR_SOIL

      SUBROUTINE COMPUTE_VAPOR_WATER( ILOC, IANA, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine computes all realizations of air vapor concentrations from surface water 
!!    concentrations for a single location and analyte combination.
!!
!!  History:
!!    Paul W. Eslinger :  7 Feb 2014 : Original version
!!
!!**************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Debug_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Media_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=19) :: CALLER = 'COMPUTE_VAPOR_WATER' ! Name of this subroutine
!
      INTEGER :: IREL ! Realization index
      REAL :: TMPKD  ! Temporary soil-water partition coefficient (cm^3/g)
      REAL :: TMPKAS ! Temporary soil/air partition coefficient (g soil/cm^3 air)
      REAL :: TMP    ! Temporary value for 10^Kow
      REAL :: DEI    ! Temporary effective diffusivity (cm^2/s)
      REAL :: ZETA   ! Temporary effective diffusion rate (cm^2/s)
      REAL :: TMP1   ! Temporary intermediate calculation
      REAL :: TMP2   ! Temporary intermediate calculation
      REAL :: TMP3   ! Temporary intermediate calculation
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
      IF( BGAIRC ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID
 1000   FORMAT(//,A,/'Local vapor Calculations'/ &
          '------------------------'/ &
          'Location: ',A / &
          'Analyte : ',A / &
          '------------------------')
      END IF
!
! *** Compute the air (vapor) concentration
!
      DO IREL = 1, NREAL
!
        IF( ESD_ANA(IANA)%HENRY > 0.0 ) THEN ! When Henry's coefficient is greater than zero
          EC(IREL)%VAPOR = 1000.0 * VAPORDILUTE * EC(IREL)%SWAT * ESD_ANA(IANA)%HENRY / (GASCON * (ESD_LOC(ILOC)%TEMP+273.15))
          IF( BGAIRC ) THEN
            WRITE(IRPT,1010) IREL, ESD_ANA(IANA)%HENRY, ESD_LOC(ILOC)%TEMP, VAPORDILUTE, EC(IREL)%SWAT, EC(IREL)%VAPOR
 1010       FORMAT(/'   Realization Number : ',I0,/ &
              '      Value       Units                       Variable           Description'/ &
              3X,12('-'),3X,25('-'),3X,16('-'),3X,40('-')  /&
              3X,ES12.5,' : Pa-m^3/mol                : ESD_ANA%HENRY    : Henrys law constant'/&
              3X,ES12.5,' : Centigrade                : ESD_LOC%TEMP     : Temperature'/ &
              3X,ES12.5,' : unitless                  : VAPORDILUTE      : Vapor dilution coefficient'/ &
              3X,ES12.5,' : (pCi or ug)/L             : EC%SWAT          : Surface water concentration'/&
              3X,ES12.5,' : (pCi or ug)/m^3           : EC%VAPOR         : Vapor concentration')
          END IF
!
        ELSE ! When Henry's coefficient is zero
          EC(IREL)%VAPOR = 0.0
          IF( BGAIRC ) THEN
            WRITE(IRPT,1020) IREL, ESD_ANA(IANA)%HENRY, EC(IREL)%VAPOR
 1020       FORMAT(/'   Realization Number : ',I0,/ &
              '      Value       Units            Variable        Description'/ &
              3X,12('-'),3X,25('-'),3X,16('-'),3X,40('-')  /&
              3X,ES12.5,' : Pa-m^3/mol                : ESD_ANA%HENRY    : Henrys law constant'/&
              3X,ES12.5,' : (pCi or ug)/m^3           : EC%VAPOR         : Vapor concentration')
          END IF
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE COMPUTE_VAPOR_WATER

      SUBROUTINE CON_DATA( ITIM, ILOC, IANA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the concentration data for all media from
!!    their respective binary (ECDA) concentration data files for a
!!    single time, location, and analyte combination.
!!
!!  Notes:
!!
!!    If there are no concentration data in the ECDA file, set the
!!    concentrations to zero for that particular media
!!
!!  History:
!!
!!    Paul W. Eslinger : 17 Apr 2000 : Version 1.0
!!    Paul W. Eslinger : 25 Jun 2001 : Version 1.0 : Add error trap on
!!      negative concentrations
!!    Paul W. Eslinger :  4 Feb 2002 : Add concentration statistics and
!!      link detailed data output to DET_CONCEN rather than DET_BURDEN
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 18 Jan 2005 : Change output formats
!!    Paul W. Eslinger :  1 Feb 2006 : Change subroutine calls for
!!                                     debug outputs to use vector
!!                                     instead of a structure element
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Control_Mod
      USE Media_Mod
      USE Debug_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Ecda_Mod
      USE Files_Mod
      USE Stats_Mod, ONLY: RWORK
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IANA ! Analyte index
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'CON_DATA' ! Name of this subroutine
!
      INTEGER :: IREL  ! Realization looping index
!
      INTEGER :: IDX_AIRC ! Air concentration data record number
      INTEGER :: IDX_AIRD ! Air deposition data record number
      INTEGER :: IDX_GWAT ! Groundwater concentration data record number
      INTEGER :: IDX_SEEP ! Seep water concentration data record number
      INTEGER :: IDX_SWAT ! Surface water concentration data record number
      INTEGER :: IDX_SEDI ! Sediment concentration data record number
      INTEGER :: IDX_PWAT ! Pore water concentration data record number
      INTEGER :: IDX_SORP ! Riparian soil concentration data record number
      INTEGER :: IDX_SODR ! Dry-land soil concentration data record number
      INTEGER :: IDX_SOGW ! Groundwater irrigated soil concentration data record number
      INTEGER :: IDX_SOSW ! Surface water irrigated soil concentration data record number
!
      INTEGER :: TIME            ! Calendar year from file
      CHARACTER(LEN=6) :: LOC_ID ! Location ID from file
      CHARACTER(LEN=4) :: MED_ID ! Media ID from file
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  5th percentile
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
!---- Executable code --------------------------------------------
!
      IERR = 0
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
! *** Error check on the analyte index
!
      IF( IANA.LT.1 .OR. IANA.GT.ESD_NUM_ANA ) THEN
        IERR = 3
        MESSAG(1) = 'Bad analyte index for reading concentration data'
        MESSAG(2) = 'Valid range is 1 to '
        WRITE(MESSAG(2)(22:),*) ESD_NUM_ANA
        MESSAG(3) = 'Value entered was '
        WRITE(MESSAG(3)(20:),*) IANA
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      IF( BGCONC ) THEN
!
        WRITE(IRPT,1000) 'Entering ' // CALLER
 1000   FORMAT(/A)
!
        WRITE(IRPT,1010) ESD_LOC(ILOC)%ID, 'Location', ILOC
        WRITE(IRPT,1010) ESD_ANA(IANA)%ID, 'Analyte', IANA
 1010   FORMAT(3X,'"',A,'" ',A,' ID with index ',I0)
!
        WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, 'Calendar year'
        WRITE(IRPT,1020) ITIM, 'Time index'
        WRITE(IRPT,1020) NREAL, 'Number of realizations'
 1020   FORMAT(3X,I0,' : ',A)
!
      END IF
!
!--------------------------------------------------------------------------------------------
!     Air concentrations
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, IAIRC, IDX_AIRC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error retrieving ECDA index for air concentrations'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_AIRC .GT. 0 ) THEN
        CALL ECDA_READ( IDX_AIRC, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%AIRC = CVEC(IREL) * CONVERT_UNITS(IANA,IAIRC)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 5
              MESSAG(1) = 'Negative air concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 6
          MESSAG(1) = 'Error reading air concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%AIRC = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_AIRC.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%AIRC
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'AIRC', 'CONCEN', CONVERT_LABELS(IANA,IAIRC,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_AIRC.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%AIRC
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is air concentration'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'AIRC', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,IAIRC,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Air deposition
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, IAIRD, IDX_AIRD, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error retrieving ECDA index for air deposition'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_AIRD .GT. 0 ) THEN
        CALL ECDA_READ( IDX_AIRD, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%AIRD = CVEC(IREL) * CONVERT_UNITS(IANA,IAIRD)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 8
              MESSAG(1) = 'Negative air deposition encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 9
          MESSAG(1) = 'Error reading air deposition data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%AIRD = 0.0
      END IF
!
! *** Optional output of detailed depositions
      IF( DET_CONCEN .AND. IDX_AIRD.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%AIRD
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'AIRD', 'CONCEN', CONVERT_LABELS(IANA,IAIRD,2), CVEC )
      END IF
!
! *** Optionally compute and output deposition statistics
      IF( STA_CONCEN .AND. IDX_AIRD.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%AIRD
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is air deposition'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'AIRD', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,IAIRD,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Ground water
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, IGWAT, IDX_GWAT, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error retrieving ECDA index for groundwater'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_GWAT .GT. 0 ) THEN
        CALL ECDA_READ( IDX_GWAT, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%GWAT = CVEC(IREL) * CONVERT_UNITS(IANA,IGWAT)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 11
              MESSAG(1) = 'Negative groundwater concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 12
          MESSAG(1) = 'Error reading surface water concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%GWAT = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_GWAT.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%GWAT
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'GWAT', 'CONCEN', CONVERT_LABELS(IANA,IGWAT,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_GWAT.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%GWAT
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is groundwater'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'GWAT', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,IGWAT,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Surface water
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, ISWAT, IDX_SWAT, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error retrieving ECDA index for surface water'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_SWAT .GT. 0 ) THEN
        CALL ECDA_READ( IDX_SWAT, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%SWAT = CVEC(IREL) * CONVERT_UNITS(IANA,ISWAT)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 14
              MESSAG(1) = 'Negative surface water concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 15
          MESSAG(1) = 'Error reading surface water concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%SWAT = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_SWAT.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%SWAT
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'SWAT', 'CONCEN', CONVERT_LABELS(IANA,ISWAT,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_SWAT.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%SWAT
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is surface water'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'SWAT', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,ISWAT,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!    Seep water
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, ISEEP, IDX_SEEP, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error retrieving ECDA index for surface water'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_SEEP .GT. 0 ) THEN
        CALL ECDA_READ( IDX_SEEP, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%SEEP = CVEC(IREL) * CONVERT_UNITS(IANA,ISEEP)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 17
              MESSAG(1) = 'Negative seep water concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 18
          MESSAG(1) = 'Error reading seep water concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%SEEP = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_SEEP.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%SEEP
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'SEEP', 'CONCEN', CONVERT_LABELS(IANA,ISEEP,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_SEEP.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%SEEP
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is seep water'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'SEEP', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,ISEEP,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Sediment
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, ISEDI, IDX_SEDI, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 19
        MESSAG(1) = 'Error retrieving ECDA index for sediment'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_SEDI .GT. 0 ) THEN
        CALL ECDA_READ( IDX_SEDI, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%SEDI = CVEC(IREL) * CONVERT_UNITS(IANA,ISEDI)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 20
              MESSAG(1) = 'Negative sediment concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 21
          MESSAG(1) = 'Error reading sediment concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%SEDI = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_SEDI.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%SEDI
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'SEDI', 'CONCEN', CONVERT_LABELS(IANA,ISEDI,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_SEDI.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%SEDI
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is SEDI water'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'SEDI', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,ISEDI,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Pore water
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, IPWAT, IDX_PWAT, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 22
        MESSAG(1) = 'Error retrieving ECDA index for pore water'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_PWAT .GT. 0 ) THEN
        CALL ECDA_READ( IDX_PWAT, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%PWAT = CVEC(IREL) * CONVERT_UNITS(IANA,IPWAT)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 23
              MESSAG(1) = 'Negative pore water concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 24
          MESSAG(1) = 'Error reading pore water concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%PWAT = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_PWAT.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%PWAT
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'PWAT', 'CONCEN', CONVERT_LABELS(IANA,IPWAT,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_PWAT.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%PWAT
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is pore water'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'PWAT', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,IPWAT,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Riparian Soil
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, ISORP, IDX_SORP, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 25
        MESSAG(1) = 'Error retrieving ECDA index for riparian soil'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_SORP .GT. 0 ) THEN
        CALL ECDA_READ( IDX_SORP, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%SORP = CVEC(IREL) * CONVERT_UNITS(IANA,ISORP)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 26
              MESSAG(1) = 'Negative riparian soil concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 27
          MESSAG(1) = 'Error reading riparian soil concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%SORP = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_SORP.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%SORP
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', ESD_ANA(IANA)%ID, &
          'SORP', 'CONCEN', CONVERT_LABELS(IANA,ISORP,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_SORP.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%SORP
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is riparian soil'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'NONE', 'SORP', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,ISORP,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Dryland Soil
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, ISODR, IDX_SODR, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 28
        MESSAG(1) = 'Error retrieving ECDA index for dry land soil'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_SODR .GT. 0 ) THEN
        CALL ECDA_READ( IDX_SODR, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%SODR = CVEC(IREL) * CONVERT_UNITS(IANA,ISODR)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 29
              MESSAG(1) = 'Negative dry land soil concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 30
          MESSAG(1) = 'Error reading dry land soil concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%SODR = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_SODR.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%SODR
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SODR', ESD_ANA(IANA)%ID, &
          'SODR', 'CONCEN', CONVERT_LABELS(IANA,ISODR,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_SODR.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%SODR
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is dry land soil'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SODR', 'SODR', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,ISODR,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Groundwater irrigated soil
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, ISOGW, IDX_SOGW, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 31
        MESSAG(1) = 'Error retrieving ECDA index for groundwater irrigated soil'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_SOGW .GT. 0 ) THEN
        CALL ECDA_READ( IDX_SOGW, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%SOGW = CVEC(IREL) * CONVERT_UNITS(IANA,ISOGW)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 32
              MESSAG(1) = 'Negative groundwater irrigated soil concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 33
          MESSAG(1) = 'Error reading groundwater irrigated soil concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%SOGW = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_SOGW.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%SOGW
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SOGW', ESD_ANA(IANA)%ID, &
          'SOGW', 'CONCEN', CONVERT_LABELS(IANA,ISOGW,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_SOGW.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%SOGW
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is groundwater irrigated soil'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SOGW', 'SOGW', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,ISOGW,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Surface water irrigated soil
!--------------------------------------------------------------------------------------------
!
! *** Get the record number index
      CALL GET_INDEX( ITIM, ILOC, ISOSW, IDX_SOSW, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 34
        MESSAG(1) = 'Error retrieving ECDA index for surface water irrigated soil'
        MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read and error check the data
      IF( IDX_SOSW .GT. 0 ) THEN
        CALL ECDA_READ( IDX_SOSW, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, NREAL
            EC(IREL)%SOSW = CVEC(IREL) * CONVERT_UNITS(IANA,ISOSW)
          END DO
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 35
              MESSAG(1) = 'Negative surface water irrigated soil concentration encountered'
              MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        ELSE
          IERR = 36
          MESSAG(1) = 'Error reading surface water irrigated soil concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      ELSE
        FORALL(IREL=1:NREAL) EC(IREL)%SOSW = 0.0
      END IF
!
! *** Optional output of detailed concentrations
      IF( DET_CONCEN .AND. IDX_SOSW.GT.0 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = EC(IREL)%SOSW
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SOSW', ESD_ANA(IANA)%ID, &
          'SOSW', 'CONCEN', CONVERT_LABELS(IANA,ISOSW,2), CVEC )
      END IF
!
! *** Optionally compute and output concentration statistics
      IF( STA_CONCEN .AND. IDX_SOSW.GT.0 )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL)= EC(IREL)%SOSW
        CALL USTAT( CVEC, NREAL, RWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Location    = '//ESD_LOC(ILOC)%ID //'   Analyte = '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'Year        = '
          WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
          MESSAG(4) = 'Concentration type is surface water irrigated soil'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SOSW', 'SOSW', ESD_ANA(IANA)%ID, &
          'CONCEN', CONVERT_LABELS(IANA,ISOSW,2), XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
!--------------------------------------------------------------------------------------------
!     Debug write on concentration data
!--------------------------------------------------------------------------------------------
!
      IF( BGCONC ) THEN
        WRITE(IRPT,1030) ' '
        WRITE(IRPT,1030) 'Concentrations by media'
        WRITE(IRPT,1030) '    A_Index Analyte  L_Index Loc. ID  T_Index   Time  Media  M_Index Concentrations'
        WRITE(IRPT,1030) '   -------- ------- -------- ------- -------- ------ ------ -------- ------------------------------'
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'AIRC', &
          IDX_AIRC,(EC(IREL)%AIRC,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'AIRD', &
          IDX_AIRD,(EC(IREL)%AIRD,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'GWAT', &
          IDX_GWAT,(EC(IREL)%GWAT,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'SEEP', &
          IDX_SEEP,(EC(IREL)%SEEP,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'SWAT', &
          IDX_SWAT,(EC(IREL)%SWAT,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'PWAT', &
          IDX_PWAT,(EC(IREL)%PWAT,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'SEDI', &
          IDX_SEDI,(EC(IREL)%SEDI,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'SORP', &
          IDX_SORP,(EC(IREL)%SORP,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'SODR', &
          IDX_SODR,(EC(IREL)%SODR,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'SOGW', &
          IDX_SOGW,(EC(IREL)%SOGW,IREL=1,NREAL)
        WRITE(IRPT,1040) IANA,ESD_ANA(IANA)%ID, ILOC,ESD_LOC(ILOC)%ID, ITIM,ESD_TIM(ITIM)%TIME, 'SOSW', &
          IDX_SOSW,(EC(IREL)%SOSW,IREL=1,NREAL)
 1030   FORMAT(A)
 1040   FORMAT(3X,I8,2X,A, 1X,I8,2X,A, 1X,I8,1X,I6, 2X,A,2X,I8, 1P,100(1X,E12.5))
      END IF
!
      RETURN
      END SUBROUTINE CON_DATA

      SUBROUTINE ECEM_INIT( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes some global variables for ECEM.
!!
!!  History:
!!
!!    Paul W. Eslinger : 24 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Species_Mod
      USE Threshold_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: ISPC ! Species looping index
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ITYP ! Threshold ype looping index
!
!---- Executable code --------------------------------------------------
!
! *** Threshold information (set to not used)
!
      DO ISPC = 1, ESD_NUM_SPC
        THRESHOLD%SREXIST1(ISPC) = .FALSE.
        THRESHOLD%SREXIST2(ISPC) = .FALSE.
        THRESHOLD%SRLIMIT1(ISPC) = -1.0
        THRESHOLD%SRLIMIT2(ISPC) = -1.0
        DO IANA = 1, ESD_NUM_ANA
          DO ITYP = 1, MAXTYP
            THRESHOLD%EXIST1(ISPC,IANA,ITYP) = .FALSE.
            THRESHOLD%EXIST2(ISPC,IANA,ITYP) = .FALSE.
            THRESHOLD%LIMIT1(ISPC,IANA,ITYP) = -1.0
            THRESHOLD%LIMIT2(ISPC,IANA,ITYP) = -1.0
          END DO
        END DO
      END DO
!
! *** Initialize the predation matrix to 0 (CONSUME keywords overwrite
!     only the species eaten, others must stay at zero).
!
      PREDATE = RESHAPE( (/0.0,0.0/), (/ESD_NUM_SPC,ESD_NUM_SPC/), PAD=(/0.0/) )
!
      RETURN
      END SUBROUTINE ECEM_INIT

      SUBROUTINE ECEM_MEMORY( IERR )
!!**********************************************************************
!!
!! Purpose:
!!
!!    This subroutine allocates memory for stochastic variables that
!!    depend on the number of realizations or on the combination of
!!    number of analytes and realizations.
!!
!!  History:
!!
!!    Paul W. Eslinger : 13 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 21 Jan 2005 : Add food consumption calulations
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Control_Mod
      USE Esd_Mod
      USE Results_Mod
      USE Threshold_Mod
      USE Species_Mod
      USE Media_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'ECEM_MEMORY' ! Name of this routine
!
      INTEGER :: ISPC ! Species looping index
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: IERA ! Error status variable from the allocate action
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
! *** ESD_ANA : Individual components of the analyte structure
!
      DO IANA = 1, ESD_NUM_ANA
        CALL MEM_ECEM_ANA( IANA, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error allocating memory for ESD_ANA components'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END DO
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
! *** Check on the number of species
!
      IF( ESD_NUM_SPC .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'At least 1 species required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** ESD_SPC : Individual components of the species structure
!
      DO ISPC = 1, ESD_NUM_SPC
        CALL MEM_ECEM_SPC( ISPC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error allocating memory for ESD_SPC components'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END DO
!
! *** Variables depending on the number of species
!
      ALLOCATE( ISPCTMP(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating memory for ISPCTMP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( PREDATE(ESD_NUM_SPC,ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for PREDATE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( THRESHOLD%SREXIST1(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error allocating memory for SREXIST1'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( THRESHOLD%SREXIST2(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for SREXIST2'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( THRESHOLD%SRLIMIT1(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for SRLIMIT1'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( THRESHOLD%SRLIMIT2(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error allocating memory for SRLIMIT2'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Vectors depending on realizations
!
      ALLOCATE( CWORK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 11
        MESSAG(1) = 'Error allocating memory for CWORK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( FWORK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating memory for FWORK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( EC(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error allocating memory for EC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( TWORK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 14
        MESSAG(1) = 'Error allocating memory for TWORK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RDWORK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating memory for RDWORK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( EHQ(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error allocating memory for EHQ'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DDER(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 17
        MESSAG(1) = 'Error allocating memory for DDER'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DINH(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 18
        MESSAG(1) = 'Error allocating memory for DINH'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DING(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 19
        MESSAG(1) = 'Error allocating memory for DING'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DHAZ(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 20
        MESSAG(1) = 'Error allocating memory for DHAZ'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CVEC(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 21
        MESSAG(1) = 'Error allocating memory for CVEC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Arrays depending on the number of species and realizations
!
      ALLOCATE( CPAT(ESD_NUM_SPC,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 22
        MESSAG(1) = 'Error allocating memory for CPAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CPAI(ESD_NUM_SPC,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 23
        MESSAG(1) = 'Error allocating memory for CPAI'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CDER(ESD_NUM_SPC,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 24
        MESSAG(1) = 'Error allocating memory for CDER'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CING(ESD_NUM_SPC,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 25
        MESSAG(1) = 'Error allocating memory for CING'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CINH(ESD_NUM_SPC,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 26
        MESSAG(1) = 'Error allocating memory for CINH'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RADSUM_AQ(ESD_NUM_SPC,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 27
        MESSAG(1) = 'Error allocating memory for RADSUM_AQ'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RADSUM_RP(ESD_NUM_SPC,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 28
        MESSAG(1) = 'Error allocating memory for RADSUM_RP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RADSUM_UP(ESD_NUM_SPC,NREAL,ESD_NUM_SOI), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 29
        MESSAG(1) = 'Error allocating memory for RADSUM_UP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Depending on species, analytes and type
!
      ALLOCATE( THRESHOLD%EXIST1(ESD_NUM_SPC,ESD_NUM_ANA,MAXTYP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 30
        MESSAG(1) = 'Error allocating memory for EXIST1'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( THRESHOLD%EXIST2(ESD_NUM_SPC,ESD_NUM_ANA,MAXTYP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 31
        MESSAG(1) = 'Error allocating memory for EXIST2'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( THRESHOLD%LIMIT1(ESD_NUM_SPC,ESD_NUM_ANA,MAXTYP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 32
        MESSAG(1) = 'Error allocating memory for LIMIT1'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( THRESHOLD%LIMIT2(ESD_NUM_SPC,ESD_NUM_ANA,MAXTYP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 33
        MESSAG(1) = 'Error allocating memory for LIMIT2'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( FOOD(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 34
        MESSAG(1) = 'Error allocating memory for FOOD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE ECEM_MEMORY

      SUBROUTINE ECHO1(  )
!!********************************************************************************************
!!
!!  Purpose:
!!    This subroutine handles writing an echo of the identification information, file names, 
!!    and debug flag status to the report file.  The definition of stochastic variables is 
!!    deferred to later subroutines.
!!
!!  History:
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 1999 : Version 1.1
!!    Paul W. Eslinger : 24 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 18 Nov 2004 : Add irigation amount
!!    Paul W. Eslinger : 11 Jan 2005 : Change report file info for FNSUM
!!    Paul W. Eslinger : 18 Jul 2006 : (SCR-1137) Add further irrigation logic
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Make output of SEASON_GROW based
!!                                     on the presence of terrestrial plants
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Output aquatic foods for terrestrial animals
!!                                     and benchmark values
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Revise and format statements
!!
!!********************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Seeds_Mod
      USE Debug_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Threshold_Mod
      USE Stats_Mod, ONLY: BG_STOC_VALU
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IANA ! Analyte looping index
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//24('='),' Echo of the Problem Definition ',24('='))
!
! *** Problem Identification
!
      WRITE(IRPT,1010) TRIM(PTITLE), USRNAM
 1010 FORMAT(/'Title: ',A/'User:  ',A)
!
      WRITE(IRPT,1020) NREAL
 1020 FORMAT(I0,' Realizations requested')
!
! *** Files names used in the analysis
!
      WRITE(IRPT,1030) 'File Name for Input Keyword Data', TRIM(FNKEY)
      WRITE(IRPT,1030) 'File Name for the Report File', TRIM(FNRPT)
      IF( DET_USE  )      WRITE(IRPT,1030) 'File Name for Output Detailed Data',        TRIM(FNDET)
      IF( THS_USE  )      WRITE(IRPT,1030) 'File Name for Output Threshold Statistics', TRIM(FNTHS)
      IF( BG_STOC_VALU  ) WRITE(IRPT,1030) 'File Name for Generated Stochastic Values', TRIM(FNVAL)
      IF( STA_USE )       WRITE(IRPT,1030) 'File Name for Output Summary Statistics',   TRIM(FNSUM)
 1030 FORMAT(/A/'File: ',A)
!
!     Concentration files for each analyte
!
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%COMP ) THEN
          WRITE(IRPT,1040) TRIM(ESD_ANA(IANA)%ID), TRIM(FNCON(IANA))
 1040     FORMAT(/'File Name for Media Concentrations for analyte with ID="',A,'"'/'File: ',A)
        END IF
      END DO
!
! *** Debug flags
!
      WRITE(IRPT,1050)
 1050 FORMAT(/'Debug Flag Information')
!
      IF( BGCONC ) THEN
        WRITE(IRPT,1060) 'Yes', 'Concentration Data'
 1060   FORMAT(3X,A3,' : ',A)
      ELSE
        WRITE(IRPT,1060) 'No ', 'Concentration Data'
      END IF
!
      IF( BGSTOC ) THEN
        WRITE(IRPT,1060) 'Yes', 'Stochastic Calculations'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Stochastic Calculations'
      END IF
!
      IF( BGAIRC ) THEN
        WRITE(IRPT,1060) 'Yes', 'Air Concentration Calculations'
        WRITE(IRPT,1060) 'Yes', 'Air Deposition Calculations'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Air Concentration Calculations'
        WRITE(IRPT,1060) 'No ', 'Air Deposition Calculations'
      END IF
!
      IF( BGCOST ) THEN
        WRITE(IRPT,1060) 'Yes', 'Constant Calculations'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Constant Calculations'
      END IF
!
      IF( BGLOOP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Loop control'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Loop control'
      END IF
!
      IF( BGRADS ) THEN
        WRITE(IRPT,1060) 'Yes', 'Detailed radionuclide sums'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Detailed radionuclide sums'
      END IF
!
      IF( BGSPEC ) THEN
        WRITE(IRPT,1060) 'Yes', 'Species information'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Species information'
      END IF
!
      IF( BGANAL ) THEN
        WRITE(IRPT,1060) 'Yes', 'Analyte information'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Analyte information'
      END IF
!
      IF( BGORQP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OR_QP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OR_QP'
      END IF
!
      IF( BGORTP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OR_TP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OR_TP'
      END IF
!
      IF( BGORQA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OR_QA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OR_QA'
      END IF
!
      IF( BGORTA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OR_TA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OR_TA'
      END IF
!
      IF( BGNRQP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NR_QP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NR_QP'
      END IF
!
      IF( BGNRTP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NR_TP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NR_TP'
      END IF
!
      IF( BGNRQA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NR_QA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NR_QA'
      END IF
!
      IF( BGNRTA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NR_TA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NR_TA'
      END IF
!
      IF( BGOSQP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OS_QP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OS_QP'
      END IF
!
      IF( BGOSTP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OS_TP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OS_TP'
      END IF
!
      IF( BGOSQA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OS_QA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OS_QA'
      END IF
!
      IF( BGOSTA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine OS_TA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine OS_TA'
      END IF
!
      IF( BGNSQP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NS_QP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NS_QP'
      END IF
!
      IF( BGNSTP ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NS_TP'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NS_TP'
      END IF
!
      IF( BGNSQA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NS_QA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NS_QA'
      END IF
!
      IF( BGNSTA ) THEN
        WRITE(IRPT,1060) 'Yes', 'Equations in subroutine NS_TA'
      ELSE
        WRITE(IRPT,1060) 'No ', 'Equations in subroutine NS_TA'
      END IF
!
! *** Output flags for summary statistics
!
      WRITE(IRPT,1070)
 1070 FORMAT(/'Summary Statistics Output Flag Information')
!
      IF( STA_BURDEN ) THEN
        WRITE(IRPT,1110) 'Yes', 'BURDEN', 'Body burden'
 1110   FORMAT(3X,A3,' : ',A,' : ',A)
      ELSE
        WRITE(IRPT,1110) 'No ', 'BURDEN', 'Body burden'
      END IF
!
      IF( STA_EHQ ) THEN
        WRITE(IRPT,1110) 'Yes', 'EHQ   ', 'Environmental hazard quotient '
      ELSE
        WRITE(IRPT,1110) 'No ', 'EHQ   ', 'Environmental hazard quotient'
      END IF
!
      IF( STA_DOSHAZ ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSHAZ', 'Hazardous dose by nuclide'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSHAZ', 'Hazardous dose by nuclide'
      END IF
!
      IF( STA_DOSRAD ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSRAD', 'Radionuclide dose by nuclide'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSRAD', 'Radionuclide dose by nuclide'
      END IF
!
      IF( STA_SUMRAD ) THEN
        WRITE(IRPT,1110) 'Yes', 'SUMRAD', 'Summed radionuclide dose'
      ELSE
        WRITE(IRPT,1110) 'No ', 'SUMRAD', 'Summed radionuclide dose'
      END IF
!
      IF( STA_TFOODS ) THEN
        WRITE(IRPT,1110) 'Yes', 'TFOODS', 'Aquatic foods for terrestrial animals'
      ELSE
        WRITE(IRPT,1110) 'No ', 'TFOODS', 'Aquatic foods for terrestrial animals'
      END IF
!
      IF( STA_DOSDER ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSDER', 'Dermal dose by analyte'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSDER', 'Dermal dose by analyte'
      END IF
!
      IF( STA_DOSING ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSING', 'Ingestion dose by analyte'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSING', 'Ingestion dose by analyte'
      END IF
!
      IF( STA_DOSINH ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSINH', 'Inhalation dose by analyte'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSINH', 'Inhalation dose by nuclide'
      END IF
!
      IF( STA_BMRDOS ) THEN
        WRITE(IRPT,1110) 'Yes', 'BMRDOS', 'Rad dose benchmark'
      ELSE
        WRITE(IRPT,1110) 'No ', 'BMRDOS', 'Rad dose benchmark'
      END IF
!
! *** Output flags for detailed data
!
      WRITE(IRPT,1140)
 1140 FORMAT(/'Detailed Data Output Flag Information')
!
      IF( DET_BURDEN ) THEN
        WRITE(IRPT,1110) 'Yes', 'BURDEN', 'Body burden'
      ELSE
        WRITE(IRPT,1110) 'No ', 'BURDEN', 'Body burden'
      END IF
!
      IF( DET_EHQ ) THEN
        WRITE(IRPT,1110) 'Yes', 'EHQ   ', 'Environmental hazard quotient'
      ELSE
        WRITE(IRPT,1110) 'No ', 'EHQ   ', 'Environmental hazard quotient'
      END IF
!
      IF( DET_DOSHAZ ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSHAZ', 'Hazardous dose by nuclide'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSHAZ', 'Hazardous dose by nuclide'
      END IF
!
      IF( DET_DOSRAD ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSRAD', 'Radionuclide dose by nuclide'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSRAD', 'Radionuclide dose by nuclide'
      END IF
!
      IF( DET_SUMRAD ) THEN
        WRITE(IRPT,1110) 'Yes', 'SUMRAD', 'Summed radionuclide dose'
      ELSE
        WRITE(IRPT,1110) 'No ', 'SUMRAD', 'Summed radionuclide dose'
      END IF
!
      IF( DET_TFOODS ) THEN
        WRITE(IRPT,1110) 'Yes', 'TFOODS', 'Aquatic foods for terrestrial animals'
      ELSE
        WRITE(IRPT,1110) 'No ', 'TFOODS', 'Aquatic foods for terrestrial animals'
      END IF
!
      IF( DET_DOSDER ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSDER', 'Dermal dose by analyte'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSDER', 'Dermal dose by analyte'
      END IF
!
      IF( DET_DOSING ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSING', 'Ingestion dose by analyte'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSING', 'Ingestion dose by analyte'
      END IF
!
      IF( DET_DOSINH ) THEN
        WRITE(IRPT,1110) 'Yes', 'DOSINH', 'Inhalation dose by analyte'
      ELSE
        WRITE(IRPT,1110) 'No ', 'DOSINH', 'Inhalation dose by nuclide'
      END IF
!
      IF( DET_BMRDOS ) THEN
        WRITE(IRPT,1110) 'Yes', 'BMRDOS', 'Rad dose benchmark'
      ELSE
        WRITE(IRPT,1110) 'No ', 'BMRDOS', 'Rad dose benchmark'
      END IF
!
! *** Growing season information
!
      IF( NUM_TP .GT. 0 ) THEN
        WRITE(IRPT,1130) SEASON_START, SEASON_END, SEASON_GROW, IRIG_AMT, IRIG_START
 1130   FORMAT(/'Growing season information for terrestrial plant calulations'/&
              3X,I7,  ' : Day of year (Julian calendar) the growing season starts'/&
              3X,I7,  ' : Day of year (Julian calendar) the growing season ends'/&
              3X,F7.4,' : Fraction of year assigned to the growing season'/&
              3X,F7.4,' : Amount of irrigation water applied in a year (cm)'/&
              3X,I7,  ' : First year for irrigation')
      END IF
!
      RETURN
      END SUBROUTINE ECHO1

      SUBROUTINE ECHO2(  )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine handles writing an echo of the definitions of times, locations,
!!    analytes, species, and optional threshold values to the report file.  The
!!    definition of stochastic variables is deferred to subroutine STOGEN.
!!
!!  History:
!!    Paul W. Eslinger :  5 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 15 Mar 1991 : Version 1.2
!!    Paul W. Eslinger : 27 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 13 Jan 2005 : Change output formats
!!    Paul W. Eslinger : 24 Jan 2005 : Add food consumption calulations
!!                                     for aquatic and terrestrial animals
!!    Paul W. Eslinger :  1 Feb 2006 : Fix index problem before call to REPORT_SPECIES
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Add Kd logic
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1147) Change threshold logic (no EHQ)
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Always put out detailed species information
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Seeds_Mod
      USE Debug_Mod
      USE Species_Mod
      USE Threshold_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ILOC ! Location looping index
      INTEGER :: ISPC ! Species looping index
      INTEGER :: ITIM ! Time looping index
      INTEGER :: ITYP ! Threshold type looping index
      INTEGER :: IDX  ! Index variable
!
      INTEGER :: NUM_THS        ! Number of thresholds
      CHARACTER(LEN=80) :: CTMP ! Temporary output variable
!
!---- First executable code --------------------------------------------
!
! *** Time slices
!
      WRITE(IRPT,1100)
 1100 FORMAT(/'Time Slices Requested: (Index, Calendar Year)')
      DO ITIM = 1, ESD_NUM_TIM
        IF( ESD_TIM(ITIM)%COMP ) THEN
          WRITE(IRPT,1110) ITIM, ESD_TIM(ITIM)%TIME
 1110     FORMAT(3X,I4,' : ',I0)
        END IF
      END DO
      WRITE(IRPT,1120) ECM_NUM_TIM
 1120 FORMAT('A total of ',I0,' times have been requested.')
!
! *** Location definitions
!
      WRITE(IRPT,1000)
 1000 FORMAT(/'Locations Requested: (Index, ID, Type, Name)')
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
        WRITE(IRPT,1010) ILOC, ESD_LOC(ILOC)%ID, ESD_LOC(ILOC)%TYPE, TRIM(ESD_LOC(ILOC)%NAME)
 1010   FORMAT(3X,I4,' : ',A,' : ',A,' : "',A,'"')
        IF( ESD_LOC(ILOC)%SECOND .GT. 0 ) THEN
          IDX = ESD_LOC(ILOC)%SECOND
          WRITE(IRPT,1015) IDX, ESD_LOC(IDX)%ID, ESD_LOC(IDX)%TYPE, TRIM(ESD_LOC(IDX)%NAME)
 1015     FORMAT(10X,'Matched with : ',I4,' : ',A,' : ',A,' : "',A,'"')
        END IF
      END DO
      WRITE(IRPT,1020) ECM_NUM_LOC
 1020 FORMAT('A total of ',I0,' locations have been requested.')
!
! *** Analyte Definitions
!
      WRITE(IRPT,1030)
 1030 FORMAT(/'Analytes Requested:'/ &
        'Index   ID   Element Type Description'/ &
        '----- ------ ------- ---- ',48('-'))
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        WRITE(IRPT,1040) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%ELEM_ID, ESD_ANA(IANA)%TYPE, TRIM(ESD_ANA(IANA)%NAME)
 1040   FORMAT(1X,I4,1X,A,2X,A,2X,A,2X,A)
      END DO
      WRITE(IRPT,1050) ECM_NUM_ANA
 1050 FORMAT('A total of ',I0,' analytes have been requested.')
!
! *** KDSOIL mappings by analyte
!
      IF( NEED_KD ) THEN
        WRITE(IRPT,1180)
 1180   FORMAT(/'Soil-Water KD Mapping',/&
        '  Analyte       KDSOIL ID String'/&
        '  ------------  ----------------------')
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
          IF( ESD_ANA(IANA)%TYPE(1:1) .NE. 'N' ) CYCLE
          IF( ESD_ANA(IANA)%HENRY .LE. 0.0 ) CYCLE
          WRITE(IRPT,1190) IANA, ESD_ANA(IANA)%ID, KD_MAP(IANA)
 1190     FORMAT(2X,I3,': "',A,'"  "',A,'"')
        END DO
      END IF
!
! *** Species Definitions
!
      WRITE(IRPT,1060)
 1060 FORMAT(/ &
        'Species information listed in order of computation'/ &
        'Order Out Type Habitat     ID    Long Name'/ &
        '----- --- ---- --------  ------  ',48('-'))
      DO ISPC = 1, ESD_NUM_SPC
        IDX = ESD_SPC(ISPC)%ORDER
        IF( IDX .LT. 0 ) CYCLE
        IF( .NOT. ESD_SPC(IDX)%COMP ) CYCLE
        IF( ESD_SPC(IDX)%OUTPUT ) THEN
          WRITE(IRPT,1070) IDX, 'Yes', ESD_SPC(IDX)%TYPE, ESD_SPC(IDX)%HABITAT, ESD_SPC(IDX)%ID, TRIM(ESD_SPC(IDX)%NAME)
        ELSE
          WRITE(IRPT,1070) IDX, 'No ', ESD_SPC(IDX)%TYPE, ESD_SPC(IDX)%HABITAT, ESD_SPC(IDX)%ID, TRIM(ESD_SPC(IDX)%NAME)
        END IF
 1070   FORMAT(1X,I4,1X,A,2X,A2,2X,A8,' "',A,'" ',A)
      END DO
      WRITE(IRPT,1090) ECM_NUM_SPC
 1090 FORMAT('A total of ',I0,' species have been requested.')
!
! *** Output species information in an easy-to-read format
!      IF( BGSPEC ) THEN
      WRITE(IRPT,1160)
 1160 FORMAT(/'!',100('-')/'!',15X,'Detailed definition for each species in order of computation'/'!',100('-'))
      DO ISPC = 1, ESD_NUM_SPC
        IDX = ESD_SPC(ISPC)%ORDER
        IF( IDX .LT. 0 ) CYCLE
        IF( .NOT. ESD_SPC(IDX)%COMP ) CYCLE
        CALL REPORT_SPECIES( IDX )
      END DO
!      END IF
!
! *** Threshold Definitions
      NUM_THS = 0
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        IF( THRESHOLD%SREXIST1(ISPC) .OR. THRESHOLD%SREXIST2(ISPC) ) NUM_THS = NUM_THS + 1
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
          DO ITYP = 1, MAXTYP
            IF( THRESHOLD%EXIST1(ISPC,IANA,ITYP) .OR. THRESHOLD%EXIST2(ISPC,IANA,ITYP) ) NUM_THS = NUM_THS + 1
          END DO
        END DO
      END DO
!
      IF( NUM_THS .GT. 0 ) THEN
        WRITE(IRPT,1130)
 1130   FORMAT(/'Thresholds Requested'/ &
          '   Species Analyte  Type    Limit1      Limit2'/ &
          '   ------- -------  ------  ----------  ----------')
        NUM_THS = 0
        DO ISPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
          IF( THRESHOLD%SREXIST1(ISPC) .OR. THRESHOLD%SREXIST2(ISPC) ) THEN
            CTMP = '   ' // ESD_SPC(ISPC)%ID // '   ' // '      ' // '  ' // THRESHLBL(MAXTYP)
            IF( THRESHOLD%SREXIST1(ISPC) ) WRITE(CTMP(29:38),'(1P,E10.3)' ) THRESHOLD%SRLIMIT1(ISPC)
            IF( THRESHOLD%SREXIST2(ISPC) ) WRITE(CTMP(41:50),'(1P,E10.3)' ) THRESHOLD%SRLIMIT2(ISPC)
            WRITE(IRPT,'(A)') TRIM(CTMP)
            NUM_THS = NUM_THS + 1
          END IF
          DO IANA = 1, ESD_NUM_ANA
            IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
            DO ITYP = 1, MAXTYP
              IF( THRESHOLD%EXIST1(ISPC,IANA,ITYP) .OR. THRESHOLD%EXIST2(ISPC,IANA,ITYP) ) THEN
                CTMP = '   ' // ESD_SPC(ISPC)%ID // '   ' // ESD_ANA(IANA)%ID // '  ' // THRESHLBL(ITYP)
                IF( THRESHOLD%EXIST1(ISPC,IANA,ITYP) ) WRITE(CTMP(29:38),'(1P,E10.3)' ) THRESHOLD%LIMIT1(ISPC,IANA,ITYP)
                IF( THRESHOLD%EXIST2(ISPC,IANA,ITYP) ) WRITE(CTMP(41:50),'(1P,E10.3)' ) THRESHOLD%LIMIT2(ISPC,IANA,ITYP)
                WRITE(IRPT,'(A)') TRIM(CTMP)
                NUM_THS = NUM_THS + 1
              END IF
            END DO
          END DO
        END DO
        WRITE(IRPT,1140) NUM_THS
 1140   FORMAT('A total of ',I0,' thresholds have been requested.')
      ELSE
        WRITE(IRPT,1150)
 1150   FORMAT(/'No Thresholds Requested that Match with Requested Species and Analytes')
      END IF
!
      RETURN
      END SUBROUTINE ECHO2

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
!!    Paul W. Eslinger : 14 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IFLG ! Flag for place (begining or end)
      INTEGER, INTENT(IN) :: IRPT ! Unit number for the report file
      INTEGER :: IERR ! Error flag indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'ELAPSE' ! Name of this routine
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
      END SUBROUTINE ELAPSE

      SUBROUTINE ESD_INIT( )
!!*********************************************************************************
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
!!    Paul W. Eslinger : 21 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 20 May 2005 : Add PARTICLE and remove FUNGUS
!!    Paul W. Eslinger : 24 Aug 2005 : Add THROTTLE Logic
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Add Kd logic
!!    Paul W. Eslinger : 14 Sep 2006 : (SCR-1144) Soil ingestion initialization
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!*********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE ESD_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod
      USE ECDA_Mod, ONLY: ECDA_NMED
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no match
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'ESD_INIT' ! Name of this subroutine
!
      INTEGER :: ITIM ! Time looping variable
      INTEGER :: IANA ! Analyte looping variable
      INTEGER :: ILOC ! Location looping variable
      INTEGER :: ISPC ! Species looping variable
      INTEGER :: IMED ! Media looping variable
      INTEGER :: IREL ! Realization looping variable
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
        MESSAG(2) = 'Suggest modifying the ESD TIME keyword, ECOLOGICAL modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      DO ITIM = 1, ESD_NUM_TIM
        ESD_TIM(ITIM)%TIME = 0
        ESD_TIM(ITIM)%COMP = .FALSE.
      END DO
!
! *** Variables for the location data
!
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid value for ESD_NUM_LOC'
        MESSAG(2) = 'Suggest modifying the ESD LOCATION keyword, ECOLOGICAL modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      DO ILOC = 1, ESD_NUM_LOC
        ESD_LOC(ILOC)%ID = ' '
        ESD_LOC(ILOC)%NAME = ' '
        ESD_LOC(ILOC)%TYPE = ' '
        ESD_LOC(ILOC)%COMP = .FALSE.
        ESD_LOC(ILOC)%OUTPUT = .FALSE.
        ESD_LOC(ILOC)%EASTING  = 0.0
        ESD_LOC(ILOC)%NORTHING = 0.0
        ESD_LOC(ILOC)%SECOND = 0
        ESD_LOC(ILOC)%SECOND_ID = ' '
        ESD_LOC(ILOC)%GWAT = .FALSE.
        ESD_LOC(ILOC)%SEEP = .FALSE.
        ESD_LOC(ILOC)%SWAT = .FALSE.
        ESD_LOC(ILOC)%PWAT = .FALSE.
        ESD_LOC(ILOC)%SEDI = .FALSE.
        ESD_LOC(ILOC)%SORP = .FALSE.
        ESD_LOC(ILOC)%SODR = .FALSE.
        ESD_LOC(ILOC)%SOGW = .FALSE.
        ESD_LOC(ILOC)%SOSW = .FALSE.
        ESD_LOC(ILOC)%AIRC = .FALSE.
        ESD_LOC(ILOC)%AIRD = .FALSE.
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
      DO IANA = 1, ESD_NUM_ANA
        ESD_ANA(IANA)%ID = ' '
        ESD_ANA(IANA)%ELEM_ID = ' '
        ESD_ANA(IANA)%NAME = ' '
        ESD_ANA(IANA)%COMP     = .FALSE.
        ESD_ANA(IANA)%THROTTLE = .FALSE.
        ESD_ANA(IANA)%OUTPUT   = .FALSE.
        ESD_ANA(IANA)%HENRY    =  0.0
        ESD_ANA(IANA)%MOLDIFF  =  0.0
        ESD_ANA(IANA)%DFIMM    =  0.0
        ESD_ANA(IANA)%DFSED    =  0.0
        ESD_ANA(IANA)%GAMMA    =  0.0
        ESD_ANA(IANA)%PARTICLE = -1.0
        FNCON(IANA) = ' '
      END DO
!
! *** Variables for the species data
!
      IF( ESD_NUM_SPC .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'Invalid value for ESD_NUM_SPC'
        MESSAG(2) = 'Suggest modifying the ESD SPECIES keywords'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      DO ISPC = 1, ESD_NUM_SPC
        ESD_SPC(ISPC)%ID = ' '
        ESD_SPC(ISPC)%NAME = ' '
        ESD_SPC(ISPC)%HABITAT = ' '
        ESD_SPC(ISPC)%ORDER = -1
        ESD_SPC(ISPC)%COMP     = .FALSE.
        ESD_SPC(ISPC)%INTERNAL = .FALSE.
        ESD_SPC(ISPC)%EMERGENT = .FALSE.
        ESD_SPC(ISPC)%TRANS    = .FALSE.
        ESD_SPC(ISPC)%AE       = -999.0
        ESD_SPC(ISPC)%OCAR     = -999.0
        ESD_SPC(ISPC)%AWD      = -999.0
        ESD_SPC(ISPC)%DIFFHT   = -999.0
        ESD_SPC(ISPC)%ETWATER  = -999.0
        ESD_SPC(ISPC)%FDW      = -999.0
        ESD_SPC(ISPC)%FLIPID   = -999.0
        ESD_SPC(ISPC)%FMR      = -999.0
        ESD_SPC(ISPC)%FOC      = -999.0
        ESD_SPC(ISPC)%FPA      = -999.0
        ESD_SPC(ISPC)%FPL      = -999.0
        ESD_SPC(ISPC)%FPW      = -999.0
        ESD_SPC(ISPC)%FW       = -999.0
        ESD_SPC(ISPC)%FWATER   = -999.0
        ESD_SPC(ISPC)%GE       = -999.0
        ESD_SPC(ISPC)%INHRATE  = -999.0
        ESD_SPC(ISPC)%LOSSRATE = -999.0
        ESD_SPC(ISPC)%PCS      = -999.0
        ESD_SPC(ISPC)%PCW      = -999.0
        ESD_SPC(ISPC)%PSI      = -999.0
        ESD_SPC(ISPC)%RADIUS   = -999.0
        ESD_SPC(ISPC)%RHOP     = -999.0
        ESD_SPC(ISPC)%SA       = -999.0
        ESD_SPC(ISPC)%SADHER   = -999.0
        ESD_SPC(ISPC)%SOILING  =    0.0
        ESD_SPC(ISPC)%SEDING   =    0.0
        ESD_SPC(ISPC)%THETA    = -999.0
        ESD_SPC(ISPC)%WEIGHT   = -999.0
        ESD_SPC(ISPC)%GROWRATE = -999.0
        ESD_SPC(ISPC)%RESPRATE = -999.0
        ESD_SPC(ISPC)%WBMASS   = -999.0
        ESD_SPC(ISPC)%WATERING = -999.0
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
! *** Map and data for Soil Kd's
!
      DO IANA = 1, ESD_NUM_ANA
        KD_UNITS(IANA) = ' '
        KD_MAP(IANA) = ' '
        DO IREL = 1, NREAL
          KD(IANA,IREL) = -1.0
        END DO
      END DO
!
      RETURN
      END SUBROUTINE ESD_INIT

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
!!    Paul W. Eslinger : 22 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 10 Aug 2006 : Add Kdsoil logic
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE ESD_Mod
      USE Radius_Mod
      USE Control_Mod, ONLY: CONVERT_UNITS, CONVERT_LABELS
      USE ECDA_Mod, ONLY: ECDA_NMED
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'ESD_MEMORY' ! Name of this subroutine
      INTEGER :: IERA ! Error status variable from the allocate action
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Check on the number of analytes
      IF( ESD_NUM_ANA .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least 1 analyte required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** ESD_ANA : Top-level for the analyte structure
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
! *** Kd unit by analyte
      ALLOCATE( KD_UNITS(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 17
        MESSAG(1) = 'Error allocating memory for KD_UNITS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Kd mapping by analyte
      ALLOCATE( KD_MAP(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 18
        MESSAG(1) = 'Error allocating memory for KD_MAP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Kd data by analyte and realization
      ALLOCATE( KD(ESD_NUM_ANA,ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating memory for KD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Media concentration files by analyte
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
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'At least 1 location required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** ESD_LOC : Top-level of the location structure
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
! *** Check on the number of species
      IF( ESD_NUM_SPC .LT. 1 ) THEN
        IERR = 7
        MESSAG(1) = 'At least 1 species required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Food concentration files by analyte and species
      ALLOCATE( FNFOD(ESD_NUM_ANA,ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for FNFOD(ESD_NUM_ANA,ESD_NUM_SPC)'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( IFOD(ESD_NUM_ANA,ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for IFOD(ESD_NUM_ANA,ESD_NUM_SPC)'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** ESD_SPC : Top-level of the species structure
      ALLOCATE( ESD_SPC(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error allocating memory for ESD_SPC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the number of times
      IF( ESD_NUM_TIM .LT. 1 ) THEN
        IERR = 11
        MESSAG(1) = 'At least 1 time required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** ESD_TIM : Top-level of the times structure
      ALLOCATE( ESD_TIM(ESD_NUM_TIM), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating memory for ESD_TIM'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Units conversion factors
      ALLOCATE( CONVERT_UNITS(ESD_NUM_ANA,ECDA_NMED), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating memory for CONVERT_UNITS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CONVERT_LABELS(ESD_NUM_ANA,ECDA_NMED,2), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error allocating memory for CONVERT_LABELS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE ESD_MEMORY

      SUBROUTINE EXTRACT_BCFVEG( IANA, ISPC, NUM_BCFVEG, WORK, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine extracts all realizations of a stochastic BCFVEG
!!    variable from the scratch file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Oct 2004 : Original source code
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Debug_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Files_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IANA            ! (Input)  Analyte index
      INTEGER :: ISPC            ! (Input)  Species index
      INTEGER :: NUM_BCFVEG      ! (Input)  Number of BCFVEG stochastic variables
      REAL, DIMENSION(*) :: WORK ! (Output) Work vector of values
      INTEGER :: IERR            ! (Output) Error flag variable
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'EXTRACT_BCFVEG' ! Name of this subroutine
      CHARACTER(LEN=6) :: TMP_ELEM ! Element name from the file
      CHARACTER(LEN=6) :: TMP_SPEC ! Species name from the file
!
      INTEGER :: IREL ! Realization looping index
      INTEGER :: ISTO ! Stochastic variable looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR   = 0
!
      IF( BGSTOC ) THEN
        WRITE(IRPT,1000) CALLER
 1000   FORMAT(/'Entering: ',A)
        WRITE(IRPT,1010) 'Analyte = '//ESD_ANA(IANA)%ID
        WRITE(IRPT,1010) 'Element = '//ESD_ANA(IANA)%ELEM_ID
        WRITE(IRPT,1010) 'Species = '//ESD_SPC(ISPC)%ID
        WRITE(IRPT,1010) 'Values'
 1010   FORMAT(3X,A)
      END IF
!
! *** Read from the scratch file looking for a match
!
      REWIND( IBCF )
!
      DO ISTO = 1, NUM_BCFVEG
        READ(IBCF,*) TMP_ELEM, TMP_SPEC, (WORK(IREL),IREL=1,NREAL)
        IF( TMP_ELEM .NE. ESD_ANA(IANA)%ELEM_ID ) CYCLE
        IF( TMP_SPEC .NE. ESD_SPC(ISPC)%ID ) CYCLE
        IF( BGSTOC ) WRITE(IRPT,1020) (WORK(IREL),IREL=1,NREAL)
 1020                FORMAT(2X,1P,5(1X,E12.5))
        RETURN
      END DO
!
! *** No match was found
!
      IERR = 1
      MESSAG(1) = 'No BCFVEG stochastic variable was found for'
      MESSAG(2) = 'Analyte = '//ESD_ANA(IANA)%ID
      MESSAG(3) = 'Element = '//ESD_ANA(IANA)%ELEM_ID
      MESSAG(4) = 'Species = '//ESD_SPC(ISPC)%ID
      CALL PRTERR( IERR, CALLER, 4 )
!
      RETURN
      END SUBROUTINE EXTRACT_BCFVEG

      SUBROUTINE EXTRACT_TRANSFER( IANA, ISPC, NUM_TRANSFER, WORK, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine extracts all realizations of a stochastic TRANSFER
!!    variable from the scratch file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Oct 2004 : Original source code
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Debug_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Files_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IANA            ! (Input)  Analyte index
      INTEGER :: ISPC            ! (Input)  Species index
      INTEGER :: NUM_TRANSFER    ! (Input)  Number of TRANSFER stochastic variables
      REAL, DIMENSION(*) :: WORK ! (Output) Work vector of values
      INTEGER :: IERR            ! (Output) Error flag variable
!
! *** Local variables
      CHARACTER(LEN=16) :: CALLER = 'EXTRACT_TRANSFER' ! Name of this subroutine
      CHARACTER(LEN=6) :: TMP_ELEM ! Element name from the file
      CHARACTER(LEN=6) :: TMP_SPEC ! Species name from the file
!
      INTEGER :: IREL ! Realization looping index
      INTEGER :: ISTO ! Stochastic variable looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR   = 0
!
      IF( BGSTOC ) THEN
        WRITE(IRPT,1000) CALLER
 1000   FORMAT(/'Entering: ',A)
        WRITE(IRPT,1010) 'Analyte = '//ESD_ANA(IANA)%ID
        WRITE(IRPT,1010) 'Element = '//ESD_ANA(IANA)%ELEM_ID
        WRITE(IRPT,1010) 'Species = '//ESD_SPC(ISPC)%ID
        WRITE(IRPT,1010) 'Values'
 1010   FORMAT(3X,A)
      END IF
!
! *** Read from the scratch file looking for a match
!
      REWIND( ITRN )
!
      DO ISTO = 1, NUM_TRANSFER
        READ(ITRN,*) TMP_ELEM, TMP_SPEC, (WORK(IREL),IREL=1,NREAL)
        IF( TMP_ELEM .NE. ESD_ANA(IANA)%ELEM_ID ) CYCLE
        IF( TMP_SPEC .NE. ESD_SPC(ISPC)%ID ) CYCLE
        IF( BGSTOC ) WRITE(IRPT,1020) (WORK(IREL),IREL=1,NREAL)
 1020                FORMAT(2X,1P,5(1X,E12.5))
        RETURN
      END DO
!
! *** No match was found
!
      IERR = 1
      MESSAG(1) = 'No TRANSFER stochastic variable was found for'
      MESSAG(2) = 'Analyte = '//ESD_ANA(IANA)%ID
      MESSAG(3) = 'Element = '//ESD_ANA(IANA)%ELEM_ID
      MESSAG(4) = 'Species = '//ESD_SPC(ISPC)%ID
      CALL PRTERR( IERR, CALLER, 4 )
!
      RETURN
      END SUBROUTINE EXTRACT_TRANSFER

      SUBROUTINE FIND_RADIUS_ANALYTE( VLABEL, IDXA )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine finds the index of a variable (VLABEL) in the
!!    array of variables input in the radius library.
!!
!!  History:
!!
!!    Paul W. Eslinger :  9 Dec 1997 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!  Call List Variables:
!!
!!    VLABEL : Variable label
!!    IDXV   : Output integer index
!!              0 = No match on the analyte label
!!             >0 = Match found for the analyte label
!!
!!**********************************************************************
!
! *** Global variables
      USE Radius_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDXA ! Index of the analyte
      CHARACTER(LEN=*), INTENT(IN) :: VLABEL ! Label for matching
!
! *** Local variables
      INTEGER :: I ! Looping control variable
!
!---- Executable code ---------------------------------------------------
!
      IDXA = 0
!
! *** Offset by one index for the Radius values in the first position
!
      DO I = 1, (NANA_RLIB+1)
        IF( VLABEL .EQ. RLIBANA(I)%ID ) THEN
          IDXA = I
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE FIND_RADIUS_ANALYTE

      SUBROUTINE FIND_RADIUS_INDEX( ISPC, IDXR, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine finds the index of a variable (VLABEL) in the
!!    array of variables input in the radius library.
!!
!!    The radius information is contained in the first entry of the
!!    RLIBANA%RLIBVAL array.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Dec 1997 : Version 1.0
!!    Paul W. Eslinger : 24 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!  Call List Variables:
!!
!!    ISPC : Species index
!!    IDXR : Output radius integer index
!!              0 = No match on radius
!!             >0 = Match found for the radius
!!
!!**********************************************************************
!
! *** Global valiables
      USE Files_Mod
      USE Radius_Mod
      USE Esd_Mod
      USE Debug_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ISPC
      INTEGER :: IDXR ! Index match for the species
      INTEGER :: IERR ! Error index variable
!
! *** Local variables
      CHARACTER(LEN=17) :: CALLER = 'FIND_RADIUS_INDEX' ! Name of this subroutine
      INTEGER :: I ! Looping control variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
      IDXR = 0
!
      IF( BGCOST ) THEN
        WRITE(IRPT,1000) CALLER
 1000   FORMAT(/'Entering ',A)
      END IF
!
! *** Check on the species index
!
      IF( ISPC.LT.1 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid species index'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      IF( BGCOST ) THEN
        WRITE(IRPT,1010) ISPC, ESD_SPC(ISPC)%ID, ESD_SPC(ISPC)%RADIUS
 1010   FORMAT('Index=',I4,' Species ID=',A,' Species Radius=',1P,E12.5)
      END IF
!
! *** Check on the species radius
!
      IF( ESD_SPC(ISPC)%RADIUS .LE. 0.0 ) THEN
        IERR = 2
        MESSAG(1) = 'Species radius must be greater than 0'
        MESSAG(2) = 'Species ID is '//ESD_SPC(ISPC)%ID
        MESSAG(3) = 'Radius entered was '
        WRITE(MESSAG(3)(19:),*) ESD_SPC(ISPC)%RADIUS
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Map to the ends of the radius information
!
      IF( ESD_SPC(ISPC)%RADIUS .LE. RLIBANA(1)%RLIBVAL(1) ) THEN
        IDXR = 1
        RETURN
      END IF
!
      IF( ESD_SPC(ISPC)%RADIUS .GE. RLIBANA(1)%RLIBVAL(NRAD_RLIB) ) THEN
        IDXR = NRAD_RLIB
        RETURN
      END IF
!
! *** Find the index in the middle of the radius range
!
      DO I = 2, NRAD_RLIB
        IF( ESD_SPC(ISPC)%RADIUS .LE. RLIBANA(1)%RLIBVAL(I) ) THEN
          IDXR = I
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE FIND_RADIUS_INDEX

      SUBROUTINE GEN_BCFVEG( NUM_BCFVEG, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the stochastic
!!    BCFVEG variables and writes them to a scratch file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Oct 2004 : Original source code
!!    Paul W. Eslinger :  6 Dec 2004 : Change length of VNAME to 24
!!    Paul W. Eslinger :  6 Jul 2005 : Add check on organics
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Stats_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: NUM_BCFVEG ! (Output) Number of BCFVEG stochastic variables
      INTEGER :: IERR       ! (Output) Error flag variable
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'GEN_BCFVEG' ! Name of this subroutine
      CHARACTER(LEN=24) :: VNAME ! Variable name (ID) for determining the stochastic distribution
!
      LOGICAL :: ONLY_OR ! Only variables for organics are needed
!
      INTEGER :: IREL ! Realization looping index
      INTEGER :: VIDX ! Stochastic variable index
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ISTO ! Stochastic variable looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR   = 0
!
      IF( BGSTOC ) WRITE(IRPT,1000) CALLER
 1000 FORMAT(/'Entering: ',A)
!
      NUM_BCFVEG = 0
!
! *** Return with no action if no terrestrial plants are used
!
      IF( .NOT. NEED_TP ) RETURN
!
! *** Determine the number of stochastic BCFVEG definitions
!
      DO ISTO = 1, INDSTO
        IF( VLABEL(ISTO)(13:18) .EQ. 'BCFVEG' ) NUM_BCFVEG = NUM_BCFVEG + 1
      END DO
!
! *** Generate the temporary values for BCFVEG (by element)
!
      IF( NUM_BCFVEG .LE. 0 ) THEN
        ONLY_OR   = .TRUE.
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
          IF( ESD_ANA(IANA)%TYPE(1:1) .NE. 'O' ) ONLY_OR = .FALSE.
        END DO
        IF( .NOT.ONLY_OR ) THEN
          IERR = 1
          MESSAG(1) = 'Inconsistent inputs: Terrestrial plants are being computed but no'
          MESSAG(2) = 'stochastic definitions containing BCFVEG were detected.'
          MESSAG(3) = 'Error in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
      END IF
!
! *** Get a unit number for the scratch file
!
      IBCF = GET_UNIT_NUMBER(  )
      IF( IBCF .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for BCFVEG scratch file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open a scratch file
!
      OPEN(IBCF,STATUS='SCRATCH',IOSTAT=IERR)
!      OPEN(IBCF,FILE='SCRATCH_BCFVEG.TXT',IOSTAT=IERR)
      IF( IERR .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Unable to open the BCFVEG scratch file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Write all values to the scratch file
!
      DO ISTO = 1, INDSTO
!       Skip all stochastic variables that are not BCFVEG identifiers
        IF( VLABEL(ISTO)(13:18) .NE. 'BCFVEG' ) CYCLE
!
! ***   Generate the values
        VNAME = VLABEL(ISTO)(1:18)
        VIDX = ISTO
        IF( BGSTOC ) WRITE(IRPT,*) 'Generating variable "' // TRIM(VNAME) // '" with index ', VIDX
        CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
!       Save the generated values in a scratch file
        WRITE(IBCF,1010) TRIM(VNAME(1:6)), TRIM(VNAME(7:12)), (WORK(IREL),IREL=1,NREAL)
1010    FORMAT('"',A,'","',A,'"',1P,500(:,',',E12.5))
      END DO
!
! *** Rewind the file and exit
!
      REWIND( IBCF )
!
      RETURN
      END SUBROUTINE GEN_BCFVEG

      SUBROUTINE GEN_TRANSFER( NUM_TRANSFER, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the stochastic
!!    TRANSFER variables and writes them to a scratch file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Oct 2004 : Original source code
!!    Paul W. Eslinger :  6 Dec 2004 : Change length of VNAME to 24
!!    Paul W. Eslinger : 10 Aug 2006 : Standardize species need flags
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Stats_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: NUM_TRANSFER ! (Output) Number of TRANSFER stochastic variables
      INTEGER :: IERR         ! (Output) Error flag variable
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'GEN_TRANSFER' ! Name of this subroutine
      CHARACTER(LEN=24) :: VNAME ! Variable name (ID) for determining the stochastic distribution
!
      INTEGER :: IREL ! Realization looping index
      INTEGER :: VIDX ! Stochastic variable index
      INTEGER :: ISTO ! Stochastic variable looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR   = 0
!
      IF( BGSTOC ) WRITE(IRPT,1000) CALLER
 1000 FORMAT(/'Entering: ',A)
!
      NUM_TRANSFER = 0
!
! *** Return with no action if no terrestrial animals are used
!
      IF( .NOT. NEED_TA ) RETURN
!
! *** Determine the number of stochastic TRANSFER definitions
!
      DO ISTO = 1, INDSTO
        IF( VLABEL(ISTO)(13:20) .EQ. 'TRANSFER' ) NUM_TRANSFER = NUM_TRANSFER + 1
      END DO
!
! *** Generate the temporary values for TRANSFER (by element)
!     This is an optional parameter, so it may not be defined
!
      IF( NUM_TRANSFER .LE. 0 ) RETURN
!
! *** Get a unit number for the scratch file
!
      ITRN = GET_UNIT_NUMBER(  )
      IF( ITRN .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for TRANSFER scratch file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open a scratch file
!
      OPEN(ITRN,STATUS='SCRATCH',IOSTAT=IERR)
!      OPEN(ITRN,FILE='SCRATCH_TRANSFER.TXT',IOSTAT=IERR)
      IF( IERR .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Unable to open the TRANSFER scratch file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Write all values to the scratch file
!
      DO ISTO = 1, INDSTO
!       Skip all stochastic variables that are not TRANSFER identifiers
        IF( VLABEL(ISTO)(13:20) .NE. 'TRANSFER' ) CYCLE
!
! ***   Generate the values
        VNAME = VLABEL(ISTO)
        VIDX = ISTO
        IF( BGSTOC ) WRITE(IRPT,*) 'Generating variable "' // TRIM(VNAME) // '" with index ', VIDX
        CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
!       Save the generated values in a scratch file
        WRITE(ITRN,1010) TRIM(VNAME(1:6)), TRIM(VNAME(7:12)), (WORK(IREL),IREL=1,NREAL)
1010    FORMAT('"',A,'","',A,'"',1P,500(:,',',E12.5))
      END DO
!
! *** Rewind the file and exit
!
      REWIND( ITRN )
!
      RETURN
      END SUBROUTINE GEN_TRANSFER

      SUBROUTINE GET_CHEM_EFF( ISPC, IANA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates all realizations of the chemical
!!    transfer efficiency for a given species and analyte.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 20 May 2005 : KOW data modified to be log data
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!  Call List Variables:
!!
!!    ISPC : Species index
!!    IANA : Analyte index
!!    IERR : Output integer error flag
!!           0 = No errors
!!          >0 = Terminal error setting data
!!
!!  Notes:
!!
!!    1. The lookup table is hard coded in subroutine INIT
!!
!!    2. The lookup table has a KOW entry and a chemical transfer
!!       efficiency in each row.
!!
!!    3. The code logic assumes that the KOW entries in the table
!!       are entered in strictly increasing order
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Species_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'GET_CHEM_EFF' ! Name of this subroutine
!
      LOGICAL :: FOUND ! Local logical flag
      INTEGER :: IREL  ! Realiztion index
      INTEGER :: ICHM  ! Chemical index
      REAL :: PCT ! Local interpolation variable
      REAL :: DIF ! Local interpolation variable
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      DO IREL = 1, NREAL
!
! ***   Get the KOW and check for an invalid value
!
        IF( ESD_ANA(IANA)%KOW(IREL) .LT. 0.0 ) THEN
          IERR = 1
          MESSAG(1) = 'Invalid KOW (log of octanol/water partition coefficient) - value is negative'
          MESSAG(2) = 'Analyte ' // ESD_ANA(IANA)%ID
          MESSAG(3) = ' '
          WRITE(MESSAG(3)(1:),'(A,1X,I0)') 'Realization number:', IREL
          MESSAG(4) = ' '
          WRITE(MESSAG(4)(1:),'(A,1X,1P,E11.4)') 'Offending Value:', ESD_ANA(IANA)%KOW(IREL)
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
!
! ***   Check for extreme values
!       The convention is to map extreme values to the ends of the table
!
        IF( ESD_ANA(IANA)%KOW(IREL) .LE. CHEM_EFF(1,1) ) THEN
          ESD_SPC(ISPC)%CHEM(IREL) = CHEM_EFF(1,2)
          CYCLE
        END IF
!
        IF( ESD_ANA(IANA)%KOW(IREL) .GE. CHEM_EFF(NUM_CHEM_EFF,1) ) THEN
          ESD_SPC(ISPC)%CHEM(IREL) = CHEM_EFF(NUM_CHEM_EFF,2)
          CYCLE
        END IF
!
! ***   Linear interpolation on the values
!
        FOUND = .FALSE.
        DO ICHM = 2, NUM_CHEM_EFF
          IF( ESD_ANA(IANA)%KOW(IREL).GT.CHEM_EFF(ICHM-1,1) .AND. ESD_ANA(IANA)%KOW(IREL).LE.CHEM_EFF(ICHM,1) ) THEN
            PCT = (ESD_ANA(IANA)%KOW(IREL) - CHEM_EFF(ICHM-1,1)) / (CHEM_EFF(ICHM,1) - CHEM_EFF(ICHM-1,1))
            DIF = CHEM_EFF(ICHM,2) - CHEM_EFF(ICHM-1,2)
            ESD_SPC(ISPC)%CHEM(IREL) = CHEM_EFF(ICHM-1,2) + PCT*DIF
            FOUND = .TRUE.
            EXIT
          END IF
        END DO
!
! ***   The only reason to not find the value is if the table is not
!       entered in ascending order
!
        IF( .NOT.FOUND ) THEN
          IERR = 2
          MESSAG(1) = 'Error in determining the chemical transfer efficiency'
          MESSAG(2) = 'Unexpected error: check the CHEM_EFF table in subroutine INIT'
          MESSAG(3) = 'The table entries must be in ascending order in KOW'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE GET_CHEM_EFF

      SUBROUTINE GET_INDEX( ITIM, ILOC, IMED, IDX, IERR )
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
!!    Paul W. Eslinger : 17 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      INTEGER :: IDX  ! Output record number index
      INTEGER :: IERR ! Error number variable
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'GET_INDEX' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
!
      IERR = 0
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
      IF( IDX .GT. 0 ) RETURN
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
        IF( IDX .GT. 0 ) RETURN
      END IF
!
      RETURN
      END SUBROUTINE GET_INDEX

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
!!    Paul W. Eslinger : 22 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Dec 2004 : Last change to PRGVER and PRGDAT
!!                                     Change date separator characters
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
      USRNAM = 'Anonymous User'
!
! *** Program name and version number
      PRGNAM = 'ECEM'
      PRGVER = '4.00.006'
!
! *** Program date (DD MMM YYYYY)
      PRGDAT = '12 Jun 2014'
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
      END SUBROUTINE IDEN_SET

      SUBROUTINE INIT( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine initializes the global variables of the ECEM software package.
!!
!!  History:
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 1999 : Version 1.1
!!    Paul W. Eslinger : 15 Mar 1999 : Version 1.2
!!    Paul W. Eslinger : 24 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Feb 2002 : Add STA_CONCEN and DET_CONCEN
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 28 Jul 2004 : (SCR-1063) Remove TUPTAKE
!!    Paul W. Eslinger : 13 Jan 2005 : Add riparian/aquatic pairing flag
!!    Paul W. Eslinger : 18 Jul 2006 : (SCR-1137) Add irrigation year
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Add Kd logic, standardize
!!                                     species use flags
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Add rad benchmark solution
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger : 15 Aug 2007 : Add NEED variables for rads, chemicals
!!                                     and irrigation
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!**************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Files_Mod
      USE Stats_Mod
      USE Seeds_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Species_Mod
      USE Threshold_Mod
      USE Esd_Mod
      USE Errors_Mod
      USE Radius_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------
!
! *** File handling
!
      REPORT = .FALSE.
      FNRPT = ' ' ! Ouput report file
!
      FNKEY = ' ' ! Input ECEM keyword file
      FNESD = ' ' ! Input ESD keyword input file
      FNKDS = ' ' ! Input file of Kdsoil data
      FNMAP = ' ' ! Input concentration record number index file
      FNTHS = ' ' ! Output file of threshold statistics
      FNSUM = ' ' ! Output file of summary statistics
      FNDET = ' ' ! Output file of detailed data
      FNVAL = ' ' ! Output file of generated statistical data
!
! *** Seed for random number generator
!
      SDSTOC = 0.0D0
!
! *** Debug flags
!
      BGANAL = .FALSE.
      BGAIRC = .FALSE.
      BGFCDA = .FALSE.
      BGCONC = .FALSE.
      BGCOST = .FALSE.
      BGRADS = .FALSE.
      BGLOOP = .FALSE.
      BGSPEC = .FALSE.
      BGSTOC = .FALSE.
      BGVERB = .FALSE.
      BGORQP = .FALSE.
      BGORTP = .FALSE.
      BGORQA = .FALSE.
      BGORTA = .FALSE.
      BGNRQP = .FALSE.
      BGNRTP = .FALSE.
      BGNRQA = .FALSE.
      BGNRTA = .FALSE.
      BGOSQP = .FALSE.
      BGOSTP = .FALSE.
      BGOSQA = .FALSE.
      BGOSTA = .FALSE.
      BGNSQP = .FALSE.
      BGNSTP = .FALSE.
      BGNSQA = .FALSE.
      BGNSTA = .FALSE.
!
! *** Problem execution and output flags
!
      EXECUT = .FALSE.
!
      STA_BURDEN = .FALSE.
      STA_CONCEN = .FALSE.
      STA_EHQ    = .FALSE.
      STA_DOSRAD = .FALSE.
      STA_DOSDER = .FALSE.
      STA_DOSINH = .FALSE.
      STA_DOSING = .FALSE.
      STA_SUMRAD = .FALSE.
      STA_TFOODS = .FALSE.
      STA_BMRDOS = .FALSE.
      STA_USE    = .FALSE.
!
      DET_BURDEN = .FALSE.
      DET_CONCEN = .FALSE.
      DET_EHQ    = .FALSE.
      DET_DOSRAD = .FALSE.
      DET_DOSDER = .FALSE.
      DET_DOSINH = .FALSE.
      DET_DOSING = .FALSE.
      DET_SUMRAD = .FALSE.
      DET_TFOODS = .FALSE.
      DET_BMRDOS = .FALSE.
      DET_USE    = .FALSE.
!
      NEED_KD   = .FALSE.
      NEED_IRIG = .FALSE.
!
! *** Problem title
!
      PTITLE = ' '
!
! *** Radioactive radius library
!
      NHED_RLIB = 1 ! One line of distances (first in library)
      NANA_RLIB = 0
      NRAD_RLIB = 0
!
! *** Radioactive analytes use information
!
      USE_RADS = .FALSE.
!
! *** Foods related information
!
      FOODS = .FALSE.
      FOODPATH = ' '
      FOODMAP = ' '
!
! *** Species use information
!
      NEED_TP = .FALSE.
      NEED_TA = .FALSE.
      NEED_QA = .FALSE.
      NEED_QP = .FALSE.
!
! *** Threshold information (set to not used)
!
      THS_USE = .FALSE.
!
      THRESHLBL(1) = 'BURDEN'
      THRESHLBL(2) = 'DOSDER'
      THRESHLBL(3) = 'DOSING'
      THRESHLBL(4) = 'DOSINH'
      THRESHLBL(5) = 'DOSRAD'
      THRESHLBL(6) = 'SUMRAD'
!
! *** Flag for required pairing of riparian and aquatic locations
      RIP_AQ_PAIRS = .FALSE.
!
! *** Optional air dilution factor for vapor concentrations from surface water
      VAPORDILUTE = 1.0
      NEED_VAPOR = .FALSE.
!
! *** Constants for terrestrial plants
      AREA    = -1.0
      COWHDFX = -1.0
      EXPOSI  = -1.0
      LENGTH  = -1.0
      RFRAC   = -1.0
!      PW Eslinger - SCR-1063 change TUPTAKE to FW
!      TUPTAKE = 0.9
!
! *** Growing season dates and irrigation amount
      SEASON_START = -1
      SEASON_END   = -1
      IRIG_AMT     = 0.0
      IRIG_START   = -1
      YEAR_START   = -1
!
! *** Define the chemical assimilation efficiency table
!     NUM_CHEM_EFF cannot exceed parameter MAXCHM
!     This table is used in subroutine GET_CHEM_EFF
!
      NUM_CHEM_EFF = 15
!
      CHEM_EFF( 1,1) = 2.0
      CHEM_EFF( 1,2) = 0.025118864
!
      CHEM_EFF( 2,1) = 2.5
      CHEM_EFF( 2,2) = 0.044668359
!
      CHEM_EFF( 3,1) = 3.0
      CHEM_EFF( 3,2) = 0.079432823
!
      CHEM_EFF( 4,1) = 3.5
      CHEM_EFF( 4,2) = 0.141253754
!
      CHEM_EFF( 5,1) = 4.0
      CHEM_EFF( 5,2) =  0.251188643
!
      CHEM_EFF( 6,1) = 4.5
      CHEM_EFF( 6,2) = 0.8
!
      CHEM_EFF( 7,1) = 5.0
      CHEM_EFF( 7,2) = 0.8
!
      CHEM_EFF( 8,1) = 5.5
      CHEM_EFF( 8,2) = 0.8
!
      CHEM_EFF( 9,1) = 6.0
      CHEM_EFF( 9,2) = 0.8
!
      CHEM_EFF(10,1) = 6.5
      CHEM_EFF(10,2) = 0.8
!
      CHEM_EFF(11,1) = 7.0
      CHEM_EFF(11,2) = 0.6
!
      CHEM_EFF(12,1) = 7.5
      CHEM_EFF(12,2) = 0.4
!
      CHEM_EFF(13,1) = 8.0
      CHEM_EFF(13,2) = 0.2
!
      CHEM_EFF(14,1) = 8.5
      CHEM_EFF(14,2) = 0.05
!
      CHEM_EFF(15,1) = 9.0
      CHEM_EFF(15,2) = 0.01
!
! *** Rad benchmarks
      TP_BMR = -1.0
      TA_BMR = -1.0
      QP_BMR = -1.0
      QA_BMR = -1.0
!
! *** Rad benchmark units
      TP_UNITS = 'x'
      TA_UNITS = 'x'
      QP_UNITS = 'x'
      QA_UNITS = 'x'
!
      RETURN
      END SUBROUTINE INIT

      SUBROUTINE KEY_ECEM_1( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the ECEM keyword control
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
!!    Paul W. Eslinger : 21 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 21 Oct 2003 : Move reads on ECEM keywords
!!    Paul W. Eslinger : 28 Jul 2004 : (SCR-1063) Remove TUPTAKE
!!    Paul W. Eslinger : 20 Jan 2004 : Move some reads on ECEM keywords
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Stats_Mod
      USE Debug_Mod
      USE Species_Mod
      USE Seeds_Mod
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
      CHARACTER(LEN=10) :: CALLER = 'KEY_ECEM_1' ! Name of this subroutine
!
      INTEGER :: IDX   ! Temporary index variable
      INTEGER :: ITMP  ! Temporary index variable
!
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary string to use with NXTQOT
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
        IERR = 999
        MESSAG(1) = 'Error in lower level RDBLK routine'
        MESSAG(2) = 'Examine the standard output messages for details'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
!-----------------------------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
          ECM_NUM_ANA = ECM_NUM_ANA + 1
!
!-----------------------------------------------------------------------
        CASE( 'DEBUG' ) ! ===> DEBUG keyword
          IF( CEXIST('AIR') )      BGAIRC = .TRUE.
          IF( CEXIST('ANALYTES') ) BGANAL = .TRUE.
          IF( CEXIST('CONCENTR') ) BGCONC = .TRUE.
          IF( CEXIST('FCDA') )     BGFCDA = .TRUE.
          IF( CEXIST('CONSTANT') ) BGCOST = .TRUE.
          IF( CEXIST('RADSUM') )   BGRADS = .TRUE.
          IF( CEXIST('LOOP') )     BGLOOP = .TRUE.
          IF( CEXIST('SPECIES') )  BGSPEC = .TRUE.
          IF( CEXIST('VERBOSE') )  BGVERB = .TRUE.
          IF( CEXIST('OR_QP') )    BGORQP = .TRUE.
          IF( CEXIST('OR_TP') )    BGORTP = .TRUE.
          IF( CEXIST('OR_QA') )    BGORQA = .TRUE.
          IF( CEXIST('OR_TA') )    BGORTA = .TRUE.
          IF( CEXIST('NR_QP') )    BGNRQP = .TRUE.
          IF( CEXIST('NR_TP') )    BGNRTP = .TRUE.
          IF( CEXIST('NR_QA') )    BGNRQA = .TRUE.
          IF( CEXIST('NR_TA') )    BGNRTA = .TRUE.
          IF( CEXIST('OS_QP') )    BGOSQP = .TRUE.
          IF( CEXIST('OS_TP') )    BGOSTP = .TRUE.
          IF( CEXIST('OS_QA') )    BGOSQA = .TRUE.
          IF( CEXIST('OS_TA') )    BGOSTA = .TRUE.
          IF( CEXIST('NS_QP') )    BGNSQP = .TRUE.
          IF( CEXIST('NS_TP') )    BGNSTP = .TRUE.
          IF( CEXIST('NS_QA') )    BGNSQA = .TRUE.
          IF( CEXIST('NS_TA') )    BGNSTA = .TRUE.
          IF( CEXIST('GENERATE') ) BGSTOC = .TRUE.
!
!-----------------------------------------------------------------------
        CASE( 'DETAILS ' ) ! ===> DETAILS keyword
          IF( CEXIST('BURDEN') ) THEN
            DET_BURDEN   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('CONCEN') ) THEN
            DET_CONCEN   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSING') ) THEN
            DET_DOSING   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSINH') ) THEN
            DET_DOSINH   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSDER') ) THEN
            DET_DOSDER   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSHAZ') ) THEN
            DET_DOSHAZ   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSRAD') ) THEN
            DET_DOSRAD   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('BMTISS') ) THEN
            DET_EHQ      = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('SUMRAD') ) THEN
            DET_SUMRAD   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('TFOODS') ) THEN
            DET_TFOODS   = .TRUE.
            DET_USE      = .TRUE.
          END IF
          IF( CEXIST('BMRDOS') ) THEN
            DET_BMRDOS   = .TRUE.
            DET_USE      = .TRUE.
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'EXECUTE' ) ! ===> EXECUTE keyword
          EXECUT = .TRUE.
!
!-----------------------------------------------------------------------
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
          IF( CEXIST('DETAILS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 12
              MESSAG(1) = 'File name (quote string) not found'
              MESSAG(2) = 'FILE keyword, DETAILS modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FNDET = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('STATISTI') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 13
              MESSAG(1) = 'File name (quote string) not found'
              MESSAG(2) = 'FILE keyword, STATISTI modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FNSUM = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('THRESHOL') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 14
              MESSAG(1) = 'File name (quote string) not found'
              MESSAG(2) = 'FILE keyword, THRESHOL modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FNTHS = TMP_NAME
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'FOODS' ) ! ===> FOODS keyword
!
          FOODS = .TRUE.
!
          IF( CEXIST('PATH') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 17
              MESSAG(1) = 'Path name (quote string) not found'
              MESSAG(2) = 'FOODS keyword, PATH modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FOODPATH = TMP_NAME
            END IF
          ELSE
            IERR = 18
            MESSAG(1) = 'The PATH modifier is required on the FOODS keyword'
            MESSAG(2) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('MAP') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 12
              MESSAG(1) = 'File name (quote string) not found'
              MESSAG(2) = 'FOODS keyword, MAP modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FOODMAP = TMP_NAME
            END IF
          ELSE
            IERR = 19
            MESSAG(1) = 'The MAP modifier is required on the FOODS keyword'
            MESSAG(2) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'GROWTH' ) ! ===> GROWTH keyword
!
! ***     Growth rate multiplier
!
          IF( CEXIST('DELTA') ) THEN
            CALL NXTVAL( IDX, GROWTH_DELTA )
            IF( IDX .EQ. 0 ) THEN
              IERR = 22
              MESSAG(1) = 'Growth DELTA value not found'
              MESSAG(2) = 'GROWTH keyword, DELTA modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Growth rate exponent
!
          IF( CEXIST('BETA') ) THEN
            CALL NXTVAL( IDX, GROWTH_BETA )
            IF( IDX .EQ. 0 ) THEN
              IERR = 23
              MESSAG(1) = 'Growth BETA value not found'
              MESSAG(2) = 'GROWTH keyword, BETA modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
          ECM_NUM_LOC = ECM_NUM_LOC + 1
!
!-----------------------------------------------------------------------
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
          NREAL = VALUE(1)
!
!-----------------------------------------------------------------------
        CASE( 'RESPIRE' ) ! ===> RESPIRE keyword
!
! ***     Respiration rate multiplier
!
          IF( CEXIST('PHI') ) THEN
            CALL NXTVAL( IDX, RESPIRE_PHI )
            IF( IDX .EQ. 0 ) THEN
              IERR = 28
              MESSAG(1) = 'Respiration rate multiplier value not found'
              MESSAG(2) = 'RESPIRE keyword, PHI modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Respiration rate exponent
!
          IF( CEXIST('GAMMA') ) THEN
            CALL NXTVAL( IDX, RESPIRE_GAMMA )
            IF( IDX .EQ. 0 ) THEN
              IERR = 29
              MESSAG(1) = 'Respiration GAMMA value not found'
              MESSAG(2) = 'RESPIRE keyword, GAMMA modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'SEED' ) ! ===> SEED keyword
          SDSTOC = VALUE(1)
!
!-----------------------------------------------------------------------
        CASE( 'STATISTI' ) ! ===> STATISTICS keyword
          IF( CEXIST('BURDEN') ) THEN
            STA_BURDEN   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('CONCEN') ) THEN
            STA_CONCEN   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSING') ) THEN
            STA_DOSING   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSINH') ) THEN
            STA_DOSINH   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSDER') ) THEN
            STA_DOSDER   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSHAZ') ) THEN
            STA_DOSHAZ   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('DOSRAD') ) THEN
            STA_DOSRAD   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('BMTISS') ) THEN
            STA_EHQ      = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('SUMRAD') ) THEN
            STA_SUMRAD   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('TFOODS') ) THEN
            STA_TFOODS   = .TRUE.
            STA_USE      = .TRUE.
          END IF
          IF( CEXIST('BMRDOS') ) THEN
            STA_BMRDOS   = .TRUE.
            STA_USE      = .TRUE.
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
          ECM_NUM_TIM = ECM_NUM_TIM + NVALUE
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
        CASE( 'VARIABLE' ) ! ===> VARIABLE keyword
!
! ***     Tritium uptake
!
!          PW Eslinger - SCR-1063 change TUPTAKE to FW
!          IF( CEXIST('TUPTAKE') ) THEN
!            CALL NXTVAL( IDX, TUPTAKE )
!            IF( IDX .EQ. 0 ) THEN
!              IERR = 42
!              MESSAG(1) = 'Tritium uptake value not found'
!              MESSAG(2) = 'VARIABLE keyword, TUPTAKE modifier'
!              MESSAG(3) = 'Problem in the ECEM keyword file'
!              CALL PRTERR( IERR, CALLER, 3 )
!            END IF
!          END IF
!
! ***     Dilution factor for vapor derived from surface water
          IF( CEXIST('VDILUT') ) THEN
            CALL NXTVAL( IDX, VAPORDILUTE )
            IF( IDX .EQ. 0 ) THEN
              IERR = 243
              MESSAG(1) = 'Dilution factor for vapor calculations not found'
              MESSAG(2) = 'VARIABLE keyword, VDILUT modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Cowherd Function
          IF( CEXIST('COWHDFX') ) THEN
            CALL NXTVAL( IDX, COWHDFX )
            IF( IDX .EQ. 0 ) THEN
              IERR = 43
              MESSAG(1) = 'Cowherd function value not found'
              MESSAG(2) = 'VARIABLE keyword, COWHDFX modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Exposure interval
          IF( CEXIST('EXPOSI') ) THEN
            CALL NXTVAL( IDX, EXPOSI )
            IF( IDX .EQ. 0 ) THEN
              IERR = 44
              MESSAG(1) = 'Exposure interval value not found'
              MESSAG(2) = 'VARIABLE keyword, EXPOSI modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Respirable fraction
          IF( CEXIST('RFRAC') ) THEN
            CALL NXTVAL( IDX, RFRAC )
            IF( IDX .EQ. 0 ) THEN
              IERR = 46
              MESSAG(1) = 'Respirable fraction value not found'
              MESSAG(2) = 'VARIABLE keyword, RFRAC modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Area of contamination
          IF( CEXIST('AREA') ) THEN
            CALL NXTVAL( IDX, AREA )
            IF( IDX .EQ. 0 ) THEN
              IERR = 47
              MESSAG(1) = 'Area of contamination value not found'
              MESSAG(2) = 'VARIABLE keyword, AREA modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Length of side of contaminated area
          IF( CEXIST('LENGTH') ) THEN
            CALL NXTVAL( IDX, LENGTH )
            IF( IDX .EQ. 0 ) THEN
              IERR = 48
              MESSAG(1) = 'Length of side of contaminated area value not found'
              MESSAG(2) = 'VARIABLE keyword, LENGTH modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
! ***     Radiation dose benchmark for terrestrial plants
          IF( CEXIST('TP_BMR') ) THEN
            CALL NXTVAL( IDX, TP_BMR )
            IF( IDX .EQ. 0 ) THEN
              IERR = 148
              MESSAG(1) = 'Rad benchmark value - terrestrial plants'
              MESSAG(2) = 'VARIABLE keyword, TP_BMR modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
          IF( CEXIST('TP_UNITS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 149
              MESSAG(1) = 'Rad benchmark value - terrestrial plants units'
              MESSAG(2) = 'VARIABLE keyword, TP_UNITS modifier - missing units quote string'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            ELSE
              TP_UNITS = TMP_NAME
            END IF
          END IF
!
! ***     Radiation dose benchmark for terrestrial animals
          IF( CEXIST('TA_BMR') ) THEN
            CALL NXTVAL( IDX, TA_BMR )
            IF( IDX .EQ. 0 ) THEN
              IERR = 150
              MESSAG(1) = 'Rad benchmark value - terrestrial animals'
              MESSAG(2) = 'VARIABLE keyword, TA_BMR modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
          IF( CEXIST('TA_UNITS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 151
              MESSAG(1) = 'Rad benchmark value - terrestrial animals units'
              MESSAG(2) = 'VARIABLE keyword, TA_UNITS modifier - missing units quote string'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            ELSE
              TA_UNITS = TMP_NAME
            END IF
          END IF
!
! ***     Radiation dose benchmark for aquatic plants
          IF( CEXIST('QP_BMR') ) THEN
            CALL NXTVAL( IDX, QP_BMR )
            IF( IDX .EQ. 0 ) THEN
              IERR = 152
              MESSAG(1) = 'Rad benchmark value - aquatic plants'
              MESSAG(2) = 'VARIABLE keyword, QP_BMR modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
          IF( CEXIST('QP_UNITS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 153
              MESSAG(1) = 'Rad benchmark value - aquatic plants units'
              MESSAG(2) = 'VARIABLE keyword, QP_UNITS modifier - missing units quote string'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            ELSE
              QP_UNITS = TMP_NAME
            END IF
          END IF
!
! ***     Radiation dose benchmark for aquatic animal
          IF( CEXIST('QA_BMR') ) THEN
            CALL NXTVAL( IDX, QA_BMR )
            IF( IDX .EQ. 0 ) THEN
              IERR = 154
              MESSAG(1) = 'Rad benchmark value - aquatic animals'
              MESSAG(2) = 'VARIABLE keyword, QA_BMR modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END IF
!
          IF( CEXIST('QA_UNITS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 155
              MESSAG(1) = 'Rad benchmark value - aquatic animals units'
              MESSAG(2) = 'VARIABLE keyword, QA_UNITS modifier - missing units quote string'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
            ELSE
              QA_UNITS = TMP_NAME
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE DEFAULT ! Ignore other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE KEY_ECEM_1

      SUBROUTINE KEY_ECEM_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!    This subroutine handles reading and storing all inputs from the
!!    keyword control file.
!!
!!  Call List Variables:
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!    CEXIST, PRTERR, SDECOD, and all RDBLK related routines
!!
!!  History:
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 1999 : Version 1.1
!!    Paul W. Eslinger : 15 Mar 1999 : Version 1.2
!!    Paul W. Eslinger :  4 Feb 2002 : Add STA_CONCEN and DET_CONCEN
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS values
!!                                     Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 21 Oct 2003 : Move reads on ECEM keywords
!!    Paul W. Eslinger : 19 Jan 2004 : Add ELEMENT modifier on ANALYTE keyword
!!    Paul W. Eslinger : 20 Jan 2004 : Move some reads on ECEM keywords
!!    Paul W. Eslinger : 26 Oct 2004 : Add RETURN on an error trap
!!    Paul W. Eslinger :  6 Dec 2004 : Change length on CTMP from 20 to 24
!!    Paul W. Eslinger : 13 Jan 2005 : Add riparian/aquatic location pair check
!!    Paul W. Eslinger : 24 Aug 2005 : Add THROTTLE logic
!!    Paul W. Eslinger : 18 Jul 2006 : (SCR-1137) Remove SEDING override
!!    Paul W. Eslinger : 17 Aug 2006 : (SCR-1140) Add Kdsoil logic
!!                                     and debug logic
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Threshold_Mod
      USE Stats_Mod
      USE Species_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL ::  CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
!
      CHARACTER(LEN=10) :: CALLER = 'KEY_ECEM_2' ! Name of this subroutine
!
      CHARACTER(LEN=LENCRD) :: TITLE ! Title (keyword line) from RDBLK
      CHARACTER(LEN=24) :: CTMP ! Temporary ID for stochastic variables
!
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary variable for file names
      CHARACTER(LEN=LENQQQ) :: CQTMP    ! Temporary variable for quote strings
      CHARACTER(LEN=LENQQQ) :: CMES     ! Temporary variable for quote strings
      CHARACTER(LEN=LENQQQ) :: LOC_ID_2 ! Temporary variable for quote strings
      CHARACTER(LEN=LENQQQ) :: TMP_KD   ! Temporary variable for quote strings
      CHARACTER(LEN=6) :: SHORT_ID, PREY_ID
!
      LOGICAL :: TRUNC ! Truncation flag for SDECOD
!
      INTEGER :: ONE  ! The number 1
      INTEGER :: ITIM ! Local time index
      INTEGER :: IDXT ! Local time index
      INTEGER :: IDXA ! Local analyte index
      INTEGER :: IDXS ! Local analyte index
      INTEGER :: IANA ! Local analyte index
      INTEGER :: ISPC ! Local speciesindex
      INTEGER :: IDXL ! Local location index
      INTEGER :: IDX  ! Local index
      INTEGER :: IDX2 ! Local index
      INTEGER :: ITYP ! Local index
      INTEGER :: I    ! Local index
!
      REAL :: RTMP, TMPV ! Temporary variable
      INTEGER :: TEMPTIME ! Temporary time variable
      INTEGER :: NPREY ! Number of prey species for a predator
      INTEGER :: IDXQ, IDXN, IPREY, IQ, IP, IDXPS
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
      ILINE = 0
!
! *** Reset counters set in KEY_ECEM_1 for dimensioning purposes
      INDSTO = 0
      INDTBL = 0
      ECM_NUM_LOC = 0
      ECM_NUM_SPC = 0
      ECM_NUM_TIM = 0
!
! *** Rewind the keyword file after it was used by KEY_ECEM_1
      REWIND( IKEY )
!
! *** Set some local useage
      ONE = 1
      CTMP = 'No quotes entered'
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
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
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_ANA( SHORT_ID, IDXA )
            IF( IDXA .GT. 0 ) THEN
              ESD_ANA(IDXA)%COMP = .TRUE.
            ELSE
              IERR = 1
              MESSAG(1) = 'Analyte requested that is not in the master list'
              MESSAG(2) = 'Analyte ID is '// SHORT_ID
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'ID modifier not entered on the ANALYTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('ELEMENT') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(IDXA)%ELEM_ID = CQTMP
            ELSE
              IERR = 42
              MESSAG(1) = 'Quote string for analyte element ID was not found'
              MESSAG(2) = 'Analyte ID is '// ESD_ANA(IDXA)%ID
              MESSAG(3) = 'Problem on the ANALYTE keyword in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 43
            MESSAG(1) = 'ELEMENT modifier not entered on the ANALYTE keyword'
            MESSAG(2) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('PARTICLE') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 44
              MESSAG(1) = 'Particulate fraction value not found'
              MESSAG(2) = 'ANALYTE keyword, PARTICLE modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              MESSAG(4) = 'Change the ANALYTE keyword'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            ELSE
              ESD_ANA(IDXA)%PARTICLE = RTMP
            END IF
          ELSE
            IERR = 45
            MESSAG(1) = 'PARTICLE modifier not entered on the ANALYTE keyword'
            MESSAG(2) = 'Problem in the ECEM keyword file'
            MESSAG(3) = 'Analyte ID is '// ESD_ANA(IDXA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          IF( CEXIST('OUTPUT') ) THEN
            ESD_ANA(IDXA)%OUTPUT = .TRUE.
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'CONSUME' ) ! ===> CONSUME keyword
!
          IF( CEXIST('ID') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            IF( IDX .GT. 0 ) THEN
              SHORT_ID = CQTMP
            ELSE
              IERR = 3
              MESSAG(1) = 'Consume predator ID string not found'
              MESSAG(2) = 'Problem in the ECEM keyword file'
              MESSAG(3) = 'Change the CONSUME keyword'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 4
            MESSAG(1) = 'ID modifier not entered on the CONSUME keyword'
            MESSAG(2) = 'Problem in the ECEM keyword file'
            MESSAG(3) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Match with a species in the list
!
          CALL MATCH_SPC( SHORT_ID, IDXS )
          IF( IDXS .LT. 1 ) THEN
            IERR = 5
            MESSAG(1) = 'Specie requested that is not in the master list'
            MESSAG(2) = 'Specie ID is '// SHORT_ID
            MESSAG(3) = 'Problem in the ECEM keyword file'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Optional sediment ingestion
!
          IF( CEXIST('SEDING') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 6
              MESSAG(1) = 'Sediment ingestion value not found'
              MESSAG(2) = 'CONSUME keyword, SEDING modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              MESSAG(4) = 'Change the CONSUME keyword'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            ELSE
              ESD_SPC(IDXS)%SEDING = RTMP
            END IF
          END IF
!
!         Optional soil ingestion
!
          IF( CEXIST('SOILING') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 7
              MESSAG(1) = 'Soil ingestion value not found'
              MESSAG(2) = 'CONSUME keyword, SOILING modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              MESSAG(4) = 'Change the CONSUME keyword'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            ELSE
              ESD_SPC(IDXS)%SOILING = RTMP
            END IF
          END IF
!
!         Optional prey consumption ingestion
!
          IF( CEXIST('PREY') ) THEN
            NPREY = NQUOTE - 1
            CALL NXTQOT( IDXQ, CQTMP )
            IF( IDXQ .LT. 1 ) THEN
              IERR = 8
              MESSAG(1) = 'No prey ID strings were found'
              MESSAG(2) = 'CONSUME keyword, PREY modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              MESSAG(4) = 'ID was '//SHORT_ID
              MESSAG(5) = 'Change the CONSUME keyword'
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
            CALL NXTVAL( IDXN, RTMP )
            IF( IDXN .EQ. 0 ) THEN
              IERR = 9
              MESSAG(1) = 'Prey consumption fraction not found'
              MESSAG(2) = 'CONSUME keyword, PREY modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              MESSAG(4) = 'Change the CONSUME keyword'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
            IF( NVALUE .LT. (IDXN+NPREY-1) ) THEN
              IERR = 10
              MESSAG(1) = 'Not enough prey consumption fractions entered'
              MESSAG(2) = 'CONSUME keyword, PREY modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              MESSAG(4) = 'Change the CONSUME keyword'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
            IQ = IDXQ - 1
            IP = IDXN - 1
            DO IPREY = 1, NPREY
              IQ = IQ + 1
              PREY_ID = QUOTE(IQ)
              CALL MATCH_SPC( PREY_ID, IDXPS )
              IF( IDXPS .LT. 1 ) THEN
                IERR = 11
                MESSAG(1) = 'Prey species requested that is not in the master list'
                MESSAG(2) = 'Predator species ID is '// SHORT_ID
                MESSAG(3) = 'Prey species ID is '// PREY_ID
                MESSAG(4) = 'Problem in the ECEM keyword file'
                MESSAG(5) = 'Change the CONSUME keyword'
                CALL PRTERR( IERR, CALLER, 5 )
                RETURN
              END IF
              IP = IP + 1
              PREDATE(IDXS,IDXPS) = VALUE(IP)
!             Check for riparian to aquatic food web crossover
              IF( ESD_SPC(IDXS)%TYPE.EQ.'TA' .AND. ESD_SPC(IDXS)%HABITAT.EQ.'RIPARIAN' ) THEN
                IF( ESD_SPC(IDXPS)%TYPE.EQ.'QA' .OR. ESD_SPC(IDXPS)%TYPE.EQ.'QP' ) RIP_AQ_PAIRS = .TRUE.
              END IF
            END DO
!
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'DEBUG' ) ! ===> DEBUG keyword
!         These debug values required after stochastic variables initialized
          IF( CEXIST('GENERATE') ) THEN
            BG_STOC_DEFN = .TRUE.
            BG_STOC_STAT = .TRUE.
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IKEY )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('VALUES') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 11
              MESSAG(1) = 'File name (quote string) not found'
              MESSAG(2) = 'FILE keyword, VALUES modifier'
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FNVAL = TMP_NAME
              BG_STOC_VALU = .TRUE.
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'THROTTLE' ) ! ===> THROTTLE keyword
!
          IF( NQUOTE .GT. 0 ) THEN
            DO I = 1, NQUOTE
              SHORT_ID = QUOTE(I)
              CALL MATCH_ANA( SHORT_ID, IDXA )
              IF( IDXA .GT. 0 ) ESD_ANA(IDXA)%THROTTLE = .TRUE.
            END DO
          ELSE
            IERR = 42
            MESSAG(1) = 'THROTTLE keyword does not contain any quote strings'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'KDSOIL' ) ! ===> KDSOIL keyword
!
          IF( CEXIST('ID') ) THEN
            CALL NXTQOT( IDX, TMP_KD )
            IF( IDX .LE. 0 ) THEN
              IERR = 7
              MESSAG(1) = 'ID modifier on the KDSOIL keyword is missing the quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 8
            MESSAG(1) = 'Every KDSOIL keyword must also have the ID modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('ANALYTE') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_ANA( SHORT_ID, IDXA )
!           Store data only for well defined analytes
            IF( IDXA .GT. 0 ) KD_MAP(IDXA) = TRIM(TMP_KD)
          ELSE
            IERR = 6
            MESSAG(1) = 'Every KDSOIL keyword must have the ANALYTE modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
!
          ECM_NUM_LOC = ECM_NUM_LOC + 1
!
          IF( CEXIST('PRIMARY') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_LOC( SHORT_ID, IDXL )
            IF( IDXL .GT. 0 ) THEN
              ESD_LOC(IDXL)%COMP = .TRUE.
            ELSE
              IERR = 24
              MESSAG(1) = 'Location requested that is not in the master list'
              MESSAG(2) = 'Location ID is ' // SHORT_ID
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 25
            MESSAG(1) = 'Every LOCATION card must have the PRIMARY modifier'
            MESSAG(2) = 'Location card number XXXX is missing the modifier'
            WRITE(MESSAG(2)(22:25),'(I4)') ECM_NUM_LOC
            MESSAG(3) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          IF( CEXIST('OUTPUT') ) THEN
            ESD_LOC(IDXL)%OUTPUT = .TRUE.
          END IF
!
          IF( CEXIST('SECOND') ) THEN
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 26
              MESSAG(1) = 'The SECOND modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(CQTMP)
              MESSAG(4) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            ELSE
              SHORT_ID = LOC_ID_2
              CALL MATCH_LOC( SHORT_ID, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%SECOND = IDX2
                ESD_LOC(IDXL)%SECOND_ID = SHORT_ID
              ELSE
                IERR = 27
                MESSAG(1) = 'Secondary location is not in the master list'
                MESSAG(2) = 'Location and secondary IDs are: '//TRIM(CQTMP)//' and '//TRIM(LOC_ID_2)
                MESSAG(3) = 'Problem in the ECEM keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
!
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'SPECIES' ) ! ===> SPECIES keyword
!
          ECM_NUM_SPC = ECM_NUM_SPC + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            IDXS = -1
            CALL MATCH_SPC( SHORT_ID, IDXS )
            IF( IDXS .GT. 0 ) THEN
              ESD_SPC(IDXS)%COMP = .TRUE.
            ELSE
              IERR = 30
              MESSAG(1) = 'Species requested that is not in the master list'
              MESSAG(2) = 'Species ID is '// SHORT_ID
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 31
            MESSAG(1) = 'ID modifier not entered on the SPECIES keyword'
            MESSAG(2) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Check for optional detailed output
!
          IF( CEXIST('OUTPUT') ) THEN
            ESD_SPC(IDXS)%OUTPUT = .TRUE.
          END IF
!
! ***     Check for optional source of plant concentrations (internal or combined)
!
          IF( CEXIST('INTERNAL') ) THEN
            ESD_SPC(IDXS)%INTERNAL = .TRUE.
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'STATISTI' ) ! ===> STATISTICS keyword
          IF( CEXIST('DEFINE') )   BG_STOC_DEFN = .TRUE.
          IF( CEXIST('GENERATE') ) BG_STOC_STAT = .TRUE.
!
!-----------------------------------------------------------------------
        CASE( 'STOCHAST' ) ! ===> STOCHASTIC keyword
!
!         Force the user to enter at least one quote string
          IF( NQUOTE .LT. 1 ) THEN
            IERR = 33
            MESSAG(1) = 'One or two quote strings required (STOCHASTIC Keyword)'
            MESSAG(2) = 'Previous stochastic variable entered was "' // TRIM(CTMP) // '"'
            MESSAG(3) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Store the quote strings and decode the numerical parameters
!
          CTMP = QUOTE(1)
          IF( NQUOTE .GT. 1 ) THEN
            CMES = QUOTE(2)
          ELSE
            CMES = 'No statistical distribution description entered'
          END IF
          IF( CEXIST('TRUNCATE') ) THEN
            TRUNC = .TRUE.
          ELSE
            TRUNC = .FALSE.
          END IF
          CALL SDECOD( CTMP, CMES, ONE, TRUNC, VALUE, NVALUE, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error for variable ' // CTMP
            MESSAG(2) = 'Problem in the ECEM keyword file'
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
                ECM_NUM_TIM = ECM_NUM_TIM + 1
              ELSE
                IERR = 34
                MESSAG(1) = 'Time requested is not in the master list'
                MESSAG(2) = 'Invalid time is '
                WRITE(MESSAG(2)(17:),*) TEMPTIME
                MESSAG(3) = 'Problem in the ECEM keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END DO
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'THRESHOL' ) ! ===> THRESHOLD keyword
!
! ***     General approach is to ignore thresholds for species and analyte
!         combinations that are not defined at the point in the input file
!         that the THRESHOLD card appears
!
! ***     Get the type label (ID)
!
          IF( CEXIST('TYPE    ') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 35
              MESSAG(1) = 'No quote string with the TYPE modifier on the THRESHOLD card'
              MESSAG(2) = 'Title: '//TITLE(1:65)
              MESSAG(3) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 36
            MESSAG(1) = 'TYPE modifier missing on THRESHOLD card'
            MESSAG(2) = 'Title: '//TITLE(1:65)
            MESSAG(3) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!          WRITE(IRPT,*) ' THRESHOLD, TYPE=',TRIM(CQTMP)
          ITYP = 0
          DO I = 1, MAXTYP
            IF( TRIM(CQTMP) .EQ. THRESHLBL(I) ) THEN
              ITYP = I
              EXIT
            END IF
          END DO
!
!          WRITE(IRPT,*) ' THRESHOLD, ITYP=',ITYP
          IF( ITYP .EQ. 0 ) THEN
            IERR = 37
            MESSAG(1) = 'No type name match on the THRESHOLD card'
            MESSAG(2) = 'Enter BURDEN, DOSDER, DOSING, DOSINH, DOSRAD, BMTISS, or SUMRAD'
            MESSAG(3) = 'Title: '//TITLE(1:65)
            MESSAG(4) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
! ***     Get the species label (ID)
!
          IF( CEXIST('SPECIES') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 38
              MESSAG(1) = 'No quote string with the SPECIES modifier on the THRESHOLD card'
              MESSAG(2) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 39
            MESSAG(1) = 'SPECIES modifier missing on THRESHOLD card'
            MESSAG(2) = 'Problem in the ECEM keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     See if the species has been defined and save index in ISPC on match
!
!         WRITE(IRPT,*) ' THRESHOLD, SPECIE=',TRIM(CQTMP)
!          IF( ESD_NUM_SPC .LT. 1 ) GO TO 10
!
          SHORT_ID = CQTMP
          CALL MATCH_SPC( SHORT_ID, ISPC )
!         WRITE(IRPT,*) ' THRESHOLD, ISPC=',ISPC
          IF( ISPC .LE. 0 ) GO TO 10
          IF( .NOT.ESD_SPC(ISPC)%COMP ) GO TO 10
!
! ***     Check for SUMRAD type limit (don't need analyte ID)
!
          IF( THRESHLBL(ITYP) .EQ. 'SUMRAD' ) THEN
!
! ***       Get the first limit (optional)
!
            IF( CEXIST('LIMIT1') ) THEN
              CALL NXTVAL( IDX, TMPV )
!              WRITE(IRPT,*) ' THRESHOLD, SRLIMIT1=',TMPV
              IF( IDX .GT. 0 ) THEN
                THRESHOLD%SREXIST1(ISPC) = .TRUE.
                THRESHOLD%SRLIMIT1(ISPC) = TMPV
                THS_USE = .TRUE.
              END IF
            END IF
!
! ***       Get the second limit (optional)
!
            IF( CEXIST('LIMIT2') ) THEN
              CALL NXTVAL( IDX, TMPV )
!             WRITE(IRPT,*) ' THRESHOLD, SRLIMIT2=',TMPV
              IF( IDX .GT. 0 ) THEN
                THRESHOLD%SREXIST2(ISPC) = .TRUE.
                THRESHOLD%SRLIMIT2(ISPC) = TMPV
                THS_USE = .TRUE.
              END IF
            END IF
!
          ELSE
!
! ***       Get the analyte label (ID)
!
            IF( CEXIST('ANALYTE ') ) THEN
              CALL NXTQOT( IDX, CQTMP )
              IF( IDX .EQ. 0 ) THEN
                IERR = 40
                MESSAG(1) = 'No quote string with the ANALYTE modifier on the THRESHOLD card'
                MESSAG(2) = 'Problem in the ECEM keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            ELSE
              IERR = 41
              MESSAG(1) = 'ANALYTE modifier missing on THRESHOLD card'
              MESSAG(2) = 'Problem in the ECEM keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
! ***       See if the analyte has been defined and save index in IANA on match
!
!            IF( ECM_NUM_ANA .LT. 1 ) GO TO 10
!
            SHORT_ID = CQTMP
            CALL MATCH_ANA( SHORT_ID, IANA )
!           WRITE(IRPT,*) ' THRESHOLD, IANA=',IANA
            IF( IANA .LE. 0 ) GO TO 10
            IF( .NOT.ESD_ANA(IANA)%COMP ) GO TO 10
!
! ***       Get the first limit (optional)
!
            IF( CEXIST('LIMIT1  ') ) THEN
              CALL NXTVAL( IDX, TMPV )
!              WRITE(IRPT,*) ' THRESHOLD, LIMIT1=',TMPV
              IF( IDX .GT. 0 ) THEN
                THRESHOLD%EXIST1(ISPC,IANA,ITYP) = .TRUE.
                THRESHOLD%LIMIT1(ISPC,IANA,ITYP) = TMPV
                THS_USE = .TRUE.
              END IF
            END IF
!
! ***       Get the second limit (optional)
!
            IF( CEXIST('LIMIT2  ') ) THEN
              CALL NXTVAL( IDX, TMPV )
!             WRITE(IRPT,*) ' THRESHOLD, LIMIT2=',TMPV
              IF( IDX .GT. 0 ) THEN
                THRESHOLD%EXIST2(ISPC,IANA,ITYP) = .TRUE.
                THRESHOLD%LIMIT2(ISPC,IANA,ITYP) = TMPV
                THS_USE = .TRUE.
              END IF
            END IF
!
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
      END SUBROUTINE KEY_ECEM_2

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
!!    Paul W. Eslinger : 14 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE Radius_Mod
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
      CHARACTER :: TITLE*(LENCRD) ! Title line from RDBLK
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
      ILINE = 0
!
! *** Top of loop on reading keyword cards
!
   10 CONTINUE
!
      CALL RDBLK( IESD, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
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
        CASE( 'RADIUS' ) ! ===> RADIUS Keyword
          NRAD_RLIB = NVALUE
          NANA_RLIB = NANA_RLIB + 1
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
        CASE DEFAULT ! Ignore other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit the subroutine routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE KEY_ESD_1

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
!!    Paul W. Eslinger : 12 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger : 15 Apr 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 18 Nov 2004 : Add IRIG_AMT
!!    Paul W. Eslinger : 11 Jan 2005 : Move RADIUS to KEY_ESD_3
!!    Paul W. Eslinger : 18 Jul 2006 : (SCR-1137) Add irrigation year and
!!                                     simulation start year & PERIOD
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Add Kdsoil logic
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE Radius_Mod
      USE Control_Mod
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
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_2' ! Name of this subroutine
!
      REAL :: RTMP ! Temporary real variable
      INTEGER :: IDX, ISPC, ITMP, IDXA ! Temporary index variables
      CHARACTER :: TITLE*(LENCRD)      ! Title line from RDBLK
!
!     Temporary strings to match with RDBLK quote strings
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_ANA
      CHARACTER(LEN=LENQQQ) :: TMP_NAME
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
      ILINE = 0
!
! *** Reinitialize counters
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
        IERR = 999
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
              CALL UPCASE( TMP_NAME )
              ESD_ANA(ESD_NUM_ANA)%TYPE = TMP_NAME
!              IF( ESD_ANA(ESD_NUM_ANA)%TYPE(2:2) .EQ. 'R' ) USE_RADS = .TRUE.
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
          IF( CEXIST('DFIMM ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%DFIMM = RTMP
            ELSE
              IERR = 7
              MESSAG(1) = 'Location DFIMM modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('DFSED ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%DFSED = RTMP
            ELSE
              IERR = 8
              MESSAG(1) = 'Location DFSED modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('GAMMA ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%GAMMA = RTMP
            ELSE
              IERR = 9
              MESSAG(1) = 'Location GAMMA modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('HENRY ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%HENRY = RTMP
            ELSE
              IERR = 10
              MESSAG(1) = 'Location HENRY modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('MOLDIFF ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%MOLDIFF = RTMP
            ELSE
              IERR = 11
              MESSAG(1) = 'Location MOLDIFF modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          REWIND( IESD )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('C_ECDA') ) THEN
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 12
                MESSAG(1) = 'NAME modifier missing quote string on FILE (C_ECDA) keyword'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
            ELSE
              IERR = 13
              MESSAG(1) = 'NAME modifier not found on FILE card for type C_ECDA'
              MESSAG(4) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            IF( CEXIST('ANALYTE ') ) THEN
              CALL NXTQOT( IDX, TMP_ANA )
              CALL MATCH_ANA( TMP_ANA, IDXA )
              IF( IDXA .GT. 0 ) THEN
                FNCON(IDXA) = TMP_ID
              ELSE
                IERR = 14
                MESSAG(1) = 'Analyte requested is not yet entered with an ANALYTE keyword'
                MESSAG(2) = 'Analyte name is '//TRIM(TMP_ANA)
                MESSAG(3) = 'Put the FILE keywords for concentrations after the ANALYTE keywords'
                MESSAG(4) = 'In the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              END IF
            ELSE
              IERR = 15
              MESSAG(1) = 'ANALYTE modifier not found on FILE card for type C_ECDA'
              MESSAG(4) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('I_ECDA') ) THEN
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 16
                MESSAG(1) = 'NAME modifier missing quote string on FILE (I_ECDA) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              ELSE
                FNMAP = TMP_ID
              END IF
            ELSE
              IERR = 17
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
                IERR = 75
                MESSAG(1) = 'NAME modifier missing quote string on FILE (KDSOIL) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              ELSE
                FNKDS = TMP_ID
              END IF
            ELSE
              IERR = 76
              MESSAG(1) = 'NAME modifier not found on FILE card for type KDSOIL'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'IRRIGATE' )
!
          IF( CEXIST('SPRING') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 18
              MESSAG(1) = 'Growing season spring start day not found'
              MESSAG(2) = 'IRRIGATE keyword, SPRING modifier'
              MESSAG(3) = 'Problem in the SOIL keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            SEASON_START = RTMP
          ELSE
            IERR = 19
            MESSAG(1) = 'The SPRING modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the SOIL keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('FALL') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 20
              MESSAG(1) = 'Growing season fall end day not found'
              MESSAG(2) = 'IRRIGATE keyword, FALL modifier'
              MESSAG(3) = 'Problem in the SOIL keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            SEASON_END = RTMP
          ELSE
            IERR = 21
            MESSAG(1) = 'The FALL modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the SOIL keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('RATE') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 22
              MESSAG(1) = 'Irrigation rate not found'
              MESSAG(2) = 'IRRIGATE keyword, RATE modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            IRIG_AMT = RTMP
          ELSE
            IERR = 23
            MESSAG(1) = 'The RATE modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('START') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 48
              MESSAG(1) = 'Irrigation start year not found'
              MESSAG(2) = 'IRRIGATE keyword, START modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            IRIG_START = RTMP
          ELSE
            IERR = 49
            MESSAG(1) = 'The START modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
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
              IERR = 24
              MESSAG(1) = 'Location ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 25
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
              IERR = 26
              MESSAG(1) = 'Location NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 27
            MESSAG(1) = 'NAME modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!          IF( CEXIST('EASTING ') ) THEN
!            CALL NXTVAL( IDX, RTMP )
!            IF( IDX .GT. 0 ) THEN
!              ESD_LOC(ESD_NUM_LOC)%EASTING = RTMP
!            ELSE
!              IERR = 28
!              MESSAG(1) = 'Location EASTING modifier missing value'
!              CALL PRTERR( IERR, CALLER, 1 )
!              RETURN
!            END IF
!          ELSE
!            IERR = 29
!            MESSAG(1) = 'EASTING modifier not entered on the LOCATION keyword'
!            CALL PRTERR( IERR, CALLER, 1 )
!            RETURN
!          END IF
!
!          IF( CEXIST('NORTHING ') ) THEN
!            CALL NXTVAL( IDX, RTMP )
!            IF( IDX .GT. 0 ) THEN
!              ESD_LOC(ESD_NUM_LOC)%NORTHING = RTMP
!            ELSE
!              IERR = 30
!              MESSAG(1) = 'Location NORTHING modifier missing value'
!              CALL PRTERR( IERR, CALLER, 1 )
!              RETURN
!            END IF
!          ELSE
!            IERR = 31
!            MESSAG(1) = 'NORTHING modifier not entered on the LOCATION keyword'
!            CALL PRTERR( IERR, CALLER, 1 )
!            RETURN
!          END IF
!
          IF( CEXIST('GWAT') ) ESD_LOC(ESD_NUM_LOC)%GWAT = .TRUE.
          IF( CEXIST('SEEP') ) ESD_LOC(ESD_NUM_LOC)%SEEP = .TRUE.
          IF( CEXIST('SWAT') ) ESD_LOC(ESD_NUM_LOC)%SWAT = .TRUE.
          IF( CEXIST('PWAT') ) ESD_LOC(ESD_NUM_LOC)%PWAT = .TRUE.
          IF( CEXIST('SEDI') ) ESD_LOC(ESD_NUM_LOC)%SEDI = .TRUE.
          IF( CEXIST('SORP') ) ESD_LOC(ESD_NUM_LOC)%SORP = .TRUE.
          IF( CEXIST('SODR') ) ESD_LOC(ESD_NUM_LOC)%SODR = .TRUE.
          IF( CEXIST('SOGW') ) ESD_LOC(ESD_NUM_LOC)%SOGW = .TRUE.
          IF( CEXIST('SOSW') ) ESD_LOC(ESD_NUM_LOC)%SOSW = .TRUE.
          IF( CEXIST('AIRC') ) ESD_LOC(ESD_NUM_LOC)%AIRC = .TRUE.
          IF( CEXIST('AIRD') ) ESD_LOC(ESD_NUM_LOC)%AIRD = .TRUE.
!
          IF( CEXIST('TYPE') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              CALL UPCASE( TMP_ID )
              ESD_LOC(ESD_NUM_LOC)%TYPE = TRIM(TMP_ID(1:8))
            ELSE
              IERR = 32
              MESSAG(1) = 'TYPE modifier missing quote string on the LOCATION keyword'
              MESSAG(2) = 'Modify the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 33
            MESSAG(1) = 'TYPE modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          CALL SET_LOCATIONS( IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'PERIOD' ) ! ====> PERIOD keyword
!
          IF( CEXIST('START   ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              YEAR_START = RTMP
            ELSE
              IERR = 50
              MESSAG(1) = 'The START modifier is missing an associated value'
              MESSAG(2) = 'Modify the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 51
            MESSAG(1) = 'The START modifier was not entered on the PERIOD keyword'
            MESSAG(2) = 'Modify the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
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
              IERR = 37
              MESSAG(1) = 'Species ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 38
            MESSAG(1) = 'ID modifier not entered on the SPECIES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Ensure the species ID is unique
          DO ISPC = 1, ESD_NUM_SPC-1
            IF( ESD_SPC(ESD_NUM_SPC)%ID .EQ. ESD_SPC(ISPC)%ID ) THEN
              IERR = 39
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
              IERR = 40
              MESSAG(1) = 'Species NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 41
            MESSAG(1) = 'NAME modifier not entered on the SPECIES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('TYPE    ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              CALL UPCASE( TMP_ID )
              ESD_SPC(ESD_NUM_SPC)%TYPE = TMP_ID
            ELSE
              IERR = 42
              MESSAG(1) = 'Species TYPE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 43
            MESSAG(1) = 'TYPE modifier not entered on the SPECIES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('HABITAT') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            CALL UPCASE( TMP_ID )
            IF( IDX .GT. 0 ) THEN
              IF( TMP_ID.EQ.'AQUATIC' .OR. TMP_ID.EQ.'RIPARIAN' .or. TMP_ID.EQ.'UPLAND' ) THEN
                ESD_SPC(ESD_NUM_SPC)%HABITAT = TMP_ID
              ELSE
                IERR = 44
                MESSAG(1) = 'Invalid HABITAT string encountered'
                MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
                MESSAG(3) = 'Expecting AQUATIC or RIPARIAN or UPLAND, obtained '//TRIM(TMP_ID)
                MESSAG(4) = 'Problem in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              END IF
            ELSE
              IERR = 45
              MESSAG(1) = 'No quote string with the HABITAT modifier on the SPECIES keyword'
              MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
              MESSAG(3) = 'Expecting AQUATIC or RIPARIAN or UPLAND'
              MESSAG(4) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            IERR = 46
            MESSAG(1) = 'HABITAT modifier not entered on the SPECIES keyword'
            MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
            MESSAG(3) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          CALL SET_SPECIES( IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
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
            IERR = 47
            MESSAG(1) = 'No numeric values found on the TIMES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE DEFAULT ! Ignore other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE KEY_ESD_2

      SUBROUTINE KEY_ESD_3( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading the environmental settings
!!    keyword control information for RADIUS keywords.
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
!!    Paul W. Eslinger : 11 Jan 2005 : Split out from KEY_ESD_2
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE Radius_Mod
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number (0=no errors)
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_3' ! Name of this subroutine
      INTEGER :: J, IDX           ! Temporary indices
!
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
!
!     Temporary character string to match with a RDBLK quote string
      CHARACTER(LEN=LENQQQ) :: TMP_ID
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
      NANA_RLIB   = 0 ! Number of analytes (+1 here) in the radius library
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------C
!
   10 CONTINUE
!
      CALL RDBLK( IESD, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
!-----------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IESD )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'RADIUS' ) ! ===> RADIUS Keyword
!
          NANA_RLIB = NANA_RLIB + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              RLIBANA(NANA_RLIB)%ID = TRIM(TMP_ID)
              IF( NANA_RLIB.EQ.1 .AND. RLIBANA(NANA_RLIB)%ID.NE.'Radius' ) THEN
                IERR = 1
                MESSAG(1) = 'The radius information must be in the first line in the radius library'
                MESSAG(2) = 'The first RADIUS keyword must contain the ID "Radius"'
                MESSAG(3) = 'The ID found was "'//TRIM(RLIBANA(NANA_RLIB)%ID)//'"'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
              DO J = 1, NRAD_RLIB
                RLIBANA(NANA_RLIB)%RLIBVAL(J) = VALUE(J)
              END DO
            ELSE
              IERR =2
              MESSAG(1) = 'RADIUS ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 3
            MESSAG(1) = 'ID modifier not entered on the RADIUS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE DEFAULT ! Ignore other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE KEY_ESD_3

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
!!    LABEL : Input  - Character - Analyte ID
!!    IDX   : Output - Integer   - Index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
      INTEGER :: IDX
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
      END SUBROUTINE MATCH_ANA

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
!!    LABEL : Input  - Character - Location ID
!!    IDX   : Output - Integer   - Iindex for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 15 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
      INTEGER :: IDX
!
! *** Local variables
      INTEGER :: ILOC ! Looping variable
!
!---- First executable code --------------------------------------------
!
      IDX = -1
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( ESD_LOC(ILOC)%ID .EQ. LABEL ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE MATCH_LOC

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
!!    LABEL : Input  - Character - Species label
!!    IDX   : Output - Integer   - Index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
      INTEGER :: IDX
!
! *** Local variables
      INTEGER :: ISPC ! Looping variable
!
!---- First executable code --------------------------------------------
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
      END SUBROUTINE MATCH_SPC

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
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      INTEGER :: TIME
      INTEGER :: IDX
!
! *** Local variables
      INTEGER :: ITIM ! Looping variable
!
!---- First executable code --------------------------------------------
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
      END SUBROUTINE MATCH_TIM

      SUBROUTINE MEM_ECEM_ANA( IANA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine allocates memory for one element of the
!!    ESD_ANA analyte type.
!!
!!  History:
!!
!!    Paul W. Eslinger : 24 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 May 2002 : Clean up error numbers
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 25 Oct 2004 : Change BCFVEG to have plant and
!!                                     analyte dependency
!!    Paul W. Eslinger : 18 Nov 2004 : Add KLIWET and KLIDRY
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_mod
      USE Esd_mod
      USE Control_mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IANA ! Species index
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'MEM_ECEM_ANA' ! Name of this subroutine
      INTEGER :: IERA ! Local error number
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      IF( IANA.LT.1 .OR. IANA.GT.ESD_NUM_ANA ) THEN
        IERR = 1
        MESSAG(1) = 'Analyte index out of range'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ESD_ANA(IANA)%KOW(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating memory for KOW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE MEM_ECEM_ANA

      SUBROUTINE MEM_ECEM_SPC( ISPC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine allocates memory for one element of the
!!    ESD_SPC species type.
!!
!!  History:
!!
!!    Paul W. Eslinger : 24 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 25 Oct 2004 : Change BCFVEG
!!    Paul W. Eslinger : 26 Oct 2004 : Add TRANSVAL
!!    Paul W. Eslinger : 18 Nov 2004 : Add BIOMASS, IFRACDRY, IFRACWET,
!!                                     LWEATHDRY, LWEATHWET, remove FOLIAR
!!    Paul W. Eslinger :  6 Dec 2004 : Change dependencies for IFRACDRY,
!!                                     IFRACWET, LWEATHDRY, and LWEATHWET.
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_mod
      USE Esd_mod
      USE Control_mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ISPC ! Species index
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'MEM_ECEM_SPC' ! Name of this subroutine
      INTEGER :: IERA ! Local error number
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      IF( ISPC.LT.1 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
        IERR = 1
        MESSAG(1) = 'Species index out of range'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Dependence on species and realization
!
      ALLOCATE( ESD_SPC(ISPC)%EFFENG(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating memory for EFFENG'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%CHEM(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating memory for CHEM'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%NIRTOT(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for NIRTOT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%BPORE(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating memory for BPORE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%FABOVE(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for FABOVE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%KPS1(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error allocating memory for KPS1'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%KPA2(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for KPA2'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%BODYBURDEN(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for BODYBURDEN'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%BIOMASS(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error allocating memory for BIOMASS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Dependence on species, analyte, and realization
!
      ALLOCATE( ESD_SPC(ISPC)%BMTISS(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 11
        MESSAG(1) = 'Error allocating memory for BMTISS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%BCF(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating memory for BCF'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%BCFVEG(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error allocating memory for BCFVEG'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%TRANSVAL(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 14
        MESSAG(1) = 'Error allocating memory for TRANSVAL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%ALPHAIJ(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating memory for ALPHAIJ'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%DEPRATE(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error allocating memory for DEPRATE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%METBLOSS(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 17
        MESSAG(1) = 'Error allocating memory for METBLOSS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%ALPHAING(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 18
        MESSAG(1) = 'Error allocating memory for ALPHAING'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%ALPHAPAR(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 19
        MESSAG(1) = 'Error allocating memory for ALPHAPAR'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%ALPHAVAP(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 20
        MESSAG(1) = 'Error allocating memory for ALPHAVAP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%ALPHADW(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 21
        MESSAG(1) = 'Error allocating memory for ALPHADW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%ALPHADS(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 22
        MESSAG(1) = 'Error allocating memory for ALPHADS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%KLIWET(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 23
        MESSAG(1) = 'Error allocating memory for KLIWET'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%KLIDRY(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 24
        MESSAG(1) = 'Error allocating memory for KLIDRY'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%IFRACDRY(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 25
        MESSAG(1) = 'Error allocating memory for IFRACDRY'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%IFRACWET(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 2612
        MESSAG(1) = 'Error allocating memory for IFRACWET'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%LWEATHDRY(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 27
        MESSAG(1) = 'Error allocating memory for LWEATHDRY'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ESD_SPC(ISPC)%LWEATHWET(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 28
        MESSAG(1) = 'Error allocating memory for LWEATHWET'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE MEM_ECEM_SPC

      SUBROUTINE NR_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Radioactive
!!      Species: Aquatic, Animal
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  3 Jun 2002 : Change sediment ingestion equation.
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 28 Jul 2004 : SCR-1063 Adjust for tritium
!!                                     actually being water
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 Modify equations
!!    Paul W. Eslinger : 15 Aug 2006 : (SCR-1140) Reformat outputs
!!                                     Simplify the equations for food ingestion
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS ! Logical whether skip all data outputs
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NR_QA' ! Name of this subroutine
      CHARACTER(LEN=6) :: LOC_ID ! Local location ID
!
      INTEGER :: IREL ! Realization index
      INTEGER :: NREC ! Record number for writing to the foods file
      INTEGER :: JSPC ! Species index
!
      REAL :: DINT       ! Internal dose
      REAL :: DIMM       ! Immersion dose
      REAL :: DSED       ! Dose from sediment exposure
      REAL :: DSED_ABOVE ! Dose from sediment exposure - above
      REAL :: DSED_BELOW ! Dose from sediment exposure - below
      REAL :: CFELEV     ! Conversion factor for elevation (sediment dose)
!
      REAL :: TMPING  ! Body burden from food ingestion
      REAL :: TMPWAT  ! Body burden from water exposure
      REAL :: TMPSED  ! Body burden from sediment ingestion
      REAL :: TMPAIR  ! Body burden from air inhalation
!
      REAL :: IIJ    ! Temporary variable in ingestion equations (feeding rate)
      REAL :: BIOACC ! Temporary bioaccumulation value in ingestion equations
!
      LOGICAL :: EAT_FOOD ! Local flag whether this animal eats food
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGNRQA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Radioactive  '/ &
          'Species : ',A,' : Aquatic, Animal         '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
      FORALL(IREL=1:NREAL) FWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
!     Determine whether this animal eats any food.  This calculation doesn't depend on any specific
!     realizations and simply makes it easier to write more elegant debug statements
      EAT_FOOD = .FALSE.
      DO JSPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) EAT_FOOD = .TRUE.
      END DO
!
! *** Start calculations depending on realizations
      DO IREL = 1, NREAL
!
!       Body burden due to water exposure
        TMPWAT = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
                 (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * ESD_SPC(ISPC)%BCF(IANA,IREL)
!
!       Body burden from air inhalation
        IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
          TMPAIR = (EC(IREL)%AIRC+EC(IREL)%VAPOR)*ESD_SPC(ISPC)%INHRATE / (ESD_SPC(ISPC)%WBMASS/ESD_SPC(ISPC)%AWD)
          TMPAIR = TMPAIR * ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / (ESD_SPC(ISPC)%DEPRATE(IANA,IREL)+ESD_SPC(ISPC)%GROWRATE)
        ELSE
          TMPAIR = 0.0
        END IF
!
!       Initialize food ingestion and sediment ingestion body burdens
        TMPING = 0.0
        TMPSED = 0.0
!
!       Skip a lot of calculations if this animal doesn't eat any food
        IF( EAT_FOOD ) THEN
!
!         Debug outputs
          IF( BGNRQA ) THEN
            WRITE(IRPT,1010)
 1010       FORMAT(/'   Food intake calculations for all prey ...'/&
                    '   IIJ = Feeding rate (g prey dry weight/g predator dry weight/day)'/ &
              '   Number  Name      PREDATE     ALPHAIJ    GROWRATE    RESPRATE        OCAR', &
              '       FOC-J       FOC-I   DEPRATE    BioFactor      IIJ'/ &
              '   ------ ------ ----------- ----------- ----------- ----------- -----------', &
              ' ----------- ----------- ----------- ----------- -----------')
          END IF
!
!         Food and sediment ingestion calculations
          BIOACC = 0.0
          DO JSPC = 1, ESD_NUM_SPC
!
!           Skip species not computed or not eaten by the predator
            IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
            IF( .NOT.(PREDATE(ISPC,JSPC).GT.0.0) ) CYCLE
!
!           Feeding rate (g prey dry weight/g predator dry weight/d)
            IIJ = ( ESD_SPC(ISPC)%GROWRATE+ESD_SPC(ISPC)%RESPRATE ) / ESD_SPC(ISPC)%OCAR * &
              ( ESD_SPC(ISPC)%FOC / ESD_SPC(JSPC)%FOC )
!
!           Accumulation factor for prey body burden
            BIOACC = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL) * IIJ / &
                ( ESD_SPC(ISPC)%DEPRATE(IANA,IREL) + ESD_SPC(ISPC)%GROWRATE )
!
!           Increment the ingestion and sediment body burdens
            TMPING = TMPING + BIOACC * ESD_SPC(JSPC)%BODYBURDEN(IREL)
            TMPSED = TMPSED + BIOACC * (EC(IREL)%SEDI * ESD_SPC(ISPC)%SEDING)
!
!           Debug outputs
            IF( BGNRQA ) &
              WRITE(IRPT,1020) JSPC, ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL), &
                ESD_SPC(ISPC)%GROWRATE, ESD_SPC(ISPC)%RESPRATE, ESD_SPC(ISPC)%OCAR, &
                ESD_SPC(JSPC)%FOC, ESD_SPC(ISPC)%FOC, ESD_SPC(ISPC)%DEPRATE(IANA,IREL), BIOACC, IIJ
 1020         FORMAT(3X,I6,1P,1X,A,10(1X,E11.4))
!
          END DO
!
        END IF
!
!       Total body burden (water exposure + food ingestion + sediment ingestion + air inhalation)
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = TMPWAT + TMPING + TMPSED + TMPAIR
!
!       Convert Body Burden to Concentration (special case for tritium)
        IF( ESD_ANA(IANA)%ID .EQ. 'H3' ) THEN
          CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * (1.0 - 1.0/ESD_SPC(ISPC)%AWD)
        ELSE
          CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%AWD
        END IF
!
!       Save concentration for terrestrial consumption
        CING(ISPC,IREL) = CWORK(IREL)
!
!       Save concentration for human consumption (wet weight basis)
        FWORK(IREL) = CWORK(IREL)
!
! ***   Calculate radiation dose
!       Internal radiation dose (units of rad/day)
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from water immersion (units of rad/day)
        DIMM = (ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
           (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT) * ESD_ANA(IANA)%DFIMM * QIMMFAC
!
!       Radiation dose from sediment exposure (rad/day)
        CFELEV = 0.8*ESD_SPC(ISPC)%BPORE(IREL) + 0.2
        DSED_ABOVE = EC(IREL)%SEDI * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDSF * CFSWD
        DSED_BELOW = EC(IREL)%SEDI * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSED = DSED_ABOVE + DSED_BELOW
!
!       Total radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSED
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / QA_BMR
!
!       Debug outputs
        IF( BGNRQA ) THEN
          WRITE(IRPT,1030) IREL, EC(IREL)%PWAT, EC(IREL)%SWAT, EC(IREL)%SEDI, ESD_SPC(ISPC)%BPORE(IREL), &
            ESD_SPC(ISPC)%BCF(IANA,IREL), ESD_SPC(ISPC)%SEDING, TMPWAT, TMPING, TMPSED, TMPAIR, ESD_SPC(ISPC)%BODYBURDEN(IREL), &
            ESD_SPC(ISPC)%AWD, CWORK(IREL), FWORK(IREL), ESD_SPC(ISPC)%EFFENG(IANA), DINT, ESD_ANA(IANA)%DFIMM, &
            DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_ANA(IANA)%DFSED, DSED_ABOVE, ESD_ANA(IANA)%GAMMA, &
            DSED_BELOW, DSED, RDWORK(IREL)
 1030     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units               : Variable           : Description'/ &
            3X,12('-'),3X,19('-'),3X,18('-'),3X,40('-')  /&
            3X, E12.5,' : pCi/L               : EC%PWAT            : Pore water concentration'/ &
            3X, E12.5,' : pCi/L               : EC%SWAT            : Surface water concentration'/ &
            3X, E12.5,' : pCi/kg              : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : unitless            : ESD_SPC%BPORE      : Relative exposure to pore water'/ &
            3X, E12.5,' : L/kg dry            : ESD_SPC%BCF        : Bioconcentration factor from water for contaminant'/ &
            3X, E12.5,' : unitless            : ESD_SPC%SEDING     : Diet fraction from sediment'/ &
            3X, E12.5,' : pCi/kg dry          : TMPWAT             : Water exposure part of body burden'/ &
            3X, E12.5,' : pCi/kg dry          : TMPING             : Food ingestion part of body burden'/ &
            3X, E12.5,' : pCi/kg dry          : TMPSED             : Sediment ingestion part of body burden'/ &
            3X, E12.5,' : pCi/kg dry          : TMPAIR             : Inhalation part of body burden'/ &
            3X, E12.5,' : pCi/kg dry          : ESD_SPC%BODYBURDEN : Body burden'/ &
            3X, E12.5,' : g wet/g dry         : ESD_SPC%AWD        : Wet to dry weight ratio'/ &
            3X, E12.5,' : pCi/kg wet          : CWORK              : Concentration consumed by terrestrial animals'/ &
            3X, E12.5,' : pCi/kg wet          : FWORK              : Concentration consumed by human'/ &
            3X,12('-'),3X,19('-'),3X,18('-'),3X,40('-')  /&
            3X, E12.5,' : MeV/disintegration  : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X, E12.5,' : rad/day             : DINT               : Radiation dose from internal contamination'/ &
            3X, E12.5,' : mrad/yr per uCi/m^3 : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X, E12.5,' : rad/day             : DIMM               : Radiation dose from water immersion'/ &
            3X, E12.5,' : unitless            : CFELEV             : Conversion factor based on time in pore water'/ &
            3X, E12.5,' : unitless            : FABOVE             : Fraction time spent above sediment'/ &
            3X, E12.5,' : Sv-m^3/sec-Bq       : ESD_ANA%DFSED      : Dose factor for external exposure to sediment'/ &
            3X, E12.5,' : rad/day             : DSED_ABOVE         : Radiation dose from sediment - above'/ &
            3X, E12.5,' : MeV/nt              : ESD_ANA%GAMMA      : Gamma (photon) energy'/ &
            3X, E12.5,' : rad/day             : DSED_BELOW         : Radiation dose from sediment - below'/ &
            3X, E12.5,' : rad/day             : DSED               : Total radiation dose from sediment'/ &
            3X, E12.5,' : rad/day             : RDWORK             : Total radiation dose')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1040) QA_BMR, EHQ(IREL)
 1040     FORMAT(1P, &
            3X, E12.5,' : rad/day             : QA_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless            : EHQ                : Environmental hazard quotient')
!
          IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
            WRITE(IRPT,1070) EC(IREL)%AIRC,EC(IREL)%VAPOR,ESD_SPC(ISPC)%INHRATE,ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL)
 1070       FORMAT(&
            3X,ES12.5,' : pCi/m^3             : EC%AIRC            : Air concentration'/ &
            3X,ES12.5,' : pCi/m^3             : EC%VAPOR           : Vapor concentration'/ &
            3X,ES12.5,' : m^3/day             : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
            3X,ES12.5,' : unitless            : ESD_SPC%ALPHAVAP   : Vapor inhalation assimilation efficiency')
          END IF
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGNRQA .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_RAD_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD',UNITS_RAD_DOS_AQ,NREAL,RDWORK)
        IF( DET_BMRDOS) CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'BURDEN',UNITS_RAD_BUR_AQ,CVEC )
      END IF
!
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSRAD', UNITS_RAD_DOS_AQ, RDWORK )
!
      IF( DET_TFOODS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_RAD_BUR_TER, CWORK )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the concentration (as consumed) to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold limits)
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_RAD_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_RAD_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_RAD_DOS_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_RAD_DOS_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NR_QA

      SUBROUTINE NR_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Radioactive
!!      Species: Aquatic, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 28 Jul 2004 : SCR-1063 Adjust for tritium
!!                                     actually being water
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 Modify equations
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Change to EHQ & reformat
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS ! Logical whether skip all data outputs
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NR_QP' ! Name of this subroutine
      INTEGER :: IREL    ! Realization index
      INTEGER :: NREC    ! Record number for writing to the foods file
!
      REAL :: DINT       ! Internal dose
      REAL :: DIMM       ! Immersion dose
      REAL :: DSED       ! Total sediment dose
      REAL :: DSED_ABOVE ! Dose from sediment - above
      REAL :: DSED_BELOW ! Dose from sediment - below
      REAL :: CFELEV     ! Conversion factor for elevation
!
      REAL :: TMPCR      ! Temporary body burden
!
      CHARACTER(LEN=6) :: LOC_ID ! Local ID for locations
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGNRQP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Radioactive '/ &
          'Species : ',A,' : Aquatic, Plant          '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
!
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) FWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
!       (SCR-1063) Adjust for tritium in the water of the plant (tritium is water)
        IF( ESD_ANA(IANA)%ID .EQ. 'H3' ) THEN
!         Body burden (for consumption by aquatic species)
          TMPCR  = ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT
          ESD_SPC(ISPC)%BODYBURDEN(IREL) = TMPCR * (1.0 - 1.0/ESD_SPC(ISPC)%AWD)
!         Concentration for consumption by terrestrial species
          CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        ELSE
!         Body burden (for consumption by aquatic species)
          TMPCR  = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
            (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * ESD_SPC(ISPC)%BCF(IANA,IREL)
          ESD_SPC(ISPC)%BODYBURDEN(IREL) = TMPCR
!         Concentration for consumption by terrestrial species
          CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%AWD
        END IF
!
!       Save concentration for consumption by terrestrial species
        CPAT(ISPC,IREL) = CWORK(IREL)
!
!       Save food concentration for human consumption (wet weight basis)
        FWORK(IREL) = CWORK(IREL)
!
!       Radiation dose from internal contamination
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from water immersion
        DIMM = (ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
           (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT) * ESD_ANA(IANA)%DFIMM * QIMMFAC
!
!       Radiation dose from sediment (above and below compartments)
        CFELEV = 0.8*ESD_SPC(ISPC)%BPORE(IREL) + 0.2
        DSED_ABOVE = EC(IREL)%SEDI * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDSF * CFSWD
        DSED_BELOW = EC(IREL)%SEDI * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSED = DSED_ABOVE + DSED_BELOW
!
!       Total Radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSED
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / QP_BMR
!
        IF( BGNRQP ) THEN
!
          WRITE(IRPT,1010) IREL, EC(IREL)%PWAT, EC(IREL)%SWAT, EC(IREL)%SEDI, ESD_SPC(ISPC)%AWD, ESD_SPC(ISPC)%BPORE(IREL),&
            ESD_SPC(ISPC)%BCF(IANA,IREL), ESD_SPC(ISPC)%BODYBURDEN(IREL), CWORK(IREL), FWORK(IREL), ESD_SPC(ISPC)%EFFENG(IANA), &
            DINT, ESD_ANA(IANA)%DFIMM, DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_ANA(IANA)%DFSED, DSED_ABOVE, &
            ESD_ANA(IANA)%GAMMA, DSED_BELOW, DSED, RDWORK(IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units               : Variable           : Description'/ &
            3X,12('-'),3X,19('-'),3X,18('-'),3X,50('-')  /&
            3X, E12.5,' : pCi/L               : EC%PWAT            : Pore water concentration'/ &
            3X, E12.5,' : pCi/L               : EC%SWAT            : Surface water concentration'/ &
            3X, E12.5,' : pCi/kg              : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : g wet/g dry         : ESD_SPC%AWD        : Wet to dry weight ratio'/ &
            3X, E12.5,' : unitless            : ESD_SPC%BPORE      : Relative exposure to pore water'/ &
            3X, E12.5,' : L/kg dry            : ESD_SPC%BCF        : Bioconcentration factor for contaminant'/ &
            3X, E12.5,' : pCi/kg dry          : ESD_SPC%BODYBURDEN : Body burden (consumed by aquatic animals)'/ &
            3X, E12.5,' : pCi/kg wet          : CWORK              : Concentration consumed by terrestrial animals'/ &
            3X, E12.5,' : pCi/kg wet          : FWORK              : Concentration consumed by human'/ &
            3X,12('-'),3X,19('-'),3X,18('-'),3X,50('-')  /&
            3X, E12.5,' : MeV/disintegration  : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X, E12.5,' : rad/day             : DINT               : Radiological dose from internal contamination'/ &
            3X, E12.5,' : mrad/yr per uCi/m^3 : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X, E12.5,' : rad/day             : DIMM               : Radiological dose from water immersion'/ &
            3X, E12.5,' : unitless            : CFELEV             : Conversion factor based on time in pore water'/ &
            3X, E12.5,' : unitless            : FABOVE             : Fraction time spent above sediment'/ &
            3X, E12.5,' : Sv-m^3/sec-Bq       : ESD_ANA%DFSED      : Dose factor for external exposure to sediment'/ &
            3X, E12.5,' : rad/day             : DSED_ABOVE         : Radiation dose from sediment - above'/ &
            3X, E12.5,' : MeV/nt              : ESD_ANA%GAMMA      : Gamma (photon) energy'/ &
            3X, E12.5,' : rad/day             : DSED_BELOW         : Radiation dose from sediment - below'/ &
            3X, E12.5,' : rad/day             : DSED               : Total radiation dose from sediment'/ &
            3X, E12.5,' : rad/day             : RDWORK             : Total radiation dose')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1020) QP_BMR, EHQ(IREL)
 1020     FORMAT(1P, &
            3X, E12.5,' : rad/day             : QP_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless            : EHQ                : Environmental hazard quotient')
        END IF
!
     END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGNRQP .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_RAD_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD',UNITS_RAD_DOS_AQ,NREAL,RDWORK)
        IF( DET_BMRDOS) CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN )  THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'BURDEN',UNITS_RAD_BUR_AQ,CVEC )
      END IF
!
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,&
        'DOSRAD',UNITS_RAD_DOS_AQ,RDWORK )
!
      IF( DET_TFOODS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_RAD_BUR_TER, CWORK )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the concentration (as consumed by humans) to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold limits)
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_RAD_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_RAD_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose statistics (and threshold limits)
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_RAD_DOS_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_RAD_DOS_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NR_QP

      SUBROUTINE NR_TA( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Radioactive
!!      Species: Terrestrial, Animal
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 28 May 2002 : Change debug writes
!!    Paul W. Eslinger : 28 Apr 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 26 Oct 2004 : Add optional secondary transfer factor
!!    Paul W. Eslinger : 20 May 2005 : Add particulate fraction for far-field air
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 - Restructure soil dose equation
!!    Paul W. Eslinger : 30 Jun 2005 : Add error checking on energy intake
!!    Paul W. Eslinger :  1 Sep 2005 : Change debug outputs
!!    Paul W. Eslinger :  6 Sep 2006 : (SCR-1140) Reformat outputs
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NR_TA' ! Name of this subroutine
!
      INTEGER :: IREL    ! Realization index
      INTEGER :: JSPC    ! Species index
      INTEGER :: NREC    ! Record number for writing to the foods file
!
      REAL :: DINT       ! Internal dose
      REAL :: DIMM       ! Immersion dose
      REAL :: DSOI       ! Dose from soil
      REAL :: DSOI_ABOVE ! Dose from soil - above ground
      REAL :: DSOI_BELOW ! Dose from soil - below ground
      REAL :: CFELEV     ! Conversion factor for elevation
      REAL :: DDERMS     ! Dose - dermal contact with soil
      REAL :: DDERMW     ! Dose - dermal contact with water
      REAL :: DIV        ! Dose - inhalation of vapor
      REAL :: DIP        ! Dose - inhalation of particulates
      REAL :: CIV        ! Concentration - inhalation of vapor
      REAL :: CIP        ! Concentration - inhalation of particualtes
!
      REAL :: DINGW  ! Applied daily dose from water ingestion
      REAL :: CINGW  ! Equilibrium body burden from water ingestion
      REAL :: DINGS  ! Applied daily dose from soil ingestion
      REAL :: CINGS  ! Equilibrium body burden from soil ingestion
      REAL :: DINGF  ! Applied daily dose from food ingestion
      REAL :: CINGF  ! Equilibrium body burden from food ingestion
!
      REAL :: PROD    ! Temporary - metabilizable energy from one prey species
      REAL :: SUMPROD ! (Temporary) - calculating the intake rate
      REAL :: NIR     ! Normalized intake rate
      REAL :: CONJ    ! (Temporary) Concentration of a consumed species
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGNRTA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//,A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Radioactive '/ &
          'Species : ',A,' : Terrestrial, Animal     '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! *** Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
!
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) FWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
! ***   Calculate Dermal Contact with Soil (absorbed daily dose)
        DDERMS = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCS * ESD_SPC(ISPC)%SADHER * EC(IREL)%SOIL * CFDERMS * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * ( ESD_SPC(ISPC)%ALPHADS(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT )
!
! ***   Calculate Dermal Contact with water (absorbed daily dose)
        DDERMW = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCW * ESD_SPC(ISPC)%ETWATER * EC(IREL)%WATER * CFDERMW * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * ( ESD_SPC(ISPC)%ALPHADW(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT )
!
! ***   Calculate total dermal (absorbed daily dose)
        DDER(IREL) = DDERMS + DDERMW
!
!       Calculate equilibrium body burden from dermal contact
        CDER(ISPC,IREL) = DDER(IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL)
!
        IF( BGNRTA ) THEN
          WRITE(IRPT,1010) IREL, EC(IREL)%SOIL, EC(IREL)%WATER, EC(IREL)%VAPOR, EC(IREL)%AIRC, EC(IREL)%PARTIC, &
            ESD_SPC(ISPC)%SA, ESD_SPC(ISPC)%PCS, ESD_SPC(ISPC)%SADHER, CFDERMS, ESD_SPC(ISPC)%THETA, &
            ESD_SPC(ISPC)%PSI, ESD_SPC(ISPC)%ALPHADS(IANA,IREL), ESD_SPC(ISPC)%WEIGHT, DDERMS, &
            ESD_SPC(ISPC)%PCW, ESD_SPC(ISPC)%ETWATER, CFDERMW, ESD_SPC(ISPC)%ALPHADW(IANA,IREL), &
            DDERMW, ESD_SPC(ISPC)%DEPRATE(IANA,IREL), DDER(IREL), CDER(ISPC,IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : pCi/kg                       : EC%SOIL            : Concentration of contaminant in soil'/ &
            3X,E12.5,' : pCi/L                        : EC%WATER           : Concentration of contaminant in water'/ &
            3X,E12.5,' : pCi/m^3                      : EC%VAPOR           : Concentration of contaminant in vapor'/ &
            3X,E12.5,' : pCi/m^3                      : EC%AIRC            : Concentration of contaminant in air'/ &
            3X,E12.5,' : pCi/m^3                      : EC%PARTIC          : Concentration of contaminant in particulates'/&
            3X,E12.5,' : cm^2                         : ESD_SPC%SA         : Surface area of animal species'/ &
            3X,E12.5,' : 1/day                        : ESD_SPC%PCS        : Fraction of species surface area contacting soil/day'/&
            3X,E12.5,' : mg/cm^2                      : ESD_SPC%SADHER     : Skin adherence factor for animal species'/ &
            3X,E12.5,' : kg/mg                        : CFDERMS            : Conversion factor'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%THETA      : Area use factor (contaminated/home-range area)'/&
            3X,E12.5,' : unitless                     : ESD_SPC%PSI        : Seasonality factor (fraction of year in area)'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%ALPHADS    : Soil dermal absorption factor (contaminant, species)'/&
            3X,E12.5,' : kg wet                       : ESD_SPC%WEIGHT     : Body weight of animal species'/ &
            3X,E12.5,' : pCi/kg/day wet               : DDERMS             : Absorbed daily dose from dermal contact with soil'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%PCW        : Fraction of species surface area contacting water/dy'/&
            3X,E12.5,' : hr/day                       : ESD_SPC%ETWATER    : Average exposure time to water per day'/ &
            3X,E12.5,' : L/cm^3                       : CFDERMW            : Conversion factor'/ &
            3X,E12.5,' : cm/hr                        : ESD_SPC%ALPHADW    : Water dermal absorption factor(contaminant, species)'/&
            3X,E12.5,' : pCi/kg/day wet               : DDERMW             : Absorbed daily dose from dermal contact with water'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%DEPRATE    : Depuration rate of contaminant by animal species'/&
            3X,E12.5,' : pCi/kg/day wet               : DDER               : Absorbed daily dose from dermal contact'/&
            3X,E12.5,' : pCi/kg wet                   : CDER               : Equilibrium body burden from dermal exposure')
        END IF
!
! ***   Inhalation Dose from Vapor
        IF( ESD_ANA(IANA)%HENRY>0.0) THEN
!
!         Absorbed daily dose from inhalation of vapor
          DIV = ((ESD_SPC(ISPC)%INHRATE * (EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)+EC(IREL)%VAPOR) ) / ESD_SPC(ISPC)%WEIGHT) * &
            ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Absorbed daily dose from inhalation of vapor
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGNRTA ) THEN
            WRITE(IRPT,1030) ESD_ANA(IANA)%PARTICLE, ESD_ANA(IANA)%HENRY, ESD_SPC(ISPC)%INHRATE, &
              ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL), DIV, CIV
 1030       FORMAT( 1P, &
              3X,E12.5,' : pCi/m^3                      : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : Pa-m^3/mol                   : ESD_ANA%HENRY      : Henrys law constant'/&
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : unitless                     : ESD_SPC%ALPHAVAP   : Inhalation absorption factor of contaminant'/ &
              3X,E12.5,' : pCi/kg/day wet               : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : pCi/kg wet                   : CIV                : Equilibrium body burden from vapor inhalation')
          END IF
!
        ELSE
!
!         Absorbed daily dose from inhalation of vapor
          DIV = ((ESD_SPC(ISPC)%INHRATE * EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)) / ESD_SPC(ISPC)%WEIGHT) * &
            ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Absorbed daily dose from inhalation of vapor
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGNRTA ) THEN
            WRITE(IRPT,1040) ESD_SPC(ISPC)%INHRATE, ESD_ANA(IANA)%PARTICLE,  DIV, CIV
 1040       FORMAT( 1P, &
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : pCi/m^3                      : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : pCi/kg/day wet               : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : pCi/kg wet                   : CIV                : Equilibrium body burden from vapor inhalation')
          END IF
!
        END IF
!
! ***   Absorbed daily dose from inhalation of particles
        DIP = ESD_SPC(ISPC)%INHRATE * (EC(IREL)%PARTIC+EC(IREL)%AIRC*ESD_ANA(IANA)%PARTICLE) * ESD_SPC(ISPC)%THETA * &
          ESD_SPC(ISPC)%PSI / ESD_SPC(ISPC)%WEIGHT
!
!       Equilibrium body burden from inhalation of particulates
        CIP = DIP * ( ESD_SPC(ISPC)%ALPHAPAR(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
! ***   Absorbed daily dose from inhalation
        DINH(IREL) = DIV + DIP
!
!       Equilibrium body burden from inhalation
        CINH(ISPC,IREL) = CIV + CIP
!
        IF( BGNRTA ) THEN
          WRITE(IRPT,1050) DIP, CIP, DINH(IREL), CINH(ISPC,IREL)
 1050     FORMAT( 1P, &
            3X,E12.5,' : pCi/kg/day wet               : DIP                : Applied daily dose from particulate inhalation'/ &
            3X,E12.5,' : pCi/kg wet                   : CIP                : Equilibrium body burden from particulate inhalation'/ &
            3X,E12.5,' : pCi/kg/day wet               : DINH               : Applied daily dose from all inhalation'/ &
            3X,E12.5,' : pCi/kg wet                   : CINH               : Equilibrium body burden from all inhalation')
        END IF
!
! ***   Ingestion Dose from Water
        DINGW = ( ( ESD_SPC(ISPC)%WATERING * EC(IREL)%WATER ) / ESD_SPC(ISPC)%WEIGHT ) * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
        CINGW = DINGW * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
        IF( BGNRTA ) THEN
          WRITE(IRPT,1060) ESD_SPC(ISPC)%WATERING, DINGW, CINGW
 1060     FORMAT( 1P, &
            3X,E12.5,' : L/day                        : ESD_SPC%WATERING   : Water ingestion rate for animal species'/ &
            3X,E12.5,' : pCi/kg/day wet               : DINGW              : Applied daily dose from water ingestion'/ &
            3X,E12.5,' : pCi/kg wet                   : CINGW              : Equilibrium body burden from water ingestion')
        END IF
!
! ***   Calculate the total metabilizable energy from food (kcal/kg prey wet wt) [Use variable SUMPROD]
!       This is used in both food ingestion and soil ingestion calculations
        IF( BGNRTA ) THEN
          WRITE(IRPT,1070) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1070     FORMAT(/'   Species ',A,' (',A,') Metabilizable Energy From Prey'/&
            3X,'Prey   Predation',13X,'Gross Energy',20X,'Assimilation Efficiency',1X,'Metabilizable Energy'/&
            3X,'------',1X,21('-'),1X,31('-'),1X,23('-'),1X,32('-'))
        END IF
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate the metabilizable energy this prey (kcal/kg prey wet wt)
          PROD = PREDATE(ISPC,JSPC) * ESD_SPC(JSPC)%GE * ESD_SPC(JSPC)%AE
          IF( PROD .LE. 0.0 ) THEN
            IERR = 2
            MESSAG(1) = 'Food is ingested but no metabilizable energy is defined'
            MESSAG(2) = 'Location '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Species  '//ESD_SPC(ISPC)%ID //' Eating '//ESD_SPC(JSPC)%ID
            MESSAG(5) = 'Check the AE and GE values on the SPECIES keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Sum the metabilizable energy over all prey
          SUMPROD = SUMPROD + PROD
          IF( BGNRTA ) THEN
            WRITE(IRPT,1080) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(JSPC)%GE, ESD_SPC(JSPC)%AE, PROD
 1080       FORMAT(1P,3X,A, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kcal/kg wet weight', &
              1X,E12.5,' unitless', &
              3X,E12.5,' kcal/kg prey wet wt')
          END IF
!
        END DO
!
!       Total normalized intake rate (kg prey wet weight/kg predator body weight/day)
        IF( SUMPROD .GT. 0.0 ) THEN
          ESD_SPC(ISPC)%NIRTOT(IREL) = ESD_SPC(ISPC)%FMR / ( SUMPROD * ESD_SPC(ISPC)%WEIGHT )
        ELSE
          ESD_SPC(ISPC)%NIRTOT(IREL) = 0.0
        END IF
!
! ***    Calculate food ingestion dose from all species eaten
        IF( BGNRTA ) THEN
          WRITE(IRPT,1090) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1090     FORMAT(/'   Species ',A,' (',A,') Food ingestion calculations'/&
            3X,'Prey   Predation',13X,'Normalized Intake',32X,'Prey Concentration'/&
            3X,'------',1X,21('-'),1X,48('-'),1X,32('-'))
        END IF
!
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate normalized intake rate for this prey species
          NIR = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%NIRTOT(IREL)
!
!         Obtain the concentration of contaminant in the prey species
          IF( ESD_SPC(JSPC)%TYPE.EQ.'TP' .OR. ESD_SPC(JSPC)%TYPE.EQ.'QP' ) THEN
            CONJ = CPAT(JSPC,IREL)
          ELSE
            CONJ = CING(JSPC,IREL) + CINH(JSPC,IREL) + CDER(JSPC,IREL)
          END IF
!
!         Add the contaminant intake for this prey species to the total intake
          SUMPROD = SUMPROD + (CONJ * NIR)
!
          IF( BGNRTA ) THEN
            WRITE(IRPT,1110) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), NIR, CONJ
 1110       FORMAT(3X,A,1P, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kg prey wet wt/kg predator wet wt/d', &
              1X,E12.5,' pCi/kg prey wet wt')
          END IF
!
        END DO
!
!       Applied daily dose from all prey consumption (pCi/kg/day)
        DINGF = SUMPROD * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!       Equilibrium body burden from all prey consumption (pCi/kg body wet weight)
        CINGF = DINGF * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
        IF( BGNRTA ) THEN
          WRITE(IRPT,1120) DINGF, CINGF
 1120     FORMAT(/ 1P, &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : pCi/kg/day                   : DINGF              : Applied daily dose from food ingestion'/ &
            3X,E12.5,' : pCi/kg                       : CINGF              : Equilibrium body burden from food ingestion')
        END IF
!
! ***   Soil ingestion dose
        IF( ESD_SPC(ISPC)%NIRTOT(IREL) .GT. 0.0 ) THEN
!
!         Applied daily dose from soil consumption (pCi/kg/day)
          DINGS = ESD_SPC(ISPC)%SOILING * ESD_SPC(ISPC)%NIRTOT(IREL) * EC(IREL)%SOIL * &
            ESD_SPC(ISPC)%FDW * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equilibrium body burden from soil consumption (pCi/kg body wet weight)
          CINGS = DINGS * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGNRTA ) THEN
            WRITE(IRPT,1130) ESD_SPC(ISPC)%FMR, ESD_SPC(ISPC)%NIRTOT(IREL), ESD_SPC(ISPC)%SOILING, &
              ESD_SPC(ISPC)%FDW, DINGS, CINGS
 1130       FORMAT(1P, &
              3X,E12.5,' : kcal/day                     : ESD_SPC%FMR        : free-living metabolic rate of predator species'/ &
              3X,E12.5,' : kg prey wt/kg predator wt/d  : ESD_SPC%NIRTOT     : Total normalized intake rate'/ &
              3X,E12.5,' : kg soil ingested/kg dry diet : ESD_SPC%SOILING    : Soil ingestion rate'/ &
              3X,E12.5,' : kg dry/kg wet                : ESD_SPC%FDW        : Conversion factor, dry diet to wet diet'/ &
              3X,E12.5,' : pCi/kg/day wet               : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : pCi/kg wet                   : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
        ELSE
!
!         Applied daily dose from soil consumption (pCi/kg/day)
          DINGS = 0.0
!
!         Equilibrium body burden from soil consumption (pCi/kg body wet weight)
          CINGS = 0.0
!
          IF( BGNRTA ) THEN
            WRITE(IRPT,1140) DINGS, CINGS
 1140       FORMAT( 1P, &
              3X,E12.5,' : pCi/kg/day wet               : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : pCi/kg wet                   : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
        END IF
!
! ***   Applied daily dose from all ingestion (pCi/kg/day)
        DING(IREL) = DINGF + DINGS + DINGW
!
!       Equilibrium body burden from all ingestion (pCi/kg body wet weight)
        CING(ISPC,IREL) = CINGF + CINGS + CINGW
!
! ***   Equilibrium body burden from all sources (pCi/kg body wet weight)
        CWORK(IREL) = CING(ISPC,IREL) + CINH(ISPC,IREL) + CDER(ISPC,IREL)
!
!       Convert to secondary body burden for selected species
        IF( ESD_SPC(ISPC)%TRANS ) CWORK(IREL) = CWORK(IREL)*ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
!
! ***   Save the concentration for human consumption (wet weight basis)
        FWORK(IREL) = CWORK(IREL)
!
        IF( BGNRTA ) THEN
          WRITE(IRPT,1150) DING(IREL), CING(ISPC,IREL), DHAZ(IREL), CWORK(IREL), FWORK(IREL)
 1150     FORMAT( 1P, &
            3X,E12.5,' : pCi/kg/day wet               : DING               : Applied daily dose from all ingestion'/ &
            3X,E12.5,' : pCi/kg wet                   : CING               : Equilibrium body burden from all ingestion'/ &
            3X,E12.5,' : pCi/kg/day wet               : DHAZ               : Total absorbed daily dose'/ &
            3X,E12.5,' : pCi/kg wet                   : CWORK              : Total equilibrium body burden'/ &
            3X,E12.5,' : pCi/kg wet                   : FWORK              : Total equilibrium body burden for human consumption')
          IF( ESD_SPC(ISPC)%TRANS ) WRITE(IRPT,1160) ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
 1160     FORMAT( 1P, &
            3X,E12.5,' : pCi second/pCi primary       : TRANSVAL           : Transfer factor')
        END IF
!
! ***   Radiation dose from internal sources
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from immersion
        DIMM = EC(IREL)%WATER * ESD_ANA(IANA)%DFIMM * ESD_SPC(ISPC)%FWATER * ESD_SPC(ISPC)%ETWATER * TIMMFAC
!
!       External radiation dose from soil
        IF( ESD_SPC(ISPC)%DIFFHT .LT. 1.0 ) THEN
          CFELEV = 2.0
        ELSE
          CFELEV = 1.0
        END IF
        DSOI_ABOVE = EC(IREL)%SOIL * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDRF
        DSOI_BELOW = EC(IREL)%SOIL * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSOI = DSOI_ABOVE + DSOI_BELOW
!
!       Total radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSOI
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / TA_BMR
!
        IF( BGNRTA ) THEN
          WRITE(IRPT,1170) ESD_SPC(ISPC)%EFFENG(IANA), DINT, ESD_ANA(IANA)%DFIMM, ESD_SPC(ISPC)%FWATER, ESD_SPC(ISPC)%ETWATER, &
            DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_ANA(IANA)%DFSED, EC(IREL)%SEDI, DSOI_ABOVE, ESD_ANA(IANA)%GAMMA, &
            DSOI_BELOW, DSOI, RDWORK(IREL)
 1170     FORMAT(1P, &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X, E12.5,' : MeV/disintegration           : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X, E12.5,' : rad/day                      : DINT               : Radiological dose from internal contamination'/ &
            3X, E12.5,' : mrad/yr per uCi/m^3          : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X, E12.5,' : unitless                     : ESD_SPC%FWATER     : Fraction of surface area in water contact'/ &
            3X, E12.5,' : h/day                        : ESD_SPC%ETWATER    : Exposure time to water'/ &
            3X, E12.5,' : rad/day                      : DIMM               : Radiological dose from water immersion'/ &
            3X, E12.5,' : unitless                     : CFELEV             : Conversion factor based on time in water'/ &
            3X, E12.5,' : unitless                     : ESD_SPC%FABOVE     : Fraction time spent above soil'/ &
            3X, E12.5,' : Sv-m^3/sec-Bq                : ESD_ANA%DFSED      : Dose factor for external exposure to soil'/ &
            3X, E12.5,' : pCi/kg                       : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : rad/day                      : DSED_ABOVE         : Radiological dose from soil - above'/ &
            3X, E12.5,' : MeV/nt                       : ESD_ANA%GAMMA      : (photon) gamma energy'/ &
            3X, E12.5,' : rad/day                      : DSED_BELOW         : Radiological dose from soil - below'/ &
            3X, E12.5,' : rad/day                      : DSED               : Total radiological dose from soil'/ &
            3X, E12.5,' : rad/day                      : RDWORK             : Total radiological dose')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1180) TA_BMR, EHQ(IREL)
 1180     FORMAT(1P, &
            3X, E12.5,' : rad/day                      : TA_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless                     : EHQ                : Environmental hazard quotient')
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
!
      IF( BGNRTA .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD',UNITS_RAD_DOS_TER,NREAL,RDWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSDER',UNITS_RAD_DOS_TER,NREAL,DDER)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSINH',UNITS_RAD_DOS_TER,NREAL,DINH)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSING',UNITS_RAD_DOS_TER,NREAL,DING)
        IF( DET_BMRDOS) &
          CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
!
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the computed values
!
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_RAD_BUR_TER, CWORK )
!
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSRAD', UNITS_RAD_DOS_TER, RDWORK )
!
      IF( DET_DOSDER ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSDER', UNITS_RAD_DOS_TER, DDER )
!
      IF( DET_DOSINH ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSINH', UNITS_RAD_DOS_TER, DINH )
!
      IF( DET_DOSING ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSING', UNITS_RAD_DOS_TER, DING )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the burden (as consumed) to the food file
!
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden
!
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_RAD_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radioactive dose
!
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Dermal dose
!
      IF( STA_DOSDER .OR. THRESHOLD%EXIST1(ISPC,IANA,2) .OR. THRESHOLD%EXIST2(ISPC,IANA,2) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,2)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,2)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,2)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,2)
        CALL USTAT_ECEM( DDER, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSDER'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSDER ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSDER', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSDER', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Inhalation dose
!
      IF( STA_DOSINH .OR. THRESHOLD%EXIST1(ISPC,IANA,4) .OR. THRESHOLD%EXIST2(ISPC,IANA,4) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,4)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,4)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,4)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,4)
        CALL USTAT_ECEM( DINH, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSINH'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSINH ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSINH', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSINH', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Ingestion dose
!
      IF( STA_DOSING .OR. THRESHOLD%EXIST1(ISPC,IANA,3) .OR. THRESHOLD%EXIST2(ISPC,IANA,3) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,3)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,3)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,3)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,3)
        CALL USTAT_ECEM( DING, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSING'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSING ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSING', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSING', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NR_TA

      SUBROUTINE NR_TP( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Radioactive
!!      Species: Terrestrial, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 28 Jul 2004 : SCR-1063 Replace TUPTAKE with FW
!!    Paul W. Eslinger : 18 Nov 2004 : Change leaf deposition equations
!!    Paul W. Eslinger :  6 Dec 2004 : Change dependencies for IFRACDRY,
!!                                     IFRACWET, LWEATHDRY, and LWEATHWET.
!!    Paul W. Eslinger : 17 Jan 2005 : Wet leaf deposition is applied
!!                                     only for UPLAND locations
!!    Paul W. Eslinger : 20 Jan 2005 : Adjust leaf deposition totals
!!    Paul W. Eslinger :  9 Feb 2005 : Adjust leaf deposition totals again
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Adjust leaf deposition totals again
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 - Restructure soil dose equation
!!    Paul W. Eslinger : 15 Jun 2005 : SCR-1080 - Change HUMAN food options
!!    Paul W. Eslinger : 27 Jan 2006 : Correct KPA1 equation for KOW being in log units
!!    Paul W. Eslinger : 17 Aug 2006 : (SCR-1140) Reformat outputs
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NR_TP' ! Name of this subroutine
      INTEGER :: IREL ! Realization index
      REAL :: DINT    ! Internal dose
      REAL :: DIMM    ! Immersion dose
!
      REAL :: DSOI       ! Radiation dose from sediment
      REAL :: DSOI_ABOVE ! Radiation dose from sediment - above
      REAL :: DSOI_BELOW ! Radiation dose from sediment - below
!
      REAL :: TMPCPAR ! Concentration on leaf surface from rain splash
      REAL :: TMPCPAD ! Concentration on leaf surface from dry deposition
      REAL :: TMPCPAU ! Concentration in plant from root uptake
      REAL :: TMPCPAV ! Concentration in plant from vapor uptake
      REAL :: TMPCPAP ! Concentration on leaf surface from particulate deposition
      REAL :: TMPCPAW ! Concentration on leaf surface from wet deposition
!
      REAL :: CFELEV  ! Conversion factor for elevation
      REAL :: KPA1    ! Plant-air partition coefficient to above-ground parts
!
      INTEGER :: NREC ! Record number for writing to the foods file
      REAL :: PCT     ! Temporary output variable
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGNRTP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME, &
          TRIM(BGTYPE), TRIM(BGWATER)
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Radioactive '/ &
          'Species : ',A,' : Terrestrial, Plant      '/ &
          'Time    : ',I0 / &
          'Location type is ',A/ &
          'Water source/use is ',A/ &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
      FORALL(IREL=1:NREAL) FWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
!       Concentration from rain splash effect on leaf external surface
        IF( ESD_SPC(ISPC)%EMERGENT ) THEN
          TMPCPAR = 0.0
        ELSE
          TMPCPAR = EC(IREL)%SOIL * ESD_SPC(ISPC)%KPS1(IREL)
        END IF
!
!       Concentration from dry deposition effect on leaf external surface
        TMPCPAD = EC(IREL)%AIRD * ESD_SPC(ISPC)%IFRACDRY(IANA,IREL) / &
                  (ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL) )
!
!       Concentration from wet deposition effect on leaf external surface (only for upland irrigated locations)
        IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
          TMPCPAW = EC(IREL)%WATER * (IRIG_AMT/SEASON_GROW) * ESD_SPC(ISPC)%IFRACWET(IANA,IREL) * 10.0 / &
                    ( ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHWET(IANA,IREL) )
        ELSE
          TMPCPAW = 0.0
        END IF
!
!       Concentration from local particulate deposition effect on leaf external surface
        TMPCPAP = EC(IREL)%PARTIC * ESD_SPC(ISPC)%KPA2(IREL)
!
!       Concentration from root uptake (different model for tritium)
        IF( ESD_ANA(IANA)%ID .EQ. 'H3' ) THEN
          TMPCPAU = EC(IREL)%WATER * ESD_SPC(ISPC)%FW
        ELSE
          IF( ESD_SPC(ISPC)%EMERGENT ) THEN
            TMPCPAU = EC(IREL)%SEDI * ESD_SPC(ISPC)%BCFVEG(IANA,IREL) * ( 1.0 - ESD_SPC(ISPC)%FW )
          ELSE
            TMPCPAU = EC(IREL)%SOIL * ESD_SPC(ISPC)%BCFVEG(IANA,IREL) * ( 1.0 - ESD_SPC(ISPC)%FW )
          END IF
        END IF
!
!       Concentration from foliar uptake of vapor
        IF( ESD_ANA(IANA)%HENRY > 0.0 ) THEN
          KPA1 = ( ESD_SPC(ISPC)%FPA + ( ESD_SPC(ISPC)%FPW + ESD_SPC(ISPC)%FPL * (10.0**ESD_ANA(IANA)%KOW(IREL)) ) * &
            (( GASCON * ( ESD_LOC(ILOC)%TEMP + 273.15 )) / ESD_ANA(IANA)%HENRY )) * ( 1.0 / ESD_SPC(ISPC)%RHOP )
          TMPCPAV = (EC(IREL)%VAPOR+EC(IREL)%AIRC) * KPA1
        ELSE
          TMPCPAV = 0.0
        END IF
!
!       Calculate internal plant burden
!       Vapor uptake + Root uptake + Leaf absorption (dry) + Leaf absorption (wet)
        CPAI(ISPC,IREL) = TMPCPAV + TMPCPAU + (TMPCPAR+TMPCPAP+TMPCPAD)*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) + &
          TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL)
!
!       Calculate total plant burden (as consumed by predators)
!       Leaf surface (rain + particulates) + internal burden
        CPAT(ISPC,IREL) = TMPCPAV + TMPCPAU + TMPCPAR + TMPCPAP + TMPCPAD + TMPCPAW
!
!       Save internal plant body burden
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = CPAI(ISPC,IREL)
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
!
!       Save the concentration for human consumption (wet weight basis)
        IF( ESD_SPC(ISPC)%INTERNAL ) THEN
          FWORK(IREL) = CPAI(ISPC,IREL)
        ELSE
          FWORK(IREL) = CPAT(ISPC,IREL)
        END IF
!
! ***   Debug writes
!
        IF( BGNRTP ) THEN
!
          WRITE(IRPT,1010) IREL, EC(IREL)%WATER, EC(IREL)%SOIL, EC(IREL)%SEDI, EC(IREL)%PARTIC, EC(IREL)%VAPOR, &
            EC(IREL)%AIRC, EC(IREL)%AIRD
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units                : Variable           : Description'/ &
            3X,12('-'),3X,20('-'),3X,18('-'),3X,72('-')  /&
            3X,E12.5,' : pCi/kg               : EC%WATER           : Water concentration'/ &
            3X,E12.5,' : pCi/kg               : EC%SOIL            : Soil concentration'/&
            3X,E12.5,' : pCi/kg               : EC%SEDI            : Sediment concentration'/&
            3X,E12.5,' : pCi/kg               : EC%PARTIC          : Particulate concentration'/&
            3X,E12.5,' : pCi/m^3              : EC%VAPOR           : Vapor concentration (local soil)'/&
            3X,E12.5,' : pCi/m^3              : EC%AIRC            : Air concentration (distant sources)'/&
            3X,E12.5,' : pCi/m^2/yr           : EC%AIRD            : Air deposition rate')
!
          IF( .NOT.ESD_SPC(ISPC)%EMERGENT ) WRITE(IRPT,1020) ESD_SPC(ISPC)%KPS1(IREL)
 1020     FORMAT(1P,&
            3X,E12.5,' : kg soil/kg plant wet : ESD_SPC%KPS1       : Plant-soil partition coefficient for rain splash')
!
          WRITE(IRPT,1030) TMPCPAR, ESD_SPC(ISPC)%IFRACDRY(IANA,IREL), ESD_SPC(ISPC)%BIOMASS(IREL), &
            ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL), TMPCPAD, ESD_SPC(ISPC)%FW, ESD_SPC(ISPC)%KPA2(IREL), TMPCPAP
 1030     FORMAT(1P,&
            3X,E12.5,' : pCi/kg wet           : TMCPAR             : Concentration in above-ground plant from rain splash'/&
            3X,E12.5,' : unitless             : ESD_SPC%IFRACDRY   : Foliar interception from dry deposition'/&
            3X,E12.5,' : kg/m^2               : ESD_SPC%BIOMASS    : Standing biomass of plant species'/&
            3X,E12.5,' : 1/year               : ESD_SPC%LWEATHDRY  : Foliar weathering rate from dry deposition'/&
            3X,E12.5,' : pCi/kg wet           : TMCPAD             : Concentration in above-ground plant from dry deposition'/&
            3X,E12.5,' : unitless             : ESD_SPC%FW         : Fraction of plant that is water'/ &
            3X,E12.5,' : m^3/kg plant wet     : ESD_SPC%KPA2       : Plant-air partition coeff., particulate-bound contaminants'/ &
            3X,E12.5,' : pCi/kg wet           : TMPCPAP            : Concentration in foilage from particulates')
!
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) &
            WRITE(IRPT,1040) IRIG_AMT, SEASON_GROW, ESD_SPC(ISPC)%IFRACWET(IANA,IREL), ESD_SPC(ISPC)%LWEATHWET(IANA,IREL)
 1040       FORMAT(1P,&
            3X,E12.5,' : cm                   : IRIG_AMT           : Amount of irrigation water applied in a year'/ &
            3X,E12.5,' : year                 : SEASON_GROW        : Length of the growing season'/ &
            3X,E12.5,' : unitless             : ESD_SPC%IFRACWET   : Foliar wet interception fraction'/ &
            3X,E12.5,' : 1/year               : ESD_SPC%LWEATHWET  : Foliar weathering rate from wet deposition')
!
          WRITE(IRPT,1050) TMPCPAW
 1050     FORMAT(1P,&
            3X,E12.5,' : pCi/kg wet           : TMCPAW             : Concentration in above-ground plant from wet deposition')
!
          IF( ESD_ANA(IANA)%ID .NE. 'H3' ) &
            WRITE(IRPT,1060) ESD_SPC(ISPC)%BCFVEG(IANA,IREL)
 1060       FORMAT(1P, &
            3X,E12.5,' : kg-soil/kg plant dry : ESD_SPC%BCFVEG     : Bioconcentration factor for vegetative parts of plant')
!
          WRITE(IRPT,1070) TMPCPAU
 1070     FORMAT(1P, &
            3X,E12.5,' : pCi/kg wet           : TMPCPAU            : Concentration in plant from root uptake')
!
          IF( ESD_ANA(IANA)%HENRY > 0.0 ) &
            WRITE(IRPT,1080) ESD_SPC(ISPC)%FPA, ESD_SPC(ISPC)%FPW, ESD_SPC(ISPC)%FPL, ESD_ANA(IANA)%KOW(IREL), &
              GASCON, ESD_LOC(ILOC)%TEMP, ESD_ANA(IANA)%HENRY, ESD_SPC(ISPC)%RHOP, KPA1
 1080       FORMAT(1P, &
            3X,E12.5,' : unitless             : ESD_SPC%FPA        : Volume fraction of plant that is air'/ &
            3X,E12.5,' : unitless             : ESD_SPC%FPW        : Volume fraction of plant that is water'/ &
            3X,E12.5,' : unitless             : ESD_SPC%FPL        : Volume fraction of plant that is lipid'/ &
            3X,E12.5,' : unitless             : ESD_ANA%KOW        : Log base 10 of octanol-water partition coefficient'/&
            3X,E12.5,' : Pa-m^3/mol-K         : GASCON             : Universal gas constant'/ &
            3X,E12.5,' : Centigrade           : ESD_LOC%TEMP       : Temperature'/ &
            3X,E12.5,' : Pa-m^3/mol           : ESD_ANA%HENRY      : Henrys law constant'/ &
            3X,E12.5,' : kg/m^3               : ESD_SPC%RHOP       : Plant tissue density'/ &
            3X,E12.5,' : m^3/kg wet weight    : KPA1               : Plant-air partition coefficient (calculated)')
!
          WRITE(IRPT,1090) TMPCPAV, ESD_SPC(ISPC)%KLIDRY(IANA,IREL), ESD_SPC(ISPC)%KLIWET(IANA,IREL), CPAI(ISPC,IREL), &
            CPAT(ISPC,IREL)
 1090     FORMAT(1P, &
            3X,E12.5,' : pCi/kg wet           : TMPCPAV            : Concentration in plant from vapor'/&
            3X,E12.5,' : unitless             : ESD_SPC%KLIDRY     : Foliar dry interception of contaminant by plant'/ &
            3X,E12.5,' : unitless             : ESD_SPC%KLIWET     : Foliar wet interception of contaminant by plant'/ &
            3X,E12.5,' : pCi/kg wet           : CPAI               : Plant internal concentration' /&
            3X,E12.5,' : pCi/kg wet           : CPAT               : Plant total concentration')
!
        END IF
!
! ***   Compute Radiological Dose
!
!       Radiation dose from internal exposure
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from water immersion
        DIMM = EC(IREL)%WATER * ESD_ANA(IANA)%DFIMM * ESD_SPC(ISPC)%FWATER * ESD_SPC(ISPC)%ETWATER * TIMMFAC
!
!       Radiation dose from soil exposure
        IF( ESD_SPC(ISPC)%DIFFHT .LT. 1.0 ) THEN
          CFELEV = 2.0
        ELSE
          CFELEV = 1.0
        END IF
        DSOI_ABOVE = EC(IREL)%SOIL * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDRF
        DSOI_BELOW = EC(IREL)%SOIL * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSOI = DSOI_ABOVE + DSOI_BELOW
!
!       Total radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSOI
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / TP_BMR
!
        IF( BGNRTP ) THEN
          WRITE(IRPT,1100) ESD_SPC(ISPC)%EFFENG(IANA), DINT, ESD_ANA(IANA)%DFIMM, ESD_SPC(ISPC)%FWATER, ESD_SPC(ISPC)%ETWATER, &
            DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_SPC(ISPC)%DIFFHT, ESD_ANA(IANA)%DFSED, DSOI_ABOVE, ESD_ANA(IANA)%GAMMA, &
            DSOI_BELOW, DSOI, RDWORK(IREL)
 1100     FORMAT(1P,&
            3X,12('-'),3X,20('-'),3X,18('-'),3X,72('-')  /&
            3X, E12.5,' : MeV/disintegration   : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X, E12.5,' : rad/day              : DINT               : Radiological dose from internal contamination'/ &
            3X, E12.5,' : mrad/yr per uCi/m^3  : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X, E12.5,' : unitless             : ESD_SPC%FWATER     : Faction of time the species is immersed in water'/ &
            3X, E12.5,' : hour/day             : ESD_SPC%ETWATER    : Exposure time to water'/ &
            3X, E12.5,' : rad/day              : DIMM               : Radiological dose from water immersion'/ &
            3X, E12.5,' : unitless             : CFELEV             : Conversion factor based on time in pore water'/ &
            3X, E12.5,' : unitless             : FABOVE             : Fraction time spent above sediment'/ &
            3X, E12.5,' : m                    : ESD_SPC%DIFFHT     : Diffusion height of species'/ &
            3X, E12.5,' : Sv-m^3/sec-Bq        : ESD_ANA%DFSED      : Dose factor for external exposure to sediment'/ &
            3X, E12.5,' : rad/day              : DSOI_ABOVE         : Radiation dose from soil - above'/ &
            3X, E12.5,' : MeV/nt               : ESD_ANA%GAMMA      : Gamma (photon) energy'/ &
            3X, E12.5,' : rad/day              : DSOI_BELOW         : Radiation dose from soil - below'/ &
            3X, E12.5,' : rad/day              : DSOI               : Total radiation dose from soil'/ &
            3X, E12.5,' : rad/day              : RDWORK             : Total radiation dose')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1110) TP_BMR, EHQ(IREL)
 1110     FORMAT(1P, &
            3X, E12.5,' : rad/day              : TP_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless             : EHQ                : Environmental hazard quotient')
!
          WRITE(IRPT,1120)
 1120     FORMAT(3X,12('-'),3X,20('-'),3X,18('-'),3X,72('-'))
        END IF
!
        IF( BGNRTP ) THEN
!
          WRITE(IRPT,1220) '   Internal plant burden (pCi/kg wet plant weight) for '//TRIM(ESD_ANA(IANA)%ID)//' on '//&
            TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from rainsplash'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from soil particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from ambient air particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL), PCT, 'Foliar adsorption from wet deposition'
          WRITE(IRPT,1240) CPAI(ISPC,IREL), 'Total internal plant burden'
 1220     FORMAT(/A)
 1230     FORMAT(3X,1P,E12.5,0P,' : ',F8.4,'% : ',A)
 1240     FORMAT(3X,1P,E12.5,' : ',A)
!
          WRITE(IRPT,1220) '   Plant burden (pCi/kg wet plant weight) as consumed by predators for '//&
            TRIM(ESD_ANA(IANA)%ID)//' on '//TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR, PCT, 'Rainsplash to above ground plant parts'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP, PCT, 'Foliar interception from soil particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD, PCT, 'Foliar interception ambient air particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW, PCT, 'Foliar interception from wet deposition'
          WRITE(IRPT,1240) CPAT(ISPC,IREL), 'Total plant burden'
!
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
!
      IF( BGNRTP .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD',UNITS_RAD_DOS_TER,NREAL,RDWORK)
        IF( DET_BMRDOS) &
           CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
!
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the computed values
!
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_RAD_BUR_TER, CWORK )
!
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSRAD', UNITS_RAD_DOS_TER, RDWORK )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the burden (as consumed) to the food file
!
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden
!
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_RAD_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radioactive dose
!
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NR_TP

      SUBROUTINE NS_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Nonradioactive (stable)
!!      Species: Aquatic, Animal
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS
!!    Paul W. Eslinger :  3 Jun 2002 : Change sediment ingestion equation.
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 28 Jul 2004 : SCR-1063 Adjust for tritium
!!                                     actually being water
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger : 15 Aug 2006 : (SCR-1140) Reformat outputs
!!                                     Simplify the equations for food ingestion
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS      ! Logical flag whether skip all data outputs
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NS_QA' ! Name of this subroutine
      CHARACTER(LEN=6) :: LOC_ID ! Temporary location ID
!
      INTEGER :: IREL ! Realization index
      INTEGER :: JSPC ! Species index
      INTEGER :: NREC ! Record number for writing to the foods file
!
      REAL :: TMPING  ! Body burden from food ingestion
      REAL :: TMPWAT  ! Body burden from water exposure
      REAL :: TMPSED  ! Body burden from sediment ingestion
      REAL :: TMPAIR  ! Body burden from air inhalation
!
      REAL :: IIJ    ! Temporary variable in ingestion equations (feeding rate)
      REAL :: BIOACC ! Temporary bioaccumulation value in ingestion equations
!
      LOGICAL :: EAT_FOOD ! Local flag whether this animal eats food
!
!---- Executable code ------------------------------------------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGNSQA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '--------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Nonradioactive'/ &
          'Species : ',A,' : Aquatic, Animal          '/ &
          'Time    : ',I0 / &
          '--------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
      FORALL(IREL=1:NREAL) FWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
!     Determine whether this animal eats any food.  This calculation doesn't depend on any specific
!     realizations and simply makes it easier to write more elegant debug statements
      EAT_FOOD = .FALSE.
      DO JSPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) EAT_FOOD = .TRUE.
      END DO
!
! *** Start calculations depending on realization
      DO IREL = 1, NREAL
!
!       Body burden from water exposure
        TMPWAT = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
                 (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * ESD_SPC(ISPC)%BCF(IANA,IREL)
!
!       Body burden from air inhalation
        IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
          TMPAIR = (EC(IREL)%AIRC+EC(IREL)%VAPOR)*ESD_SPC(ISPC)%INHRATE / (ESD_SPC(ISPC)%WBMASS/ESD_SPC(ISPC)%AWD)
          TMPAIR = TMPAIR * ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / (ESD_SPC(ISPC)%DEPRATE(IANA,IREL)+ESD_SPC(ISPC)%GROWRATE)
        ELSE
          TMPAIR = 0.0
        END IF
!
!       Initialize food ingestion and sediment ingestion body burdens
        TMPING = 0.0
        TMPSED = 0.0
!
!       Skip a lot of calculations if this animal doesn't eat any food
        IF( EAT_FOOD ) THEN
!
!         Debug outputs
          IF( BGNSQA ) THEN
            WRITE(IRPT,1020)
 1020       FORMAT(/'   Food intake calculations for all prey ...'/&
                    '   IIJ = Feeding rate (g prey dry weight/g predator dry weight/day)'/ &
              '   Number  Name      PREDATE     ALPHAIJ    GROWRATE    RESPRATE        OCAR', &
              '       FOC-J       FOC-I   DEPRATE    BioFactor      IIJ'/ &
              '   ------ ------ ----------- ----------- ----------- ----------- -----------', &
              ' ----------- ----------- ----------- ----------- -----------')
          END IF
!
!         Food and sediment ingestion calculations
          BIOACC = 0.0
          DO JSPC = 1, ESD_NUM_SPC
!
!           Skip species not computed or not eaten by the predator
            IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
            IF( .NOT.(PREDATE(ISPC,JSPC).GT.0.0) ) CYCLE
!
!           Feeding rate (g prey dry weight/g predator dry weight/day)
            IIJ = ( ESD_SPC(ISPC)%GROWRATE+ESD_SPC(ISPC)%RESPRATE ) / ESD_SPC(ISPC)%OCAR * &
              ( ESD_SPC(ISPC)%FOC / ESD_SPC(JSPC)%FOC )
!
!           Accumulation factor for prey body burden
            BIOACC = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL) * IIJ / &
                ( ESD_SPC(ISPC)%DEPRATE(IANA,IREL) + ESD_SPC(ISPC)%GROWRATE )
!
!           Increment the ingestion and sediment body burdens
            TMPING = TMPING + BIOACC * ESD_SPC(JSPC)%BODYBURDEN(IREL)
            TMPSED = TMPSED + BIOACC * (EC(IREL)%SEDI * ESD_SPC(ISPC)%SEDING)
!
!           Debug outputs
            IF( BGNSQA ) WRITE(IRPT,1021) JSPC, ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL), &
                ESD_SPC(ISPC)%GROWRATE, ESD_SPC(ISPC)%RESPRATE, ESD_SPC(ISPC)%OCAR, &
                ESD_SPC(JSPC)%FOC, ESD_SPC(ISPC)%FOC, ESD_SPC(ISPC)%DEPRATE(IANA,IREL), BIOACC, IIJ
 1021         FORMAT(3X,I6,1P,1X,A,10(1X,E11.4))
!
          END DO
!
        END IF
!
!       Total body burden (water exposure + food ingestion + sediment ingestion + air inhalation)
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = TMPWAT + TMPING + TMPSED + TMPAIR
!
!       Convert body burden to concentration (wet weight basis)
!        IF( ESD_ANA(IANA)%ID .EQ. 'H3' ) THEN
!          CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * (1.0 - 1.0/ESD_SPC(ISPC)%AWD)
!        ELSE
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%AWD
!        END IF
!
!       Save concentration for terrestrial consumption (wet weight basis)
        CING(ISPC,IREL) = CWORK(IREL)
!
!       Save concentration in food for human consumption (wet weight basis)
        FWORK(IREL) = CWORK(IREL)
!
!       Optionally calculate the environmental hazard quotient
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          EHQ(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
        IF( BGNSQA ) THEN
          WRITE(IRPT,1050) IREL, EC(IREL)%PWAT, EC(IREL)%SWAT, EC(IREL)%SEDI, ESD_SPC(ISPC)%BPORE(IREL), &
            ESD_SPC(ISPC)%BCF(IANA,IREL), ESD_SPC(ISPC)%SEDING, TMPWAT, TMPING, TMPSED, TMPAIR, ESD_SPC(ISPC)%BODYBURDEN(IREL), &
            ESD_SPC(ISPC)%AWD, CWORK(IREL), FWORK(IREL)
 1050     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units               : Variable           : Description'/ &
            3X,12('-'),3X,19('-'),3X,18('-'),3X,40('-')  /&
            3X,E12.5,' : ug/L                : EC%PWAT            : Pore water concentration'/ &
            3X,E12.5,' : ug/L                : EC%SWAT            : Surface water concentration'/ &
            3X,E12.5,' : ug/kg               : EC%SEDI            : Sediment concentration'/ &
            3X,E12.5,' : unitless            : ESD_SPC%BPORE      : Relative exposure to pore water'/ &
            3X,E12.5,' : L/kg dry            : ESD_SPC%BCF        : Bioconcentration factor from water for contaminant'/ &
            3X,E12.5,' : unitless            : ESD_SPC%SEDING     : Diet fraction from sediment'/ &
            3X,E12.5,' : ug/kg dry           : TMPWAT             : Water exposure part of body burden'/ &
            3X,E12.5,' : ug/kg dry           : TMPING             : Food ingestion part of body burden'/ &
            3X,E12.5,' : ug/kg dry           : TMPSED             : Sediment ingestion part of body burden'/ &
            3X,E12.5,' : ug/kg dry           : TMPAIR             : Inhalation part of body burden'/ &
            3X,E12.5,' : ug/kg dry           : ESD_SPC%BODYBURDEN : Body burden'/&
            3X,E12.5,' : g wet/g dry         : ESD_SPC%AWD        : Wet to dry weight ratio'/&
            3X,E12.5,' : ug/kg wet           : CWORK              : Concentration consumed by terrestrial animals'/&
            3X,E12.5,' : ug/kg wet           : FWORK              : Concentration consumed by human')
!
          WRITE(IRPT,1060) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1060     FORMAT(&
            3X,ES12.5,' : ug/kg dry           : ESD_SPC%BMTISS     : Tissue benchmark' /&
            3X,ES12.5,' : unitless            : EHQ                : Environmental hazard quotient')
!
          IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
            WRITE(IRPT,1070) EC(IREL)%AIRC,EC(IREL)%VAPOR,ESD_SPC(ISPC)%INHRATE,ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL)
 1070       FORMAT(&
            3X,ES12.5,' : ug/m^3              : EC%AIRC            : Air concentration'/ &
            3X,ES12.5,' : ug/m^3              : EC%VAPOR           : Vapor concentration'/ &
            3X,ES12.5,' : m^3/day             : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
            3X,ES12.5,' : unitless            : ESD_SPC%ALPHAVAP   : Vapor inhalation assimilation efficiency')
          END IF
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGNSQA .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_STA_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_STA_BUR_TER,NREAL,CWORK)
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'EHQ',UNITS_NONE,NREAL,EHQ)
       END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'BURDEN',UNITS_STA_BUR_AQ,CVEC )
      END IF
      IF( DET_EHQ ) CALL WRITE_DETAILS(ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'EHQ',UNITS_NONE,EHQ)
      IF( DET_TFOODS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_STA_BUR_TER, CWORK )
!
! *** Optionally write the burden (as consumed) to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold calculations)
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_STA_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_STA_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Environmental hazard quotient statistics
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_STA_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NS_QA

      SUBROUTINE NS_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Nonradioactive (stable)
!!      Species: Aquatic, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  3 Nov 1998 : Version 1.1
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 28 Jul 2004 : SCR-1063 Adjust for tritium
!!                                     actually being water
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Change to EHQ & reformat
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS ! Logical whether skip all data outputs
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NS_QP' ! Name of this subroutine
      INTEGER :: IREL ! Realization index
      INTEGER :: NREC ! Record number for writing to the foods file
      CHARACTER(LEN=6) :: LOC_ID ! Local location ID
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGNSQP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Nonradioactive'/ &
          'Species : ',A,' : Aquatic, Plant            '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize work vectors
      FORALL(IREL=1:NREAL) FWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
!       Body burden (consumed by aquatic species)
        ESD_SPC(ISPC)%BODYBURDEN(IREL)  = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
                 (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * ESD_SPC(ISPC)%BCF(IANA,IREL)
!
!       Concentration for consumption by terrestrial species
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%AWD
!
!       Save concentration for terrestrial species consumption
        CPAT(ISPC,IREL) = CWORK(IREL)
!
!       Concentration in foods for human consumption (wet weight basis)
        FWORK(IREL) = CWORK(IREL)
!
!       Optional environmental hazard quotient (wet weight basis)
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          EHQ(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
        IF( BGNSQP ) THEN
!
          WRITE(IRPT,1010) IREL, EC(IREL)%PWAT, EC(IREL)%SWAT, ESD_SPC(ISPC)%BPORE(IREL), ESD_SPC(ISPC)%BCF(IANA,IREL), &
            ESD_SPC(ISPC)%BODYBURDEN(IREL), ESD_SPC(ISPC)%AWD, CWORK(IREL), FWORK(IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units       : Variable           : Description'/ &
            3X,12('-'),3X,11('-'),3X,18('-'),3X,50('-')  /&
            3X, E12.5,' : ug/L        : EC%PWAT            : Pore water concentration'/ &
            3X, E12.5,' : ug/L        : EC%SWAT            : Surface water concentration'/ &
            3X, E12.5,' : unitless    : ESD_SPC%BPORE      : Relative exposure to pore water'/ &
            3X, E12.5,' : L/kg dry    : ESD_SPC%BCF        : Bioconcentration factor for contaminant'/ &
            3X, E12.5,' : ug/kg dry   : ESD_SPC%BODYBURDEN : Body burden (consumed by aquatic animals)'/ &
            3X, E12.5,' : g wet/g dry : ESD_SPC%AWD        : wet to dry weight ratio'/ &
            3X, E12.5,' : ug/kg wet   : CWORK              : Concentration consumed by terrestrial animals'/ &
            3X, E12.5,' : ug/kg wet   : FWORK              : Concentration consumed by humans')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1020) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1020     FORMAT(1P, &
            3X, E12.5,' : ug/kg wet   : ESD_SPC%BMTISS     : Tissue benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless    : EHQ                : Environmental hazard quotient')
!
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGNSQP .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_STA_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_STA_BUR_TER,NREAL,CWORK)
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'EHQ',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the detailed values
!
      IF( DET_BURDEN ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'BURDEN',UNITS_STA_BUR_AQ,CVEC )
      END IF
      IF( DET_EHQ ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'EHQ', UNITS_NONE, EHQ )
      IF( DET_TFOODS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_STA_BUR_TER, CWORK )
!
! *** Optionally write the food concentration for humans to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold limits)
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_STA_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_STA_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Environmental hazard quotient
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_STA_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NS_QP

      SUBROUTINE NS_TA( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Nonradioactive (stable)
!!      Species: Terrestrial, Animal
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 15 Mar 1999 : Version 1.2
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 15 May 2002 : Change debug writes
!!    Paul W. Eslinger : 28 Apr 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 26 Oct 2004 : Add optional secondary transfer factor
!!    Paul W. Eslinger : 20 May 2005 : Add particulate fraction for far-field air
!!    Paul W. Eslinger : 30 Jun 2005 : Add error checking on energy intake
!!    Paul W. Eslinger :  1 Sep 2005 : Change debug outputs
!!    Paul W. Eslinger :  6 Sep 2006 : (SCR-1140) Reformat outputs
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NS_TA' ! Name of this subroutine
!
      INTEGER :: IREL ! Realization index
      INTEGER :: JSPC ! Species index
!
      REAL :: DDERMS  ! Applied daily dose from dermal contact with soil
      REAL :: DDERMW  ! Applied daily dose from dermal contact with water
      REAL :: DIV    ! Applied daily dose from vapor inhalation
      REAL :: DIP    ! Applied daily dose from particulate inhalation
      REAL :: CIV    ! Equilibrium body burden from vapor inhalation
      REAL :: CIP    ! Equilibrium body burden from particulate inhalation
!
      REAL :: DINGW  ! Applied daily dose from water ingestion
      REAL :: CINGW  ! Equilibrium body burden from water ingestion
      REAL :: DINGS  ! Applied daily dose from soil ingestion
      REAL :: CINGS  ! Equilibrium body burden from soil ingestion
      REAL :: DINGF  ! Applied daily dose from food ingestion
      REAL :: CINGF  ! Equilibrium body burden from food ingestion
!
      REAL :: SUMPROD ! Temporary sum variable
      REAL :: PROD    ! Temporary - metabilizable energy from one prey species
      REAL :: CONJ   ! Concentration in prey species
      REAL :: NIR    ! Normalized intake rate
!
      INTEGER :: NREC ! Record number for writing to the foods file
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGNSTA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//,A,' - Detailed Equation Calculations'/ &
          '--------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Nonradioactive'/ &
          'Species : ',A,' : Terrestrial, Animal       '/ &
          'Time    : ',I0 / &
          '--------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) FWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
! ***   Calculate Dermal Contact with Soil (absorbed daily dose)
        DDERMS = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCS * &
          ESD_SPC(ISPC)%SADHER * EC(IREL)%SOIL * CFDERMS * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * &
          ( ESD_SPC(ISPC)%ALPHADS(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT )
!
! ***   Calculate Dermal Contact with Water (absorbed daily dose)
        DDERMW = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCW * &
          ESD_SPC(ISPC)%ETWATER * EC(IREL)%WATER * CFDERMW * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * &
          ( ESD_SPC(ISPC)%ALPHADW(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT )
!
! ***   Calculate Total Dermal Dose (absorbed daily dose)
        DDER(IREL) = DDERMS + DDERMW
!
! ***   Equivalent body burden  from dermal exposure
        CDER(ISPC,IREL) = DDER(IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL)
!
        IF( BGNSTA ) THEN
          WRITE(IRPT,1010) IREL, EC(IREL)%SOIL, ESD_SPC(ISPC)%SA, ESD_SPC(ISPC)%PCS, ESD_SPC(ISPC)%SADHER, &
            ESD_SPC(ISPC)%THETA, ESD_SPC(ISPC)%PSI, ESD_SPC(ISPC)%ALPHADS(IANA,IREL), ESD_SPC(ISPC)%WEIGHT, DDERMS, &
            ESD_SPC(ISPC)%PCW, ESD_SPC(ISPC)%ETWATER, EC(IREL)%WATER, ESD_SPC(ISPC)%ALPHADW(IANA,IREL), DDERMW, &
            DDER(IREL), ESD_SPC(ISPC)%DEPRATE(IANA,IREL), CDER(ISPC,IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : ug/kg                        : EC%SOIL            : Concentration of contaminant in soil'/ &
            3X,E12.5,' : cm^2                         : ESD_SPC%SA         : Surface area of animal species'/ &
            3X,E12.5,' : 1/day                        : ESD_SPC%PCS        : Fraction of species surface area contacting soil/day'/&
            3X,E12.5,' : mg/cm^2                      : ESD_SPC%SADHER     : Skin adherence factor for animal species'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%THETA      : Area use factor (contaminated/home-range area)'/&
            3X,E12.5,' : unitless                     : ESD_SPC%PSI        : Seasonality factor (fraction of year in area)'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%ALPHADS    : Soil dermal absorption factor (contaminant, species)'/&
            3X,E12.5,' : kg wet                       : ESD_SPC%WEIGHT     : Body weight of animal species'/ &
            3X,E12.5,' : ug/kg/day                    : DDERMS             : Absorbed daily dose from dermal contact with soil'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%PCW        : Fraction of species surface area contacting water/dy'/&
            3X,E12.5,' : hr/day                       : ESD_SPC%ETWATER    : Average exposure time to water per day'/ &
            3X,E12.5,' : ug/L                         : EC%WATER           : Concentration of contaminant in water'/ &
            3X,E12.5,' : cm/hr                        : ESD_SPC%ALPHADW    : Water dermal absorption factor(contaminant, species)'/&
            3X,E12.5,' : ug/kg/day wet                : DDERMW             : Absorbed daily dose from dermal contact with water'/&
            3X,E12.5,' : ug/kg/day wet                : DDER               : Absorbed daily dose from dermal contact'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%DEPRATE    : Depuration rate of contaminant by animal species'/&
            3X,E12.5,' : ug/kg wet                    : CDER               : Equilibrium body burden from dermal exposure')
        END IF
!
! ***   Calculate Inhalation Dose from Vapor
        IF( ESD_ANA(IANA)%HENRY>0.0) THEN
!
!         Absorbed daily dose from inhalation of vapor
          DIV = ((ESD_SPC(ISPC)%INHRATE * (EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)+EC(IREL)%VAPOR) ) / ESD_SPC(ISPC)%WEIGHT) * &
            ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equivalent body burden from inhalation of vapor
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGNSTA ) THEN
            WRITE(IRPT,1030) EC(IREL)%AIRC, ESD_ANA(IANA)%PARTICLE, ESD_ANA(IANA)%HENRY, EC(IREL)%VAPOR, ESD_SPC(ISPC)%INHRATE, &
              ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL), DIV, CIV
 1030       FORMAT( 1P, &
              3X,E12.5,' : ug/m^3                       : EC%AIRC            : Concentration of contaminant in air'/ &
              3X,E12.5,' : ug/m^3                       : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : Pa-m^3/mol                   : ESD_ANA%HENRY      : Henrys law constant'/&
              3X,E12.5,' : ug/m^3                       : EC%VAPOR           : Concentration of contaminant in vapor'/ &
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : unitless                     : ESD_SPC%ALPHAVAP   : Inhalation absorption factor of contaminant'/ &
              3X,E12.5,' : ug/kg/day wet                : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : ug/kg wet                    : CIV                : Equilibrium body burden from vapor inhalation')
          END IF
        ELSE
!
!         Absorbed daily dose from inhalation of vapor
          DIV = ((ESD_SPC(ISPC)%INHRATE * EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)) / ESD_SPC(ISPC)%WEIGHT) * &
            ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equivalent body burden from inhalation of vapor
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGNSTA ) THEN
            WRITE(IRPT,1040) ESD_SPC(ISPC)%INHRATE, EC(IREL)%AIRC, ESD_ANA(IANA)%PARTICLE,  DIV, CIV
 1040       FORMAT( 1P, &
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : ug/m^3                       : EC%AIRC            : Concentration of contaminant in air'/ &
              3X,E12.5,' : ug/m^3                       : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : ug/kg/day wet                : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : ug/kg wet                    : CIV                : Equilibrium body burden from vapor inhalation')
          END IF
        END IF
!
!       Absorbed daily dose from inhalation of particulates
        DIP = ESD_SPC(ISPC)%INHRATE * (EC(IREL)%PARTIC+EC(IREL)%AIRC*ESD_ANA(IANA)%PARTICLE) * ESD_SPC(ISPC)%THETA * &
          ESD_SPC(ISPC)%PSI / ESD_SPC(ISPC)%WEIGHT
!
!       Equivalent body burden from inhalation of particulates
        CIP = DIP * ( ESD_SPC(ISPC)%ALPHAPAR(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
! ***   Total absorbed daily dose from inhalation
        DINH(IREL) = DIV + DIP
!
!       Total equivalent body burden from inhalation
        CINH(ISPC,IREL) = CIV + CIP
        IF( BGNSTA ) THEN
          WRITE(IRPT,1050) DIP, CIP, DINH(IREL), CINH(ISPC,IREL)
 1050     FORMAT( 1P, &
            3X,E12.5,' : ug/kg/day wet                : DIP                : Applied daily dose from particulate inhalation'/ &
            3X,E12.5,' : ug/kg wet                    : CIP                : Equilibrium body burden from particulate inhalation'/ &
            3X,E12.5,' : ug/kg/day wet                : DINH               : Applied daily dose from all inhalation'/ &
            3X,E12.5,' : ug/kg wet                    : CINH               : Equilibrium body burden from all inhalation')
        END IF
!
! ***   Absorbed daily dose from water ingestion
        DINGW = ( ( ESD_SPC(ISPC)%WATERING * EC(IREL)%WATER ) / ESD_SPC(ISPC)%WEIGHT ) * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!       Equivalent body burden from water ingestion
        CINGW = DINGW * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
        IF( BGNSTA ) THEN
          WRITE(IRPT,1060) ESD_SPC(ISPC)%WATERING, EC(IREL)%WATER, DINGW, CINGW
 1060     FORMAT( 1P, &
            3X,E12.5,' : L/day                        : ESD_SPC%WATERING   : Water ingestion rate for animal species'/ &
            3X,E12.5,' : ug/kg                        : EC%WATER           : Concentration of contaminant in water'/ &
            3X,E12.5,' : ug/kg/day wet                : DINGW              : Applied daily dose from water ingestion'/ &
            3X,E12.5,' : ug/kg wet                    : CINGW              : Equilibrium body burden from water ingestion')
        END IF
!
! ***   Calculate the total metabilizable energy from food (kcal/kg prey wet wt) [Use variable SUMPROD]
!       This is used in both food ingestion and soil ingestion calculations
        IF( BGNSTA ) THEN
          WRITE(IRPT,1070) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1070     FORMAT(/'   Species ',A,' (',A,') Metabilizable Energy From Prey'/&
            3X,'Prey   Predation',13X,'Gross Energy',20X,'Assimilation Efficiency',1X,'Metabilizable Energy'/&
            3X,'------',1X,21('-'),1X,31('-'),1X,23('-'),1X,32('-'))
        END IF
!
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate the metabilizable energy this prey (kcal/kg prey wet wt)
          PROD = PREDATE(ISPC,JSPC) * ESD_SPC(JSPC)%GE * ESD_SPC(JSPC)%AE
          IF( PROD .LE. 0.0 ) THEN
            IERR = 2
            MESSAG(1) = 'Food is ingested but no metabilizable energy is defined'
            MESSAG(2) = 'Location '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Species  '//ESD_SPC(ISPC)%ID //' Eating '//ESD_SPC(JSPC)%ID
            MESSAG(5) = 'Check the AE and GE values on the SPECIES keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Sum the metabilizable energy over all prey
          SUMPROD = SUMPROD + PROD
!
          IF( BGNSTA ) THEN
            WRITE(IRPT,1080) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(JSPC)%GE, ESD_SPC(JSPC)%AE, PROD
 1080       FORMAT(1P,3X,A, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kcal/kg wet weight', &
              1X,E12.5,' unitless', &
              3X,E12.5,' kcal/kg prey wet wt')
          END IF
        END DO
!
!       Total normalized intake rate (kg prey wet weight/kg predator body weight/day)
        IF( SUMPROD .GT. 0.0 ) THEN
          ESD_SPC(ISPC)%NIRTOT(IREL) = ESD_SPC(ISPC)%FMR / ( SUMPROD * ESD_SPC(ISPC)%WEIGHT )
        ELSE
          ESD_SPC(ISPC)%NIRTOT(IREL) = 0.0
        END IF
!
! ***    Calculate food ingestion dose from all species eaten
        IF( BGNSTA ) THEN
          WRITE(IRPT,1090) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1090     FORMAT(/'   Species ',A,' (',A,') Food ingestion calculations'/&
            3X,'Prey   Predation',13X,'Normalized Intake',32X,'Prey Concentration'/&
            3X,'------',1X,21('-'),1X,48('-'),1X,31('-'))
        END IF
!
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate normalized intake rate for this prey species
          NIR = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%NIRTOT(IREL)
!
!         Obtain the concentration of contaminant in the prey species
          IF( ESD_SPC(JSPC)%TYPE.EQ.'TP' .OR. ESD_SPC(JSPC)%TYPE.EQ.'QP' ) THEN
            CONJ = CPAT(JSPC,IREL)
          ELSE
            CONJ = CING(JSPC,IREL) + CINH(JSPC,IREL) + CDER(JSPC,IREL)
          END IF
!
!         Add the contaminant intake for this prey species to the total intake
          SUMPROD = SUMPROD + (CONJ * NIR)
!
          IF( BGNSTA ) THEN
            WRITE(IRPT,1110) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), NIR, CONJ
 1110       FORMAT(3X,A,1P, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kg prey wet wt/kg predator wet wt/d', &
              1X,E12.5,' ug/kg prey wet wt')
          END IF
!
        END DO
!
!       Applied daily dose from all prey consumption (ug/kg/day)
        DINGF = SUMPROD * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!       Equilibrium body burden from all prey consumption (ug/kg body wet weight)
        CINGF = DINGF * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
        IF( BGNSTA ) THEN
          WRITE(IRPT,1120) DINGF, CINGF
 1120     FORMAT(/ 1P, &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : ug/kg/day                    : DINGF              : Applied daily dose from food ingestion'/ &
            3X,E12.5,' : ug/kg                        : CINGF              : Equilibrium body burden from food ingestion')
        END IF
!
! ***   Calculate Soil Ingestion Dose
        IF( ESD_SPC(ISPC)%NIRTOT(IREL) .GT. 0.0 ) THEN
!
!         Applied daily dose from soil consumption (ug/kg/day)
          DINGS = ESD_SPC(ISPC)%SOILING * ESD_SPC(ISPC)%NIRTOT(IREL) * EC(IREL)%SOIL * &
            ESD_SPC(ISPC)%FDW * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equilibrium body burden from soil consumption (ug/kg body wet weight)
          CINGS = DINGS * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGNSTA ) THEN
            WRITE(IRPT,1130) ESD_SPC(ISPC)%FMR, ESD_SPC(ISPC)%NIRTOT(IREL), ESD_SPC(ISPC)%SOILING, EC(IREL)%SOIL, &
              ESD_SPC(ISPC)%FDW, DINGS, CINGS
 1130       FORMAT(1P, &
              3X,E12.5,' : kcal/day                     : ESD_SPC%FMR        : free-living metabolic rate of predator species'/ &
              3X,E12.5,' : kg prey wt/kg predator wt/d  : ESD_SPC%NIRTOT     : Total normalized intake rate'/ &
              3X,E12.5,' : kg soil ingested/kg dry diet : ESD_SPC%SOILING    : Soil ingestion rate'/ &
              3X,E12.5,' : ug/kg                        : EC%SOIL            : Concentration of contaminant in soil'/ &
              3X,E12.5,' : kg dry/kg wet                : ESD_SPC%FDW        : Conversion factor, dry diet to wet diet'/ &
              3X,E12.5,' : ug/kg/day wet                : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : ug/kg wet                    : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
 !
        ELSE ! No soil
!
!         Applied daily dose from soil consumption (ug/kg/day)
          DINGS = 0.0
!
!         Equilibrium body burden from soil consumption (ug/kg body wet weight)
          CINGS = 0.0
!
          IF( BGNSTA ) THEN
            WRITE(IRPT,1140) DINGS, CINGS
 1140       FORMAT( 1P, &
              3X,E12.5,' : ug/kg/day wet                : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : ug/kg wet                    : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
!
       END IF
!
! ***   Applied daily dose from all sources (ug/kg/day)
        DING(IREL) = DINGF + DINGS + DINGW
!
!       Equilibrium body burden from all sources (ug/kg body wet weight)
        CING(ISPC,IREL) = CINGF + CINGS + CINGW
!
! ***   Calculate Total Absorbed Dose
        DHAZ(IREL)  = DING(IREL) + DINH(IREL) + DDER(IREL)
        CWORK(IREL) = CING(ISPC,IREL) + CINH(ISPC,IREL) + CDER(ISPC,IREL)
        IF( ESD_SPC(ISPC)%TRANS ) CWORK(IREL) = CWORK(IREL)*ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
!
!       Optional environmental hazard quotient
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) EHQ(IREL) = CWORK(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
! ***   Save the concentration for human consumption (wet weight basis)
        FWORK(IREL) = CWORK(IREL)
!
        IF( BGNSTA ) THEN
          WRITE(IRPT,1150) DING(IREL), CING(ISPC,IREL), DHAZ(IREL), CWORK(IREL), FWORK(IREL)
 1150     FORMAT( 1P, &
            3X,E12.5,' : ug/kg/day wet                : DING               : Applied daily dose from all ingestion'/ &
            3X,E12.5,' : ug/kg wet                    : CING               : Equilibrium body burden from all ingestion'/ &
            3X,E12.5,' : ug/kg/day wet                : DHAZ               : Total absorbed daily dose'/ &
            3X,E12.5,' : ug/kg wet                    : CWORK              : Total equilibrium body burden'/ &
            3X,E12.5,' : ug/kg wet                    : FWORK              : Total equilibrium body burden for human consumption')
          IF( ESD_SPC(ISPC)%TRANS ) WRITE(IRPT,1160) ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
 1160     FORMAT( 1P, &
            3X,E12.5,' : ug second/ug primary         : TRANSVAL           : Transfer factor')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1170) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1170     FORMAT(1P, &
            3X,E12.5,' : ug/kg wet                    : ESD_SPC%BMTISS     : Tissue benchmark' /&
            3X,E12.5,' : unitless                     : EHQ                : Environmental hazard quotient')
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGNSTA .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_STA_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSDER',UNITS_STA_DOS_TER,NREAL,DDER)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSINH',UNITS_STA_DOS_TER,NREAL,DINH)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSING',UNITS_STA_DOS_TER,NREAL,DING)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSHAZ',UNITS_STA_DOS_TER,NREAL,DHAZ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the computed detailed values
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_STA_BUR_TER, CWORK )
      IF( DET_DOSDER ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSDER', UNITS_STA_DOS_TER, DDER )
      IF( DET_DOSINH ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSINH', UNITS_STA_DOS_TER, DINH )
      IF( DET_DOSING ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSING', UNITS_STA_DOS_TER, DING )
      IF( DET_DOSHAZ ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSHAZ', UNITS_STA_DOS_TER, DHAZ )
      IF( DET_EHQ ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,ESD_LOC(ILOC)%ID,SOIL_ID,ESD_ANA(IANA)%ID,&
        ESD_SPC(ISPC)%ID,'EHQ',UNITS_NONE,EHQ )
!
! *** Optionally write the burden (as consumed) to the food file
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Optionally write out statistics on body burden and threshold calculations
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_STA_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_STA_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out statistics on dermal dose and threshold calculations
      IF( STA_DOSDER .OR. THRESHOLD%EXIST1(ISPC,IANA,2) .OR. THRESHOLD%EXIST2(ISPC,IANA,2) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,2)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,2)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,2)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,2)
        CALL USTAT_ECEM( DDER, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSDER'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSDER ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSDER', UNITS_STA_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSDER', UNITS_STA_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out statistics on inhalation dose and threshold calculations
      IF( STA_DOSINH .OR. THRESHOLD%EXIST1(ISPC,IANA,4) .OR. THRESHOLD%EXIST2(ISPC,IANA,4) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,4)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,4)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,4)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,4)
        CALL USTAT_ECEM( DINH, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSINH'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSINH ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSINH', UNITS_STA_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSINH', UNITS_STA_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out statistics on ingestion dose and threshold calculations
      IF( STA_DOSING .OR. THRESHOLD%EXIST1(ISPC,IANA,3) .OR. THRESHOLD%EXIST2(ISPC,IANA,3) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,3)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,3)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,3)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,3)
        CALL USTAT_ECEM( DING, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSING'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSING ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSING', UNITS_STA_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSING', UNITS_STA_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out statistics on total dose
      IF( STA_DOSHAZ ) THEN
        CALL USTAT( DHAZ, NREAL, TWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSHAZ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSHAZ', UNITS_STA_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Environmental hazard quotient statistics
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NS_TA

      SUBROUTINE NS_TP( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Inorganic, Nonradioactive (stable)
!!      Species: Terrestrial, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 15 Mar 1999 : Version 1.2
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 28 Jul 2004 : SCR-1063 Replace TUPTAKE with FW
!!    Paul W. Eslinger : 18 Nov 2004 : Change leaf deposition equations
!!    Paul W. Eslinger :  6 Dec 2004 : Change dependencies for IFRACDRY,
!!                                     IFRACWET, LWEATHDRY, and LWEATHWET.
!!    Paul W. Eslinger : 17 Jan 2005 : Wet leaf deposition is applied
!!                                     only for UPLAND locations
!!    Paul W. Eslinger : 20 Jan 2005 : Adjust leaf deposition totals
!!    Paul W. Eslinger :  9 Feb 2005 : Adjust leaf deposition totals again
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Adjust leaf deposition totals again
!!    Paul W. Eslinger : 15 Jun 2005 : SCR-1080 Change HUMAN food options
!!    Paul W. Eslinger : 27 Jan 2006 : Correct KPA1 equation for KOW being in log units
!!                                     Remove H3 logic - it is not a stable analyte
!!    Paul W. Eslinger : 17 Aug 2006 : (SCR-1140) Change to EHQ & reformat
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'NS_TP' ! Name of this subroutine
      INTEGER :: IREL ! Realization index
!
      REAL :: TMPCPAR ! Concentration on leaf surface from rain splash
      REAL :: TMPCPAD ! Concentration on leaf surface from dry deposition
      REAL :: TMPCPAU ! Concentration in plant from root uptake
      REAL :: TMPCPAV ! Concentration in plant from vapor uptake
      REAL :: TMPCPAP ! Concentration on leaf surface from particulate deposition
      REAL :: TMPCPAW ! Concentration on leaf surface from wet deposition
!
      REAL :: KPA1    ! plant-air partition coefficient to above-ground parts of plant
!
      INTEGER :: NREC ! Record number for writing to the foods file
      REAL :: PCT     ! Temporary percent variable
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGNSTP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '--------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Inorganic, Nonradioactive'/ &
          'Species : ',A,' : Terrestrial, Plant        '/ &
          'Time    : ',I0 / &
          '--------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
!
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine COMPUTE_VAPOR_SOIL'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine COMPUTE_PARTIC'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
! ***   Calculate rain splash effect on leaf external surface
        IF( ESD_SPC(ISPC)%EMERGENT ) THEN
          TMPCPAR = 0.0
          IF( BGNSTP ) THEN
            WRITE(IRPT,1010) IREL, TMPCPAR
 1010       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value     : Units                : Variable           : Description'/ &
              3X,12('-'),3X,20('-'),3X,18('-'),3X,50('-')  /&
              3X, E12.5,' : ug/kg wet            : TMPCPAR            : Rain splash concentration on leaf surface')
          END IF
        ELSE
          TMPCPAR = EC(IREL)%SOIL * ESD_SPC(ISPC)%KPS1(IREL)
          IF( BGNSTP ) THEN
            WRITE(IRPT,1020) IREL, ESD_SPC(ISPC)%KPS1(IREL), TMPCPAR
 1020       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value     : Units                : Variable           : Description'/ &
              3X,12('-'),3X,20('-'),3X,18('-'),3X,50('-')  /&
              3X, E12.5,' : kg soil/kg plant wet : ESD_SPC%KPS1       : Plant-soil partition coefficient for rain splash'/ &
              3X, E12.5,' : ug/kg wet            : TMPCPAR            : Rain splash concentration on leaf surface')
          END IF
        END IF
!
! ***   Calculate dry deposition effect on leaf external surface
        TMPCPAD = EC(IREL)%AIRD * ESD_SPC(ISPC)%IFRACDRY(IANA,IREL) / &
                  (ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL) )
        IF( BGNSTP ) THEN
          WRITE(IRPT,1030) EC(IREL)%AIRD, ESD_SPC(ISPC)%IFRACDRY(IANA,IREL), ESD_SPC(ISPC)%BIOMASS(IREL), &
            ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL), TMPCPAD
 1030     FORMAT(1P, &
            3X, E12.5,' : kg/m^2/yr            : EC%AIRD            : Air deposition'/ &
            3X, E12.5,' : unitless             : ESD_SPC%IFRACDRY   : Leaf intersection fraction - dry deposition'/ &
            3X, E12.5,' : kg/m^2               : ESD_SPC%BIOMASS    : Standing plant biomass per unit area'/ &
            3X, E12.5,' : 1/yr                 : ESD_SPC%LWEATHDRY  : Leaf weathering rate - dry deposition'/ &
            3X, E12.5,' : ug/kg                : TMPCPAD            : Concentration in above-ground plant from air deposition')
        END IF
!
! ***   Calculate wet deposition effect on leaf external surface (only for upland irrigated locations)
        IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
          TMPCPAW = EC(IREL)%WATER * (IRIG_AMT/SEASON_GROW) * ESD_SPC(ISPC)%IFRACWET(IANA,IREL) * 10.0 / &
                    ( ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHWET(IANA,IREL) )
          IF( BGNSTP ) THEN
            WRITE(IRPT,1040) EC(IREL)%WATER, IRIG_AMT, SEASON_GROW, ESD_SPC(ISPC)%IFRACWET(IANA,IREL), &
              ESD_SPC(ISPC)%LWEATHWET(IANA,IREL), TMPCPAW
 1040       FORMAT(1P, &
              3X, E12.5,' : ug/L                 : EC%WATER           : Irrigation water concentration'/ &
              3X, E12.5,' : cm                   : IRIG_AMT           : Amount of irrigation water applied in a year'/&
              3X, E12.5,' : yr                   : SEASON_GROW        : Length of the growing season'/&
              3X, E12.5,' : unitless             : ESD_SPC%IFRACWET   : Leaf intersection fraction - wet deposition'/ &
              3X, E12.5,' : 1/yr                 : ESD_SPC%LWEATHWET  : Leaf weathering rate - wet deposition'/ &
              3X, E12.5,' : ug/kg wet            : TMPCPAW            : Concentration in above-ground plant from irrigation water')
          END IF
        ELSE
          TMPCPAW = 0.0
          IF( BGNSTP ) THEN
            WRITE(IRPT,1050) TMPCPAW
 1050       FORMAT(1P, &
              3X, E12.5,' : ug/kg wet            : TMPCPAW            : Concentration in above-ground plant from irrigation water')
          END IF
        END IF
!
! ***   Calculate local particulate deposition effect on leaf external surface
        TMPCPAP = EC(IREL)%PARTIC * ESD_SPC(ISPC)%KPA2(IREL)
        IF( BGNSTP ) THEN
          WRITE(IRPT,1060) EC(IREL)%PARTIC, ESD_SPC(ISPC)%KPA2(IREL), TMPCPAP
 1060     FORMAT(1P, &
            3X, E12.5,' : ug/m^3               : EC%PARTIC          : Particulate concentration in air'/ &
            3X, E12.5,' : m^3/kg wet           : ESD_SPC%KPA2       : Plant-air partition coefficient for above-ground plant'/&
            3X, E12.5,' : ug/kg wet            : TMPCPAP            : Concentration of contaminant in above-ground plant')
        END IF
!
! ***   Calculate Root Uptake
        IF( ESD_SPC(ISPC)%EMERGENT ) THEN
          TMPCPAU = EC(IREL)%SEDI * ESD_SPC(ISPC)%BCFVEG(IANA,IREL) * ( 1.0 - ESD_SPC(ISPC)%FW )
        ELSE
          TMPCPAU = EC(IREL)%SOIL * ESD_SPC(ISPC)%BCFVEG(IANA,IREL) * ( 1.0 - ESD_SPC(ISPC)%FW )
        END IF
        IF( BGNSTP ) THEN
          WRITE(IRPT,1070) EC(IREL)%SOIL, EC(IREL)%SEDI, ESD_SPC(ISPC)%BCFVEG(IANA,IREL), ESD_SPC(ISPC)%FW, TMPCPAU
 1070     FORMAT(1P, &
            3X, E12.5,' : ug/kg                : EC%SOIL            : Soil concentration'/ &
            3X, E12.5,' : ug/kg                : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : kg-soil/kg plant dry : ESD_SPC%BCFVEG     : Bioconcentration factor'/ &
            3X, E12.5,' : unitless             : ESD_SPC%FW         : Weight fraction of plant that is water'/&
            3X, E12.5,' : ug/kg wet            : TMPCPAU            : Concentration in above-ground plant from root uptake')
        END IF
!
! ***   Calculate foliar uptake from vapor
        IF( ESD_ANA(IANA)%HENRY > 0.0 ) THEN
!
          KPA1 = ( ESD_SPC(ISPC)%FPA + ( ESD_SPC(ISPC)%FPW + ESD_SPC(ISPC)%FPL * &
            (10.0**ESD_ANA(IANA)%KOW(IREL)) ) * &
            (( GASCON * ( ESD_LOC(ILOC)%TEMP + 273.15 )) / ESD_ANA(IANA)%HENRY )) * &
            ( 1.0 / ESD_SPC(ISPC)%RHOP )
!
          TMPCPAV = (EC(IREL)%VAPOR+EC(IREL)%AIRC) * KPA1
!
          IF( BGNSTP ) THEN
            WRITE(IRPT,1080) ESD_SPC(ISPC)%FPA, ESD_SPC(ISPC)%FPW, ESD_SPC(ISPC)%FPL, ESD_ANA(IANA)%KOW(IREL), &
              GASCON, ESD_LOC(ILOC)%TEMP, ESD_ANA(IANA)%HENRY, ESD_SPC(ISPC)%RHOP, EC(IREL)%VAPOR, &
              EC(IREL)%AIRC, KPA1, TMPCPAV
 1080       FORMAT(1P, &
              3X, E12.5,' : unitless             : ESD_SPC%FPA        : Volume fraction of plant species tissue is air'/&
              3X, E12.5,' : unitless             : ESD_SPC%FPW        : Volume fraction of plant tissue that is water'/&
              3X, E12.5,' : unitless             : ESD_SPC%FPL        : Volume fraction of plant tissue that is lipid'/&
              3X, E12.5,' : unitless             : ESD_ANA%KOW        : log base 10 of octanol-water partition coefficient'/&
              3X, E12.5,' : Pa-m^3/mol-K         : GASCON             : Universal gas constant'/&
              3X, E12.5,' : Centigrade           : ESD_LOC%TEMP       : Temperature'/&
              3X, E12.5,' : Pa-m^3/mol           : ESD_ANA%HENRY      : Henrys law constant'/&
              3X, E12.5,' : kg/m^3 wet           : ESD_SPC%RHOP       : Plant tissue density'/&
              3X, E12.5,' : ug/m^3               : EC%VAPOR           : Vapor concentration in gas phase'/&
              3X, E12.5,' : ug/m^3               : EC%AIRC            : Air concentration'/&
              3X, E12.5,' : m^3/kg wet weight    : KPA1               : Plant-air partition coefficient to above-ground parts'/&
              3X, E12.5,' : ug/kg wet            : TMPCPAV            : Concentration in above-ground plant from vapor uptake')
          END IF
!
        ELSE
!         No vapor concentration when Henry's coefficient is zero
          TMPCPAV = 0.0
          IF( BGNSTP ) THEN
            WRITE(IRPT,1090) TMPCPAV
 1090       FORMAT(1P, &
              3X, E12.5,' : ug/kg wet            : TMPCPAV            : Concentration in above-ground plant from vapor uptake')
          END IF
        END IF
!
! ***   Calculate internal plant burden
!       Vapor uptake + Root uptake + Leaf absorption (dry) + Leaf absorption (wet)
        CPAI(ISPC,IREL) = TMPCPAV + TMPCPAU + (TMPCPAR+TMPCPAP+TMPCPAD)*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) + &
          TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL)
!
! ***   Calculate total plant burden (as consumed by predators)
!       Leaf surface (rain + particulates) + internal burden
        CPAT(ISPC,IREL) = TMPCPAV + TMPCPAU + TMPCPAR + TMPCPAP + TMPCPAD + TMPCPAW
!
! ***   Save internal plant body burden
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = CPAI(ISPC,IREL)
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
!
!       Optional environmental hazard quotient
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          EHQ(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
! ***   Save the concentration for human consumption (wet weight basis)
        IF( ESD_SPC(ISPC)%INTERNAL ) THEN
          FWORK(IREL) = CPAI(ISPC,IREL)
        ELSE
          FWORK(IREL) = CPAT(ISPC,IREL)
        END IF
!
        IF( BGNSTP ) THEN
          WRITE(IRPT,1100) ESD_SPC(ISPC)%KLIDRY(IANA,IREL), ESD_SPC(ISPC)%KLIWET(IANA,IREL), CPAI(ISPC,IREL), CPAT(ISPC,IREL), &
            ESD_SPC(ISPC)%BODYBURDEN(IREL), FWORK(IREL), ESD_SPC(ISPC)%INTERNAL
 1100     FORMAT(1P, &
            3X, E12.5,' : unitless             : ESD_SPC%KLIDRY     : Leaf to internal plant transfer factor for dry deposition'/&
            3X, E12.5,' : unitless             : ESD_SPC%KLIWET     : Leaf to internal plant transfer factor for wet deposition'/&
            3X, E12.5,' : ug/kg wet            : CPAI               : Internal plant burden'/&
            3X, E12.5,' : ug/kg wet            : CPAT               : Total plant burden (as consumed by predators)'/&
            3X, E12.5,' : ug/kg wet            : ESD_SPC%BODYBURDEN : Plant body burden'/&
            3X, E12.5,' : ug/kg wet            : FWORK              : Concentration for human consumption'/&
            3X, L12  ,' : none                 : ESD_SPC%INTERNAL   : Flag for internal (true) or total concentration - human food')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1170) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1170     FORMAT(1P,&
            3X, E12.5,' : ug/kg wet            : ESD_SPC%BMTISS     : Tissue benchmark' /&
            3X, E12.5,' : unitless             : EHQ                : Environmental hazard quotient')
        END IF
!
        IF( BGNSTP ) THEN
!
          WRITE(IRPT,1220) '   Internal plant burden (ug/kg wet plant weight) for '//TRIM(ESD_ANA(IANA)%ID)//' on '//&
            TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from rainsplash'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from soil particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from ambient air particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL), PCT, 'Foliar adsorption from wet deposition'
          WRITE(IRPT,1240) CPAI(ISPC,IREL), 'Total internal plant burden'
 1220     FORMAT(/A)
 1230     FORMAT(3X,1P,E12.5,0P,' : ',F8.4,'% : ',A)
 1240     FORMAT(3X,1P,E12.5,' : ',A)
!
          WRITE(IRPT,1220) '   Plant burden (ug/kg wet plant weight) as consumed by predators for '//&
            TRIM(ESD_ANA(IANA)%ID)//' on '//TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR, PCT, 'Rainsplash to above ground plant parts'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP, PCT, 'Foliar interception from soil particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD, PCT, 'Foliar interception ambient air particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW, PCT, 'Foliar interception from wet deposition'
          WRITE(IRPT,1240) CPAT(ISPC,IREL), 'Total plant burden'
!
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
!     After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
!
      IF( BGNSTP .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_STA_BUR_TER,NREAL,CWORK)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
!
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the computed values
!
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_STA_BUR_TER, CWORK )
      IF( DET_EHQ .AND. ESD_SPC(ISPC)%OUTPUT ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,ESD_LOC(ILOC)%ID,SOIL_ID,ESD_ANA(IANA)%ID,&
        ESD_SPC(ISPC)%ID,'EHQ',UNITS_NONE,EHQ )
!
! *** Optionally write the burden (as consumed) to the food file
!
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Optional output of body burden statistics
!
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_STA_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_STA_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Environmental hazard quotient
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE NS_TP

      SUBROUTINE OPEN_CON( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the media concentration data file and reads
!!    the header information.
!!
!!  History:
!!
!!    Paul W. Eslinger : 20 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      INTEGER :: IERR ! Error number
      INTEGER :: IANA ! Looping variable for analytes
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'OPEN_CON' ! Name of this subroutine
      LOGICAL :: THERE ! Logical flag for file existence inquire
!
!---- First executable code --------------------------------------------
!
      IERR = 0
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
! ***   Open the media concentration data file
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
!
      RETURN
      END SUBROUTINE OPEN_CON

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
!!    Paul W. Eslinger : 17 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Change output label
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
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defiend functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'OPEN_DETAIL' ! Name of this subroutine
      INTEGER :: IERF ! Status variable for open statement
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
        MESSAG(1) = 'System error opening the detailed data file'
        MESSAG(2) = 'File name entered on the FILE keyword, DETAIL modifier'
        MESSAG(3) = 'File: '//TRIM(FNDET)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Write an identification line
!
      WRITE(IDET,1000,IOSTAT=IERF) &
        '"Time","Location ID","Soil Type","Analyte ID","Species ID","Solution Type","Units","Values by realization"'
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
      END SUBROUTINE OPEN_DETAIL

      SUBROUTINE OPEN_ESD( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the ESD keyword file for reading keywords.
!!
!!  History:
!!
!!    Paul W. Eslinger : 21 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
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
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error number
      INTEGER :: IERF ! Status variable for open statement
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'OPEN_ESD' ! Name of this subroutine
      LOGICAL :: THERE ! Logical flag for file existence inquire
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
      END SUBROUTINE OPEN_ESD

      SUBROUTINE OPEN_FOODS( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine creates the names for all of the required food
!!    concentration data files, creates the files, and writes the
!!    header information.  There is a separate file for every
!!    combination of analyte being processed and every species with
!!    it's output flag set.
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 16 Aug 2006 : Revise outputs
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      USE Debug_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error number
      INTEGER :: IANA ! Looping variable for analytes
      INTEGER :: ISPC ! Looping variable for species
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'OPEN_FOODS' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      IF( BGFCDA ) THEN
        WRITE(IRPT,'(A)') ' '
        WRITE(IRPT,'(A)') 'Creating food files'
      END IF
!
! *** Loop over all possible analytes
!
      DO IANA = 1, ESD_NUM_ANA
!
! ***   Only look at analytes selected for this scenario
        IF( .NOT.(ESD_ANA(IANA)%COMP.AND.ESD_ANA(IANA)%OUTPUT) ) CYCLE
!
        IF( BGFCDA ) WRITE(IRPT,'(A)') '!'
!
! ***   Loop over all the possible species
!
        DO ISPC = 1, ESD_NUM_SPC
!
! ***     Only look at species with output selected
          IF( .NOT.ESD_SPC(ISPC)%COMP ) CYCLE
          IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) CYCLE
!
! ***     Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER(  )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 1
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
! ***     Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          IF( BGFCDA ) WRITE(IRPT,'(A,I3,A,A,A)') '  Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
            '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
! ***     Create the file and write header lines
          CALL FCDA_CREATE( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), ESD_ANA(IANA)%ID, &
            ESD_SPC(ISPC)%ID, ESD_SPC(ISPC)%HABITAT, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 2
            MESSAG(1) = 'System error creating the food concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
        END DO ! Species loop
!
      END DO ! Analyte loop
!
      RETURN
      END SUBROUTINE OPEN_FOODS

      SUBROUTINE OPEN_KDSOIL( IERR )
!!**********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the input library file that contains the KD data.
!!    It also reads and stores the data.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Adapted from the SOIL code
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in opening the file
!!
!!**********************************************************************************
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
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error flag
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'OPEN_KDSOIL' ! Name of this subroutine
!
      INTEGER :: IANA ! Local analyte index
      INTEGER :: IREL ! Local realization index
      INTEGER :: I    ! Local looping variable
      INTEGER :: J    ! Local looping variable
!
      LOGICAL :: THERE ! Temporary logical variable
      CHARACTER(LEN=20) :: TMP_UNITS ! Temporary data units label
      REAL, ALLOCATABLE :: KDWORK(:) ! Temporary KD work vector
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the requested file exists
!
      IF( FNKDS .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The soil-water KD file name is blank'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, KDSOIL modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FNKDS,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested soil-water KD data file was not found'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, KDSOIL modifier'
        MESSAG(3) = 'File: '//TRIM(FNKDS)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
!     Get a unit number
      IKDS = GET_UNIT_NUMBER(  )
      IF( IKDS .LT. 7 ) THEN
        IERR = 3
        MESSAG(1) = 'Unable to get unit number for KDSOIL data file'
        CALL PRTERR( IERR, 'GET_UNIT_NUMBER', 1 )
        RETURN
      END IF
!
      OPEN(IKDS,FILE=FNKDS,STATUS='OLD',IOSTAT=IERR)
      IF( IERR .NE. 0  ) THEN
        IERR = 4
        MESSAG(1) = 'System error opening the KD data file'
        MESSAG(2) = 'File name entered on the ESD FILE keyword, KDSOIL modifier'
        MESSAG(3) = 'File: '//TRIM(FNKDS)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read all of the header data
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
        IERR = 5
        MESSAG(1) = 'Wrong data type in the soil-water KD data file'
        MESSAG(2) = 'First entry in the file was not the string KDSOIL'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( KD_NITR .LT. NREAL ) THEN
        IERR = 6
        MESSAG(1) = 'Not enough realizations of data in the KD data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!--------------------------------------------------------------------------------------
!     Read the KD data and see if it matches with the analytes used in this run
!--------------------------------------------------------------------------------------
!
      ALLOCATE( KDWORK(KD_NITR) )
!
      DO I = 1, KD_NUM
!
! ***   Read the stochastic realizations for one KDSOIL ID
        READ(IKDS,*,ERR=999,END=999) KD_NUM, KD_ID, TMP_UNITS, (KDWORK(J),J=1,KD_NITR)
!
! ***   See if the data just read are needed, if so, store them
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
          IF( KD_MAP(IANA) .EQ. KD_ID ) THEN
            KD_UNITS(IANA) = TMP_UNITS
            DO IREL = 1, NREAL
              KD(IANA,IREL) = KDWORK(IREL)
            END DO
          END IF
        END DO
!
      END DO
!
!--------------------------------------------------------------------------------------
!     Check that all required KD's are entered
!--------------------------------------------------------------------------------------
!
      DO IANA = 1, ESD_NUM_ANA
!       Skip analytes not computed
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
!       Skip organic analytes
        IF( ESD_ANA(IANA)%TYPE(1:1) .EQ. 'O' ) CYCLE
!       Skip inorganic analytes with Henry's coeffifient = 0
        IF( ESD_ANA(IANA)%HENRY .LE. 0.0 ) CYCLE
!
!       Check inorganic analytes with Henry's coefficient > 0
        DO IREL = 1, NREAL
          IF( KD(IANA,IREL) .LT. 0.0 ) THEN
            IERR = 7
            MESSAG(1) = 'Invalid (negative) KD value encountered, Realization = '
            WRITE(MESSAG(1)(56:),'(I0)') IREL
            MESSAG(2) = 'Analyte ID = '//ESD_ANA(IANA)%ID
            MESSAG(3) = 'Option: Data not matched on a KDSOIL card'
            MESSAG(4) = 'Option: Invalid distribution on an ESD KDSOIL keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
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
      IERR = 8
      MESSAG(1) = 'Error or end of file reading header data from the KDSOIL data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
! *** Error reading the header data
!
  999 CONTINUE
      IERR = 9
      MESSAG(1) = 'Error or end of file reading data from the KDSOIL data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
      END SUBROUTINE OPEN_KDSOIL

      SUBROUTINE OPEN_KEY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine performs two actions for the keyword file:
!!      1. Obtain the file name and store it in the variable FNKEY
!!         a) First check the command line to see if the file
!!            name was provided or,
!!         b) query the user for the file name if it was not
!!            entered on the command line.
!!      2. Open the file on unit number IKEY for use by subroutine KEYWD
!!
!!  Note:
!!
!!    This subroutine does not call PRTERR when an error occurs because
!!    PRTERR writes to the report file.  The report file is not opened
!!    until the keywords are read in subroutine KEYWD.
!!
!!  History:
!!
!!    Paul W. Eslinger : 20 Nov 1997 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 11 May 2005 : Change to NARGS, GETARG from GETCL,
!!                                     Add command line help option
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger : 29 Jun 2012 : Revise to a common callable routine
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
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      LOGICAL :: THERE ! Logical flag for file existence inquire
      INTEGER :: IERF ! Status variable for open statement
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
! *** Get a unit number
!
      IKEY = GET_UNIT_NUMBER(  )
      IF( IKEY .LT. 7 ) THEN
        IERR = 2
        WRITE(*,1030) TRIM(FNKEY)
 1030   FORMAT(' Unable to get unit number for the ECEM keyword file'/ &
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
      END SUBROUTINE OPEN_KEY

      SUBROUTINE OPEN_SUMMARY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain
!!    summary statistics data.
!!
!!  History:
!!
!!    Paul W. Eslinger : 14 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
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
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error number variable
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'OPEN_SUMMARY' ! Name of this subroutine
      INTEGER :: IERA ! Error indicator from the file system
!
!---- First executable code --------------------------------------------
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
        '"Time","Location ID","Soil Type","Analyte ID","Species ID","Result Type","Units",'//&
        '"Minimum","5th Percentile","10th Percentile","25th Percentile",'//&
        '"Median","75th Percentile","90th Percentile","95th Percentile",'//&
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
      END SUBROUTINE OPEN_SUMMARY

      SUBROUTINE OPEN_THRESHOLD( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain
!!    threshold data.
!!
!!  History:
!!
!!    Paul W. Eslinger : 22 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 10 Noc 2006 : Add output results type units
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
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error number variable
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'OPEN_THRESHOLD' ! Name of this subroutine
      INTEGER :: IERA ! Error indicator from the file system
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check on the file name
!
       IF( FNTHS .EQ. ' ' ) THEN
         IERR = 1
         MESSAG(1) = 'The output threshold data file name is missing'
         MESSAG(2) = 'Use the THRESH modifier on the FILE Keyword'
         CALL PRTERR( IERR, CALLER, 2 )
         RETURN
       END IF
!
! *** Get a unit number
!
      ITHS = GET_UNIT_NUMBER(  )
      IF( ITHS .LT. 7 ) THEN
        IERR = 2
        MESSAG(1) = 'Unable to get unit number for the threshold data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output threshold data file
!
      OPEN(ITHS,FILE=FNTHS,STATUS='UNKNOWN',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the threshold data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Write an identification line
!
      WRITE(ITHS,1000,IOSTAT=IERA) &
        '"Time","Location ID","Species ID","Analyte ID","Result Type","Result Units",'//&
        '"Limit 1","Exceed Prob 1","Limit 2","Exceed Prob 2"'
 1000 FORMAT(A)
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'System error writing the threshold file header line'
        MESSAG(2) = 'System error number is '
        WRITE(MESSAG(2)(24:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE OPEN_THRESHOLD

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
!!    Paul W. Eslinger : 10 Jul 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
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
      CHARACTER(LEN=10) :: CALLER = 'OPEN_VALUE' ! Name of this subroutine
      INTEGER :: IERF ! Status variable for open statement
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IVLU = GET_UNIT_NUMBER(  )
      IF( IVLU .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the generated stochastic data file'
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
      END SUBROUTINE OPEN_VALUE

      SUBROUTINE OR_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Organic, Radioactive
!!      Species: Aquatic, Animal
!!
!!  History:
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 28 May 2002 : Change debug writes
!!    Paul W. Eslinger :  3 Jun 2002 : Change sediment ingestion equation.
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 Modify equations
!!    Paul W. Eslinger : 15 Aug 2006 : (SCR-1140) Reformat outputs
!!                                     Simplify the equations for food ingestion
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS      ! Logical whether skip all data outputs
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OR_QA' ! Name of this subroutine
      CHARACTER(LEN=6) :: LOC_ID ! Local location ID
!
      INTEGER :: IREL ! Realization index
      INTEGER :: NREC ! Record number for writing to the foods file
      INTEGER :: JSPC ! Species index
!
      REAL :: TMPKU  ! Temporary uptake rate
      REAL :: TMPBCF ! Temporary bioconcentration factor
      REAL :: TMPWAT ! Body burden from water exposure
      REAL :: TMPING ! Body burden from food ingestion
      REAL :: TMPSED ! Body burden from sediment ingestion
      REAL :: TMPAIR ! Body burden from air inhalation
!
      REAL :: IIJ    ! Temporary variable in ingestion equations (feeding rate)
      REAL :: BIOACC ! Temporary bioaccumulation value in ingestion equations
!
      LOGICAL :: EAT_FOOD ! Local flag whether this animal eats food
!
      REAL :: DINT       ! Internal radiation dose
      REAL :: DIMM       ! Immersion radiation dose
      REAL :: DSED       ! Radiation dose from sediment exposure
      REAL :: DSED_ABOVE ! Radiation dose from sediment exposure - above
      REAL :: DSED_BELOW ! Radiation dose from sediment exposure - below
      REAL :: CFELEV     ! Conversion factor for elevation (sediment dose)
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGORQA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Radioactive   ' / &
          'Species : ',A,' : Aquatic, Animal        ' / &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
      FORALL(IREL=1:NREAL) FWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
! *** Get the Chemical transfer efficiency as a function of KOW for all realizations
      CALL GET_CHEM_EFF( ISPC, IANA, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Determine whether this animal eats any food.  This calculation doesn't depend on any specific
!     realizations and simply makes it easier to write more elegant debug statements
      EAT_FOOD = .FALSE.
      DO JSPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) EAT_FOOD = .TRUE.
      END DO
!
! *** Start the calculations depending on realization
      DO IREL = 1, NREAL
!
!       Uptake of dissolved sources of contaminant for predator species (L/g lipid/day)
        TMPKU  = ( 2.67 * ESD_SPC(ISPC)%FOC * ESD_SPC(ISPC)%RESPRATE * ESD_SPC(ISPC)%CHEM(IREL)) / &
                 ( ESD_SPC(ISPC)%AWD * ESD_LOC(ILOC)%COXYGEN * ESD_SPC(ISPC)%FLIPID )
!
!       Loss rate of contaminant including depuration and metabolism (1/day)
        ESD_SPC(ISPC)%LOSSRATE = TMPKU / ( (10.0**ESD_ANA(IANA)%KOW(IREL))/1000.0 )+ESD_SPC(ISPC)%METBLOSS(IANA,IREL)
!
!       Bioconcentration factor for an organic contaminant (L/g lipid)
        TMPBCF = TMPKU / ( ESD_SPC(ISPC)%LOSSRATE + ESD_SPC(ISPC)%GROWRATE )
!
!       Body burden due to water exposure (pCi/g lipid)
        TMPWAT  = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * TMPBCF
!
!       Body burden from air inhalation
        IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
          TMPAIR = (EC(IREL)%AIRC+EC(IREL)%VAPOR)*ESD_SPC(ISPC)%INHRATE / (ESD_SPC(ISPC)%WBMASS/ESD_SPC(ISPC)%AWD)
          TMPAIR = TMPAIR * ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / (ESD_SPC(ISPC)%DEPRATE(IANA,IREL)+ESD_SPC(ISPC)%GROWRATE)
        ELSE
          TMPAIR = 0.0
        END IF
!
!       Initialize food ingestion and sediment ingestion body burdens
        TMPING = 0.0
        TMPSED = 0.0
!
!       Skip a lot of calculations if this animal doesn't eat any food
        IF( EAT_FOOD ) THEN
!
          IF( BGORQA ) THEN
            WRITE(IRPT,1020)
 1020       FORMAT(/'   Food intake calculations for all prey ...'/&
                    '   IIJ = Feeding rate (g prey dry weight/g predator dry weight/day)'/ &
                    '   Index    ID     PREDATE      ALPHAIJ      GROWRATE     RESPRATE     OCAR        F lipid     FLipid(prey)',&
                    '  FOC (prey)     FOC-I         BIOACC       IIJ'/&
                    '   ------ ------ ------------ ------------ ------------ ------------ ------------ ------------ ------------',&
                    ' ------------ ------------ ------------ ------------')
          END IF
!
! ***     Food and sediment ingestion calculations
          BIOACC = 0.0
          DO JSPC = 1, ESD_NUM_SPC
!
!           Skip species not computed or not eaten by the predator
            IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
            IF( .NOT.(PREDATE(ISPC,JSPC).GT.0.0) ) CYCLE
!
!           Feeding rate (g prey dry weight/g predator dry weight/d)
            IIJ = (( ESD_SPC(ISPC)%GROWRATE+ESD_SPC(ISPC)%RESPRATE ) / ESD_SPC(ISPC)%OCAR ) * &
                ( ESD_SPC(JSPC)%AWD / ESD_SPC(ISPC)%AWD ) * ( ESD_SPC(JSPC)%FLIPID / ESD_SPC(ISPC)%FLIPID ) * &
                ( ESD_SPC(ISPC)%FOC / ESD_SPC(JSPC)%FOC )
!
!           Accumulation factor for prey body burden
            BIOACC = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL) * IIJ / (ESD_SPC(ISPC)%LOSSRATE+ESD_SPC(ISPC)%GROWRATE)
!
!           Increment the ingestion and sediment body burdens
            TMPING = TMPING + BIOACC * ESD_SPC(JSPC)%BODYBURDEN(IREL)
            TMPSED = TMPSED + BIOACC * (EC(IREL)%SEDI * ESD_SPC(ISPC)%SEDING)
!
!           Debug outputs
            IF( BGORQA ) &
              WRITE(IRPT,1021) JSPC, ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL), &
                ESD_SPC(ISPC)%GROWRATE, ESD_SPC(ISPC)%RESPRATE, ESD_SPC(ISPC)%OCAR, ESD_SPC(ISPC)%FLIPID, ESD_SPC(JSPC)%FLIPID, &
                ESD_SPC(JSPC)%FOC, ESD_SPC(ISPC)%FOC, BIOACC, IIJ
 1021         FORMAT(3X,I6,1P,1X,A,12(1X,E12.5))
!
          END DO
!
        END IF
!
!       Total body burden (water exposure + food ingestion + sediment ingestion + air inhalation)
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = TMPWAT + TMPING + TMPSED + TMPAIR
!
!       Convert Body Burden to Concentration
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
!
!       Save concentration for terrestrial consumption
        CING(ISPC,IREL) = CWORK(IREL)
!
!       Save food for human consumption (wet weight basis)
        FWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
!
! ***   Compute Radiation Dose
!
!       Radiation dose from internal sources
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from water immersion
        DIMM = (ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
           (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT) * ESD_ANA(IANA)%DFIMM * QIMMFAC
!
!       Radiation dose (external) from sediment
        CFELEV = 0.8*ESD_SPC(ISPC)%BPORE(IREL) + 0.2
        DSED_ABOVE = EC(IREL)%SEDI * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDSF * CFSWD
        DSED_BELOW = EC(IREL)%SEDI * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSED = DSED_ABOVE + DSED_BELOW
!
!       Total radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSED
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / QA_BMR
!
!       Debug outputs
        IF( BGORQA ) THEN
!
          WRITE(IRPT,1050) IREL, EC(IREL)%PWAT, EC(IREL)%SWAT,EC(IREL)%SEDI, ESD_SPC(ISPC)%BPORE(IREL),&
            ESD_ANA(IANA)%KOW(IREL), ESD_LOC(ILOC)%COXYGEN, ESD_SPC(ISPC)%AWD, ESD_SPC(ISPC)%CHEM(IREL), &
            ESD_SPC(ISPC)%METBLOSS(IANA,IREL), TMPKU, TMPBCF, TMPWAT, TMPING, TMPSED, TMPAIR, ESD_SPC(ISPC)%BODYBURDEN(IREL), CWORK(IREL), &
            FWORK(IREL), ESD_SPC(ISPC)%EFFENG(IANA), DINT, ESD_ANA(IANA)%DFIMM, &
            DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_ANA(IANA)%DFSED, DSED_ABOVE, ESD_ANA(IANA)%GAMMA, &
            DSED_BELOW, DSED, RDWORK(IREL)
 1050     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units               : Variable           : Description'/ &
            3X,12('-'),3X,19('-'),3X,18('-'),3X,40('-')  /&
            3X,E12.5,' : pCi/L               : EC%PWAT            : Pore water concentration'/ &
            3X,E12.5,' : pCi/L               : EC%SWAT            : Surface water concentration'/ &
            3X,E12.5,' : pCi/kg              : EC%SEDI            : Sediment concentration'/ &
            3X,E12.5,' : unitless            : ESD_SPC%BPORE      : Relative exposure to pore water'/ &
            3X,E12.5,' : unitless            : ESD_ANA%KOW        : log base 10 of the octanol-water partition coefficient'/ &
            3X,E12.5,' : g O2/L              : ESD_LOC%COXYGEN    : Oxygen concentration in surface water'/ &
            3X,E12.5,' : g wet/g dry         : ESD_SPC%AWD        : Wet to dry weight ratio'/&
            3X,E12.5,' : g/g per g O2/g lipid: ESD_SPC%CHEM       : Chemical transfer efficiency'/ &
            3X,E12.5,' : 1/day               : ESD_SPC%METBLOSS   : Metabolism loss rate'/  &
            3X,E12.5,' : L/g lipid/day       : TMPKU              : Uptake rate'/ &
            3X,E12.5,' : L/g lipid           : TMPBCF             : Bioconcentration factor'/ &
            3X,E12.5,' : pCi/g lipid         : TMPWAT             : Water exposure part of body burden'/ &
            3X,E12.5,' : pCi/g lipid         : TMPING             : Food ingestion part of body burden'/ &
            3X,E12.5,' : pCi/g lipid         : TMPSED             : Sediment ingestion part of body burden'/ &
            3X,E12.5,' : pCi/g lipid         : TMPAIR             : Inhalation part of body burden'/ &
            3X,E12.5,' : pCi/g lipid         : ESD_SPC%BODYBURDEN : Body burden'/ &
            3X,E12.5,' : pCi/kg wet          : CWORK              : Concentration consumed by terrestrial animals'/&
            3X,E12.5,' : pCi/kg wet          : FWORK              : Concentration consumed by human'/&
            3X,E12.5,' : MeV/disintegration  : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X,E12.5,' : rad/day             : DINT               : Radiation dose from internal contamination'/ &
            3X,E12.5,' : mrad/yr per uCi/m^3 : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X,E12.5,' : rad/day             : DIMM               : Radiation dose from water immersion'/ &
            3X,E12.5,' : unitless            : CFELEV             : Conversion factor based on time in pore water'/ &
            3X,E12.5,' : unitless            : FABOVE             : Fraction time spent above sediment'/ &
            3X,E12.5,' : Sv-m^3/sec-Bq       : ESD_ANA%DFSED      : Dose factor for external exposure to sediment'/ &
            3X,E12.5,' : rad/day             : DSED_ABOVE         : Radiation dose from sediment - above'/ &
            3X,E12.5,' : MeV/nt              : ESD_ANA%GAMMA      : Gamma (photon) energy'/ &
            3X,E12.5,' : rad/day             : DSED_BELOW         : Radiation dose from sediment - below'/ &
            3X,E12.5,' : rad/day             : DSED               : Total radiation dose from sediment'/ &
            3X,E12.5,' : rad/day             : RDWORK             : Total radiation dose')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1060) QA_BMR, EHQ(IREL)
 1060     FORMAT(1P, &
            3X,E12.5,' : rad/day             : QA_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X,E12.5,' : unitless            : EHQ                : Environmental hazard quotient')
!
          IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
            WRITE(IRPT,1070) EC(IREL)%AIRC,EC(IREL)%VAPOR,ESD_SPC(ISPC)%INHRATE,ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL)
 1070       FORMAT(&
            3X,ES12.5,' : pCi/m^3             : EC%AIRC            : Air concentration'/ &
            3X,ES12.5,' : pCi/m^3             : EC%VAPOR           : Vapor concentration'/ &
            3X,ES12.5,' : m^3/day             : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
            3X,ES12.5,' : unitless            : ESD_SPC%ALPHAVAP   : Vapor inhalation assimilation efficiency')
          END IF
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
!
      IF( BGORQA .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_ORG_RAD_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_ORG_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD',UNITS_ORG_RAD_DOS_AQ,NREAL,RDWORK)
        IF( DET_BMRDOS) CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
!
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the computed values
      IF( DET_BURDEN ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'BURDEN',UNITS_ORG_RAD_BUR_AQ,CVEC )
      END IF
!
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_AQ, RDWORK )
!
      IF( DET_TFOODS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
          ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_ORG_RAD_BUR_TER, CWORK )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the concentration (as consumed) to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold calculations
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_RAD_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_RAD_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose statistics (and threshold calculations)
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_ORG_RAD_DOS_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_ORG_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OR_QA

      SUBROUTINE OR_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equations for the following case:
!!      Analyte: Organic, Radioactive
!!      Species: Aquatic, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 Modify equations
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Change to EHQ & reformat
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Esd_Mod
      USE Results_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS ! Logical whether skip all data outputs
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OR_QP' ! Name of this subroutine
!
      INTEGER :: IREL ! Realization index
      INTEGER :: NREC ! Record number for writing to the foods file
!
      REAL :: TMPKU   ! Calculated uptake rate
      REAL :: TMPBCF  ! Calculated BCF for organic contaminant
      REAL :: DINT    ! Internal dose
      REAL :: DIMM    ! Immersion dose
      REAL :: CFELEV  ! Conversion factor for elevation
      REAL :: DSED       ! Radiation dose from sediment
      REAL :: DSED_ABOVE ! Radiation dose from sediment - above
      REAL :: DSED_BELOW ! Radiation dose from sediment - below
      CHARACTER(LEN=6) :: LOC_ID ! Temporary location ID
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGORQP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Radioactive   ' / &
          'Species : ',A,' : Aquatic, Plant         ' / &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
!     Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize the body burden and dose dose vectors
!
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) FWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
! *** Get the Chemical transfer efficiency as a function of KOW for all realizations
!
      CALL GET_CHEM_EFF( ISPC, IANA, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Loop over realizations and compute concentratiosn and doses
!
      DO IREL = 1, NREAL
!
!       Contaminant uptake from dissolved sources
        TMPKU  = ( 2.67 * ESD_SPC(ISPC)%FOC * ESD_SPC(ISPC)%RESPRATE * ESD_SPC(ISPC)%CHEM(IREL)) / &
                 ( ESD_SPC(ISPC)%AWD * ESD_LOC(ILOC)%COXYGEN * ESD_SPC(ISPC)%FLIPID )
!
!       Contaminant loss rate from metabolism
        ESD_SPC(ISPC)%LOSSRATE = TMPKU / ( (10.0**ESD_ANA(IANA)%KOW(IREL))/1000.0 )
!
!       Bioconcentration factor
        TMPBCF = TMPKU / ( ESD_SPC(ISPC)%LOSSRATE + ESD_SPC(ISPC)%GROWRATE )
!
!       Body burden (consumed by aquatic animals)
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
          (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * TMPBCF
!
!       Concentration for consumption by terrestrial animals (save in CPAT - wet weight basis)
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
        CPAT(ISPC,IREL) = CWORK(IREL)
!
!       Concentration in foods for human consumption (wet weight basis)
        FWORK(IREL) =  ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
!
        IF( BGORQP ) THEN
!
          WRITE(IRPT,1010) IREL, ESD_SPC(ISPC)%FOC, ESD_SPC(ISPC)%RESPRATE, ESD_SPC(ISPC)%CHEM(IREL), &
            ESD_SPC(ISPC)%AWD, ESD_LOC(ILOC)%COXYGEN, ESD_SPC(ISPC)%FLIPID, TMPKU, ESD_ANA(IANA)%KOW(IREL), &
            ESD_SPC(ISPC)%LOSSRATE, ESD_SPC(ISPC)%GROWRATE, TMPBCF, ESD_SPC(ISPC)%BPORE(IREL), EC(IREL)%PWAT, &
            EC(IREL)%SWAT, ESD_SPC(ISPC)%BODYBURDEN(IREL), CPAT(ISPC,IREL), FWORK(IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units                         : Variable           : Description'/ &
            3X,12('-'),3X,29('-'),3X,18('-'),3X,50('-')  /&
            3X, E12.5,' : g organic carbon/g dry weight : ESD_SPC%FOC        : fraction organic carbon'/ &
            3X, E12.5,' : 1/day                         : ESD_SPC%RESPRATE   : Oxygen use rate'/ &
            3X, E12.5,' : g/g per g O2/g lipid          : ESD_SPC%CHEM       : chemical transfer efficiency'/ &
            3X, E12.5,' : g wet/g dry                   : ESD_SPC%AWD        : wet to dry weight ratio'/ &
            3X, E12.5,' : g Oxygen/L                    : ESD_LOC%COXYGEN    : oxygen concentration in the surface water'/ &
            3X, E12.5,' : g lipid/g wet                 : ESD_SPC%FLIPID     : Fraction lipid'/ &
            3X, E12.5,' : L/g lipid/day                 : TMPKU              : contaminant uptake from dissolved sources'/ &
            3X, E12.5,' : unitless                      : ESD_ANA%KOW        : log base 10 of octanol-water partition coefficient'/&
            3X, E12.5,' : 1/day                         : ESD_SPC%LOSSRATE   : loss rate from metabolism'/ &
            3X, E12.5,' : 1/day                         : ESD_SPC%GROWRATE   : growth rate'/ &
            3X, E12.5,' : L/g lipid                     : TMPBCF             : bioconcentration factor for contaminant'/ &
            3X, E12.5,' : unitless                      : ESD_SPC%BPORE      : relative exposure to pore water'/ &
            3X, E12.5,' : pCi/L                         : EC%PWAT            : Pore water concentration'/ &
            3X, E12.5,' : pCi/L                         : EC%SWAT            : Surface water concentration'/ &
            3X, E12.5,' : pCi/g lipid                   : ESD_SPC%BODYBURDEN : Body burden (consumed by aquatic animals)'/ &
            3X, E12.5,' : pCi/kg wet                    : CPAT               : Concentration consumed by terrestrial animals'/ &
            3X, E12.5,' : pCi/kg wet                    : FWORK              : Concentration consumed by humans')
!
        END IF
!
!       Radiation dose from internal contamination
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from water immersion
        DIMM = (ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
           (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT) * ESD_ANA(IANA)%DFIMM * QIMMFAC
!
!       Radiation dose from sediment (above and below compartments)
        CFELEV = 0.8*ESD_SPC(ISPC)%BPORE(IREL) + 0.2
        DSED_ABOVE = EC(IREL)%SEDI * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDSF * CFSWD
        DSED_BELOW = EC(IREL)%SEDI * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSED = DSED_ABOVE + DSED_BELOW
!
!       Total Radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSED
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / QP_BMR
!
        IF( BGORQP ) THEN
          WRITE(IRPT,1030) ESD_SPC(ISPC)%EFFENG(IANA), &
            DINT, ESD_ANA(IANA)%DFIMM, DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_ANA(IANA)%DFSED, EC(IREL)%SEDI, DSED_ABOVE, &
            ESD_ANA(IANA)%GAMMA, DSED_BELOW, DSED, RDWORK(IREL)
 1030     FORMAT(1P, &
            3X,12('-'),3X,29('-'),3X,18('-'),3X,50('-')  /&
            3X, E12.5,' : MeV/disintegration            : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X, E12.5,' : rad/day                       : DINT               : Radiological dose from internal contamination'/ &
            3X, E12.5,' : mrad/yr per uCi/m^3           : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X, E12.5,' : rad/day                       : DIMM               : Radiological dose from water immersion'/ &
            3X, E12.5,' : unitless                      : CFELEV             : Conversion factor based on time in pore water'/ &
            3X, E12.5,' : unitless                      : FABOVE             : Fraction time spent above sediment'/ &
            3X, E12.5,' : Sv-m^3/sec-Bq                 : ESD_ANA%DFSED      : Dose factor for external exposure to sediment'/ &
            3X, E12.5,' : pCi/kg                        : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : rad/day                       : DSED_ABOVE         : Radiological dose from sediment - above'/ &
            3X, E12.5,' : MeV/nt                        : ESD_ANA%GAMMA      : (photon) gamma energy'/ &
            3X, E12.5,' : rad/day                       : DSED_BELOW         : Radiological dose from sediment - below'/ &
            3X, E12.5,' : rad/day                       : DSED               : Total radiological dose from sediment'/ &
            3X, E12.5,' : rad/day                       : RDWORK             : Total radiological dose')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1040) QP_BMR, EHQ(IREL)
 1040     FORMAT(1P, &
            3X, E12.5,' : rad/day                       : QP_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless                      : EHQ                : Environmental hazard quotient')
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
!     After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGORQP .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_ORG_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_ORG_RAD_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD',UNITS_ORG_RAD_DOS_AQ,NREAL,RDWORK)
        IF( DET_BMRDOS) CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
          ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_ORG_RAD_BUR_AQ, CVEC )
      END IF
      IF( DET_TFOODS ) THEN
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
          ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_ORG_RAD_BUR_TER, CWORK )
      END IF
!
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_AQ, RDWORK )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the burden (as consumed by predators) to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold limits)
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_RAD_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_RAD_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose statistics (and threshold limits)
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_ORG_RAD_DOS_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_ORG_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OR_QP

      SUBROUTINE OR_TA( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Organic, Radioactive
!!      Species: Terrestrial, Animal
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 29 May 2002 : Change debug writes
!!    Paul W. Eslinger : 28 Apr 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 26 Oct 2004 : Add optional secondary transfer factor
!!    Paul W. Eslinger : 20 May 2005 : Add particulate fraction for far-field air
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 - Restructure soil dose equation
!!    Paul W. Eslinger : 30 Jun 2005 : Add error checking on energy intake
!!    Paul W. Eslinger :  1 Sep 2005 : Change debug outputs
!!    Paul W. Eslinger :  6 Sep 2006 : (SCR-1140) Reformat
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OR_TA' ! Name of this subroutine
      INTEGER :: IREL ! Realization index
      INTEGER :: JSPC ! Species index
!
      REAL :: DINT    ! Internal dose
      REAL :: DIMM    ! Immersion dose
!
      REAL :: DSOI       ! Radiation dose from sediment
      REAL :: DSOI_ABOVE ! Radiation dose from sediment - above
      REAL :: DSOI_BELOW ! Radiation dose from sediment - below
!
      REAL :: CFELEV  ! Conversion factor for elevation
      REAL :: DDERMS  ! Applied daily dose from dermal contact with soil
      REAL :: DDERMW  ! Applied daily dose from dermal contact with water
!
      REAL :: DIV    ! Applied daily dose from vapor inhalation
      REAL :: DIP    ! Applied daily dose from particulate inhalation
      REAL :: CIV    ! Equilibrium body burden from vapor inhalation
      REAL :: CIP    ! Equilibrium body burden from particulate inhalation
!
      REAL :: DINGW  ! Applied daily dose from water ingestion
      REAL :: CINGW  ! Equilibrium body burden from water ingestion
      REAL :: DINGS  ! Applied daily dose from soil ingestion
      REAL :: CINGS  ! Equilibrium body burden from soil ingestion
      REAL :: DINGF  ! Applied daily dose from food ingestion
      REAL :: CINGF  ! Equilibrium body burden from food ingestion
!
      REAL :: SUMPROD ! Temporary sum variable
      REAL :: PROD    ! Temporary - metabilizable energy from one prey species
!
      REAL :: CONJ   ! Concentration in prey species
      REAL :: NIR    ! Normalized intake rate
!
      INTEGER :: NREC ! Record number for writing to the foods file
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGORTA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//,A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Radioactive   ' / &
          'Species : ',A,' : Terrestrial, Animal    ' / &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
!
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
! ***   Calculate Dermal Contact with Soil  (absorbed daily dose)
        DDERMS = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCS * &
          ESD_SPC(ISPC)%SADHER * EC(IREL)%SOIL * CFDERMS * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * &
          ESD_SPC(ISPC)%ALPHADS(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT
!
! ***   Calculate Dermal Contact with Water  (absorbed daily dose)
        DDERMW = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCW * &
          ESD_SPC(ISPC)%ETWATER * EC(IREL)%WATER * CFDERMW * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * &
          ( ESD_SPC(ISPC)%ALPHADW(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT )
!
!       Absorbed daily dose from dermal contact with contaminant in soil
        DDER(IREL) = DDERMS + DDERMW
!
!       Equilibrium body burden from dermal contact with contaminant in soil
        CDER(ISPC,IREL) = DDER(IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL)
!
        IF( BGORTA ) THEN
          WRITE(IRPT,1010) IREL, EC(IREL)%SOIL, ESD_SPC(ISPC)%SA, ESD_SPC(ISPC)%PCS, ESD_SPC(ISPC)%SADHER, &
            ESD_SPC(ISPC)%THETA, ESD_SPC(ISPC)%PSI, ESD_SPC(ISPC)%ALPHADS(IANA,IREL), ESD_SPC(ISPC)%WEIGHT, DDERMS, &
            ESD_SPC(ISPC)%PCW, ESD_SPC(ISPC)%ETWATER, EC(IREL)%WATER, ESD_SPC(ISPC)%ALPHADW(IANA,IREL), DDERMW, &
            DDER(IREL), ESD_SPC(ISPC)%DEPRATE(IANA,IREL), CDER(ISPC,IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : pCi/kg                       : EC%SOIL            : Concentration of contaminant in soil'/ &
            3X,E12.5,' : cm^2                         : ESD_SPC%SA         : Surface area of animal species'/ &
            3X,E12.5,' : 1/day                        : ESD_SPC%PCS        : Fraction of species surface area contacting soil/day'/&
            3X,E12.5,' : mg/cm^2                      : ESD_SPC%SADHER     : Skin adherence factor for animal species'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%THETA      : Area use factor (contaminated/home-range area)'/&
            3X,E12.5,' : unitless                     : ESD_SPC%PSI        : Seasonality factor (fraction of year in area)'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%ALPHADS    : Soil dermal absorption factor (contaminant, species)'/&
            3X,E12.5,' : kg wet                       : ESD_SPC%WEIGHT     : Body weight of animal species'/ &
            3X,E12.5,' : pCi/kg/day wet               : DDERMS             : Absorbed daily dose from dermal contact with soil'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%PCW        : Fraction of species surface area contacting water/dy'/&
            3X,E12.5,' : hr/day                       : ESD_SPC%ETWATER    : Average exposure time to water per day'/ &
            3X,E12.5,' : pCi/L                        : EC%WATER           : Concentration of contaminant in water'/ &
            3X,E12.5,' : cm/hr                        : ESD_SPC%ALPHADW    : Water dermal absorption factor(contaminant, species)'/&
            3X,E12.5,' : pCi/kg/day wet               : DDERMW             : Absorbed daily dose from dermal contact with water'/&
            3X,E12.5,' : pCi/kg/day wet               : DDER               : Absorbed daily dose from dermal contact'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%DEPRATE    : Depuration rate of contaminant by animal species'/&
            3X,E12.5,' : pCi/kg wet                   : CDER               : Equilibrium body burden from dermal exposure')
        END IF
!
! ***   Calculate Inhalation Dose from Vapor
!
        IF( ESD_ANA(IANA)%HENRY>0.0) THEN
          DIV = ((ESD_SPC(ISPC)%INHRATE * (EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)+EC(IREL)%VAPOR) ) / ESD_SPC(ISPC)%WEIGHT) * &
            ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
          IF( BGORTA ) THEN
            WRITE(IRPT,1030) EC(IREL)%AIRC, ESD_ANA(IANA)%PARTICLE, ESD_ANA(IANA)%HENRY, EC(IREL)%VAPOR, ESD_SPC(ISPC)%INHRATE, &
              ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL), DIV, CIV
 1030       FORMAT( 1P, &
              3X,E12.5,' : pCi/m^3                      : EC%AIRC            : Concentration of contaminant in air'/ &
              3X,E12.5,' : pCi/m^3                      : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : Pa-m^3/mol                   : ESD_ANA%HENRY      : Henrys law constant'/&
              3X,E12.5,' : pCi/m^3                      : EC%VAPOR           : Concentration of contaminant in vapor'/ &
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : unitless                     : ESD_SPC%ALPHAVAP   : Inhalation absorption factor of contaminant'/ &
              3X,E12.5,' : pCi/kg/day wet               : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : pCi/kg wet                   : CIV                : Equilibrium body burden from vapor inhalation')
           END IF
        ELSE
          DIV = ((ESD_SPC(ISPC)%INHRATE * EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)) / ESD_SPC(ISPC)%WEIGHT) * &
            ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
          IF( BGORTA ) THEN
            WRITE(IRPT,1040) ESD_SPC(ISPC)%INHRATE, EC(IREL)%AIRC, ESD_ANA(IANA)%PARTICLE,  DIV, CIV
 1040       FORMAT( 1P, &
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : pCi/m^3                      : EC%AIRC            : Concentration of contaminant in air'/ &
              3X,E12.5,' : pCi/m^3                      : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : pCi/kg/day wet               : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : pCi/kg wet                   : CIV                : Equilibrium body burden from vapor inhalation')
          END IF
        END IF
!
!       Absorbed daily dose from inhalation of particualtes
        DIP = ESD_SPC(ISPC)%INHRATE * (EC(IREL)%PARTIC+EC(IREL)%AIRC*ESD_ANA(IANA)%PARTICLE) * ESD_SPC(ISPC)%THETA * &
          ESD_SPC(ISPC)%PSI / ESD_SPC(ISPC)%WEIGHT
!
!       Equilibrium body burden from inhalation of particulates
        CIP = DIP * ( ESD_SPC(ISPC)%ALPHAPAR(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
! ***   Total absorbed daily dose from inhalation
        DINH(IREL) = DIV + DIP
!
!       Total equilibrium body burden from inhalation
        CINH(ISPC,IREL) = CIV + CIP
!
        IF( BGORTA ) THEN
          WRITE(IRPT,1050) DIP, CIP, DINH(IREL), CINH(ISPC,IREL)
 1050     FORMAT( 1P, &
            3X,E12.5,' : pCi/kg/day wet               : DIP                : Applied daily dose from particulate inhalation'/ &
            3X,E12.5,' : pCi/kg wet                   : CIP                : Equilibrium body burden from particulate inhalation'/ &
            3X,E12.5,' : pCi/kg/day wet               : DINH               : Applied daily dose from all inhalation'/ &
            3X,E12.5,' : pCi/kg wet                   : CINH               : Equilibrium body burden from all inhalation')
        END IF
!
! ***   Total absorbed daily dose from water
        DINGW = ( ( ESD_SPC(ISPC)%WATERING * EC(IREL)%WATER ) / ESD_SPC(ISPC)%WEIGHT ) * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!       Total equilibrium body burden from inhalation
        CINGW = DINGW * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
        IF( BGORTA ) THEN
          WRITE(IRPT,1060) ESD_SPC(ISPC)%WATERING, EC(IREL)%WATER, DINGW, CINGW
 1060     FORMAT( 1P, &
            3X,E12.5,' : L/day                        : ESD_SPC%WATERING   : Water ingestion rate for animal species'/ &
            3X,E12.5,' : pCi/kg                       : EC%WATER           : Concentration of contaminant in water'/ &
            3X,E12.5,' : pCi/kg/day wet               : DINGW              : Applied daily dose from water ingestion'/ &
            3X,E12.5,' : pCi/kg wet                   : CINGW              : Equilibrium body burden from water ingestion')
        END IF
!
! ***   Calculate the total metabilizable energy from food (kcal/kg prey wet wt) [Use variable SUMPROD]
!       This is used in both food ingestion and soil ingestion calculations
        IF( BGORTA ) THEN
          WRITE(IRPT,1070) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1070     FORMAT(/'   Species ',A,' (',A,') Metabilizable Energy From Prey'/&
            3X,'Prey   Predation',13X,'Gross Energy',20X,'Assimilation Efficiency',1X,'Metabilizable Energy'/&
            3X,'------',1X,21('-'),1X,31('-'),1X,23('-'),1X,32('-'))
        END IF
!
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate the metabilizable energy this prey (kcal/kg prey wet wt)
          PROD = PREDATE(ISPC,JSPC) * ESD_SPC(JSPC)%GE * ESD_SPC(JSPC)%AE
          IF( PROD .LE. 0.0 ) THEN
            IERR = 2
            MESSAG(1) = 'Food is ingested but no metabilizable energy is defined'
            MESSAG(2) = 'Location '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Species  '//ESD_SPC(ISPC)%ID //' Eating '//ESD_SPC(JSPC)%ID
            MESSAG(5) = 'Check the AE and GE values on the SPECIES keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Sum the metabilizable energy over all prey
          SUMPROD = SUMPROD + PROD
          IF( BGORTA ) THEN
            WRITE(IRPT,1080) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(JSPC)%GE, ESD_SPC(JSPC)%AE, PROD
 1080       FORMAT(1P,3X,A, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kcal/kg wet weight', &
              1X,E12.5,' unitless', &
              3X,E12.5,' kcal/kg prey wet wt')
          END IF
!
        END DO
!
!       Total normalized intake rate (kg prey wet weight/kg predator body weight/day)
        IF( SUMPROD .GT. 0.0 ) THEN
          ESD_SPC(ISPC)%NIRTOT(IREL) = ESD_SPC(ISPC)%FMR / ( SUMPROD * ESD_SPC(ISPC)%WEIGHT )
        ELSE
          ESD_SPC(ISPC)%NIRTOT(IREL) = 0.0
        END IF
!
! ***   Calculate Food Ingestion Dose
!
        IF( BGORTA ) THEN
          WRITE(IRPT,1090) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1090     FORMAT(/'   Species ',A,' (',A,') Food ingestion calculations'/&
            3X,'Prey   Predation',13X,'Normalized Intake',32X,'Prey Concentration'/&
            3X,'------',1X,21('-'),1X,48('-'),1X,31('-'))
        END IF
!
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate normalized intake rate for this prey species
          NIR = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%NIRTOT(IREL)
!
!         Obtain the concentration of contaminant in the prey species
          IF( ESD_SPC(JSPC)%TYPE.EQ.'TP' .OR. ESD_SPC(JSPC)%TYPE.EQ.'QP' ) THEN
            CONJ = CPAT(JSPC,IREL)
          ELSE
            CONJ = CING(JSPC,IREL) + CINH(JSPC,IREL) + CDER(JSPC,IREL)
          END IF
!
!         Add the contaminant intake for this prey species to the total intake
          SUMPROD = SUMPROD + (CONJ * NIR)
          IF( BGORTA ) THEN
            WRITE(IRPT,1110) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), NIR, CONJ
 1110       FORMAT(3X,A,1P, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kg prey wet wt/kg predator wet wt/d', &
              1X,E12.5,' pCi/kg prey wet wt')
          END IF
!
        END DO
!
!       Applied daily dose from all prey consumption (pCi/kg/day)
        DINGF = SUMPROD * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!       Equilibrium body burden from all prey consumption (pCi/kg body wet weight)
        CINGF = DINGF * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
        IF( BGORTA ) THEN
          WRITE(IRPT,1120) DINGF, CINGF
 1120     FORMAT(/ 1P, &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : pCi/kg/day wet               : DINGF              : Applied daily dose from food ingestion'/ &
            3X,E12.5,' : pCi/kg wet                   : CINGF              : Equilibrium body burden from food ingestion')
        END IF
!
! ***   Calculate the soil ingestion dose
        IF( ESD_SPC(ISPC)%NIRTOT(IREL) .GT. 0.0 ) THEN
!
!         Applied daily dose from soil consumption (pCi/kg/day)
          DINGS = ( ESD_SPC(ISPC)%SOILING * ESD_SPC(ISPC)%NIRTOT(IREL) * EC(IREL)%SOIL * &
            ESD_SPC(ISPC)%FDW ) * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equilibrium body burden from soil consumption (pCi/kg body wet weight)
          CINGS = DINGS * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGORTA ) THEN
            WRITE(IRPT,1130) ESD_SPC(ISPC)%FMR, ESD_SPC(ISPC)%NIRTOT(IREL), ESD_SPC(ISPC)%SOILING, EC(IREL)%SOIL, &
              ESD_SPC(ISPC)%FDW, DINGS, CINGS
 1130       FORMAT(1P, &
              3X,E12.5,' : kcal/day                     : ESD_SPC%FMR        : free-living metabolic rate of predator species'/ &
              3X,E12.5,' : kg prey wt/kg predator wt/d  : ESD_SPC%NIRTOT     : Total normalized intake rate'/ &
              3X,E12.5,' : kg soil ingested/kg dry diet : ESD_SPC%SOILING    : Soil ingestion rate'/ &
              3X,E12.5,' : pCi/kg                       : EC%SOIL            : Concentration of contaminant in soil'/ &
              3X,E12.5,' : kg dry/kg wet                : ESD_SPC%FDW        : Conversion factor, dry diet to wet diet'/ &
              3X,E12.5,' : pCi/kg/day wet               : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : pCi/kg wet                   : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
        ELSE
!
!         Applied daily dose from soil consumption (pCi/kg/day)
          DINGS = 0.0
!
!         Equilibrium body burden from soil consumption (pCi/kg body wet weight)
          CINGS = 0.0
!
          IF( BGORTA ) THEN
            WRITE(IRPT,1140) DINGS, CINGS
 1140       FORMAT( 1P, &
              3X,E12.5,' : pCi/kg/day wet               : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : pCi/kg wet                   : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
        END IF
!
! ***   Applied daily dose from all sources (pCi/kg/day)
        DING(IREL) = DINGF + DINGS + DINGW
!
!       Equilibrium body burden from all sources (pCi/kg body wet weight)
        CING(ISPC,IREL) = CINGF + CINGS + CINGW
!
! ***   Calculate Total Absorbed Dose
        CWORK(IREL) = CING(ISPC,IREL) + CINH(ISPC,IREL) + CDER(ISPC,IREL)
        IF( ESD_SPC(ISPC)%TRANS ) CWORK(IREL) = CWORK(IREL)*ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
!
! ***   Save the concentration for human consumption (wet weight basis)
!
        FWORK(IREL) = CWORK(IREL)
!
        IF( BGORTA ) THEN
          WRITE(IRPT,1150) DING(IREL), CING(ISPC,IREL), CWORK(IREL), FWORK(IREL)
 1150     FORMAT( 1P, &
            3X,E12.5,' : pCi/kg/day wet               : DING               : Applied daily dose from all ingestion'/ &
            3X,E12.5,' : pCi/kg wet                   : CING               : Equilibrium body burden from all ingestion'/ &
            3X,E12.5,' : pCi/kg wet                   : CWORK              : Total equilibrium body burden'/ &
            3X,E12.5,' : pCi/kg wet                   : FWORK              : Total equilibrium body burden for human consumption')
          IF( ESD_SPC(ISPC)%TRANS ) WRITE(IRPT,1160) ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
 1160     FORMAT( 1P, &
            3X,E12.5,' : pCi second/pCi primary       : TRANSVAL           : Transfer factor')
        END IF
!
! ***   Radiation dose from internal sources
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from water immersion
        DIMM = EC(IREL)%WATER * ESD_ANA(IANA)%DFIMM * &
            ESD_SPC(ISPC)%FWATER * ESD_SPC(ISPC)%ETWATER * TIMMFAC
!
!       External radiation dose from soil
        IF( ESD_SPC(ISPC)%DIFFHT .LT. 1.0 ) THEN
          CFELEV = 2.0
        ELSE
          CFELEV = 1.0
        END IF
        DSOI_ABOVE = EC(IREL)%SOIL * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDRF
        DSOI_BELOW = EC(IREL)%SOIL * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSOI = DSOI_ABOVE + DSOI_BELOW
!
!       Total radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSOI
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / TA_BMR
!
        IF( BGORTA ) THEN
          WRITE(IRPT,1170) ESD_SPC(ISPC)%EFFENG(IANA), DINT, ESD_ANA(IANA)%DFIMM, ESD_SPC(ISPC)%FWATER, ESD_SPC(ISPC)%ETWATER, &
            DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_ANA(IANA)%DFSED, EC(IREL)%SEDI, DSOI_ABOVE, ESD_ANA(IANA)%GAMMA, &
            DSOI_BELOW, DSOI, RDWORK(IREL)
 1170     FORMAT(1P, &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X, E12.5,' : MeV/disintegration           : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X, E12.5,' : rad/day                      : DINT               : Radiological dose from internal contamination'/ &
            3X, E12.5,' : mrad/yr per uCi/m^3          : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X, E12.5,' : unitless                     : ESD_SPC%FWATER     : Fraction of surface area in water contact'/ &
            3X, E12.5,' : h/day                        : ESD_SPC%ETWATER    : Exposure time to water'/ &
            3X, E12.5,' : rad/day                      : DIMM               : Radiological dose from water immersion'/ &
            3X, E12.5,' : unitless                     : CFELEV             : Conversion factor based on time in water'/ &
            3X, E12.5,' : unitless                     : ESD_SPC%FABOVE     : Fraction time spent above soil'/ &
            3X, E12.5,' : Sv-m^3/sec-Bq                : ESD_ANA%DFSED      : Dose factor for external exposure to soil'/ &
            3X, E12.5,' : pCi/kg                       : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : rad/day                      : DSED_ABOVE         : Radiological dose from soil - above'/ &
            3X, E12.5,' : MeV/nt                       : ESD_ANA%GAMMA      : (photon) gamma energy'/ &
            3X, E12.5,' : rad/day                      : DSED_BELOW         : Radiological dose from soil - below'/ &
            3X, E12.5,' : rad/day                      : DSED               : Total radiological dose from soil'/ &
            3X, E12.5,' : rad/day                      : RDWORK             : Total radiological dose')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1180) TA_BMR, EHQ(IREL)
 1180     FORMAT(1P, &
            3X, E12.5,' : rad/day                      : TA_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless                     : EHQ                : Environmental hazard quotient')
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGORTA .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN', &
          UNITS_ORG_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD', &
          UNITS_ORG_RAD_DOS_TER,NREAL,RDWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSDER', &
          UNITS_ORG_RAD_DOS_TER,NREAL,DDER)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSINH', &
          UNITS_ORG_RAD_DOS_TER,NREAL,DINH)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSING', &
          UNITS_ORG_RAD_DOS_TER,NREAL,DING)
        IF( DET_BMRDOS) CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS', &
          UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_ORG_RAD_BUR_TER, CWORK )
!
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_TER, RDWORK )
!
      IF( DET_DOSDER ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSDER', UNITS_ORG_RAD_DOS_TER, DDER )
!
      IF( DET_DOSINH ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSINH', UNITS_ORG_RAD_DOS_TER, DINH )
!
      IF( DET_DOSING ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSING', UNITS_ORG_RAD_DOS_TER, DING )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the burden (as consumed) to the food file
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Optionally write out body burden statistics and threshold values
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_RAD_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out radiation dose statistics and threshold values
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_ORG_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out dermal dose statistics and threshold values
      IF( STA_DOSDER .OR. THRESHOLD%EXIST1(ISPC,IANA,2) .OR. THRESHOLD%EXIST2(ISPC,IANA,2) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,2)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,2)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,2)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,2)
        CALL USTAT_ECEM( DDER, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSDER'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSDER ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSDER', UNITS_ORG_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSDER', UNITS_ORG_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out ingestion dose statistics and threshold values
      IF( STA_DOSING .OR. THRESHOLD%EXIST1(ISPC,IANA,3) .OR. THRESHOLD%EXIST2(ISPC,IANA,3) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,3)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,3)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,3)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,3)
        CALL USTAT_ECEM( DING, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSING'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSING ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSING', UNITS_ORG_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSING', UNITS_ORG_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally write out inhalation dose statistics and threshold values
      IF( STA_DOSINH .OR. THRESHOLD%EXIST1(ISPC,IANA,4) .OR. THRESHOLD%EXIST2(ISPC,IANA,4) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,4)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,4)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,4)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,4)
        CALL USTAT_ECEM( DINH, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSINH'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSINH ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSINH', UNITS_ORG_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSINH', UNITS_ORG_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OR_TA

      SUBROUTINE OR_TP( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Organic, Radioactive
!!      Species: Terrestrial, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 18 Nov 2004 : Change leaf deposition equations
!!    Paul W. Eslinger :  6 Dec 2004 : Change dependencies for IFRACDRY,
!!                                     IFRACWET, LWEATHDRY, and LWEATHWET.
!!    Paul W. Eslinger : 17 Jan 2005 : Wet leaf deposition is applied
!!                                     only for UPLAND locations
!!    Paul W. Eslinger : 20 Jan 2005 : Adjust leaf deposition totals
!!    Paul W. Eslinger :  9 Feb 2005 : Adjust leaf deposition totals again
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Adjust leaf deposition totals again
!!    Paul W. Eslinger :  4 Jun 2005 : SCR-1079 - Restructure soil dose equation
!!    Paul W. Eslinger : 15 Jun 2005 : SCR-1080 - Change HUMAN food options
!!    Paul W. Eslinger : 27 Jan 2006 : Correct KPA1 and KPS2 equations
!!                                     for KOW being in log units
!!    Paul W. Eslinger : 31 Aug 2006 : (SCR-1140) Reformat
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OR_TP' ! Name of this subroutine
!
      INTEGER :: IREL ! Realization index
!
      REAL :: DINT    ! Internal dose
      REAL :: DIMM    ! Immersion dose
      REAL :: DSOI       ! Radiation dose from sediment
      REAL :: DSOI_ABOVE ! Radiation dose from sediment - above
      REAL :: DSOI_BELOW ! Radiation dose from sediment - below
!
      REAL :: TMPCPAR ! Concentration on leaf surface from rain splash
      REAL :: TMPCPAD ! Concentration on leaf surface from dry deposition
      REAL :: TMPCPAU ! Concentration in plant from root uptake
      REAL :: TMPCPAV ! Concentration in plant from vapor uptake
      REAL :: TMPCPAP ! Concentration on leaf surface from particulate deposition
      REAL :: TMPCPAW ! Concentration on leaf surface from wet deposition
!
      REAL :: CFELEV  ! Conversion factor for elevation
      REAL :: KPA1 ! Plant-air partition coefficient to above-ground parts
      REAL :: KPS2 ! Calculated bioconcentration factor
!
      INTEGER :: NREC ! Record number for writing to the foods file
      REAL :: PCT     ! Temporary percent variable
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGORTP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID
 1000   FORMAT(//,A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Radioactive   ' / &
          'Species : ',A,' : Terrestrial, Plant     ' / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
!
      FORALL(IREL=1:NREAL) CWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) FWORK(IREL)  = 0.0
      FORALL(IREL=1:NREAL) RDWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)    = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine COMPUTE_VAPOR_SOIL'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine COMPUTE_PARTIC'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Loop over all realizations
!
      DO IREL = 1, NREAL
!
!       Calculate rain splash effect on leaf external surface
        IF( ESD_SPC(ISPC)%EMERGENT ) THEN
          TMPCPAR = 0.0
          IF( BGNSTP ) THEN
            WRITE(IRPT,1010) IREL, TMPCPAR
 1010       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value     : Units               : Variable           : Description'/ &
              3X,13('-'),3X,19('-'),3X,18('-'),3X,50('-')  /&
              3X, E12.5,' : pCi/kg wet           : TMPCPAR            : Rain splash concentration on leaf surface')
          END IF
        ELSE
          TMPCPAR = EC(IREL)%SOIL * ESD_SPC(ISPC)%KPS1(IREL)
          IF( BGORTP ) THEN
            WRITE(IRPT,1020) IREL, EC(IREL)%SOIL, ESD_SPC(ISPC)%KPS1(IREL), TMPCPAR
 1020       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value     : Units               : Variable           : Description'/ &
              3X,12('-'),3X,19('-'),3X,18('-'),3X,50('-')  /&
              3X, E12.5,' : pCi/kg               : EC%SOIL            : Soil concentration'/ &
              3X, E12.5,' : kg soil/kg plant wet : ESD_SPC%KPS1       : Plant-soil partition from rain splash'/ &
              3X, E12.5,' : pCi/kg wet           : TMPCPAR            : Rain splash concentration on leaf surface')
          END IF
        END IF
!
! ***   Calculate dry deposition effect on leaf external surface
        TMPCPAD = EC(IREL)%AIRD * ESD_SPC(ISPC)%IFRACDRY(IANA,IREL) / &
                  (ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL) )
        IF( BGORTP ) THEN
          WRITE(IRPT,1030) EC(IREL)%AIRD, ESD_SPC(ISPC)%IFRACDRY(IANA,IREL), ESD_SPC(ISPC)%BIOMASS(IREL), &
            ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL), TMPCPAD
 1030     FORMAT(1P, &
            3X, E12.5,' : kg/m^2/yr            : EC%AIRD            : Air deposition'/ &
            3X, E12.5,' : unitless             : ESD_SPC%IFRACDRY   : Leaf intersection fraction - dry deposition'/ &
            3X, E12.5,' : kg/m^2               : ESD_SPC%BIOMASS    : Standing plant biomass per unit area'/ &
            3X, E12.5,' : 1/yr                 : ESD_SPC%LWEATHDRY  : Leaf weathering rate - dry deposition'/ &
            3X, E12.5,' : pCi/kg wet           : TMPCPAD            : Concentration in above-ground plant from air deposition')
        END IF
!
! ***   Calculate wet deposition effect on leaf external surface (only for upland irrigated locations)
        IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
          TMPCPAW = EC(IREL)%WATER * (IRIG_AMT/SEASON_GROW) * ESD_SPC(ISPC)%IFRACWET(IANA,IREL) * 10.0 / &
                    ( ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHWET(IANA,IREL) )
          IF( BGORTP ) THEN
            WRITE(IRPT,1040) EC(IREL)%WATER, IRIG_AMT, SEASON_GROW, ESD_SPC(ISPC)%IFRACWET(IANA,IREL), &
              ESD_SPC(ISPC)%LWEATHWET(IANA,IREL), ESD_SPC(ISPC)%BIOMASS(IREL), TMPCPAW
 1040       FORMAT(1P, &
              3X, E12.5,' : pCi/L                : EC%WATER           : Irrigation water concentration'/ &
              3X, E12.5,' : cm                   : IRIG_AMT           : Amount of irrigation water applied in a year'/&
              3X, E12.5,' : yr                   : SEASON_GROW        : Length of the growing season'/&
              3X, E12.5,' : unitless             : ESD_SPC%IFRACWET   : Leaf intersection fraction - wet deposition'/ &
              3X, E12.5,' : 1/yr                 : ESD_SPC%LWEATHWET  : Leaf weathering rate - wet deposition'/ &
              3X, E12.5,' : kg/m^2               : ESD_SPC%BIOMASS    : Standing plant biomass per unit area'/ &
              3X, E12.5,' : pCi/kg wet           : TMPCPAW            : Concentration in above-ground plant from irrigation water')
          END IF
        ELSE
          TMPCPAW = 0.0
          IF( BGORTP ) THEN
            WRITE(IRPT,1050) TMPCPAW
 1050       FORMAT(1P, &
              3X, E12.5,' : pCi/kg wet           : TMPCPAW            : Concentration in above-ground plant from irrigation water')
          END IF
        END IF
!
! ***   Calculate local particulate deposition effect on leaf external surface
        TMPCPAP = EC(IREL)%PARTIC * ESD_SPC(ISPC)%KPA2(IREL)
        IF( BGORTP ) THEN
          WRITE(IRPT,1060) EC(IREL)%PARTIC, ESD_SPC(ISPC)%KPA2(IREL), TMPCPAP
 1060     FORMAT(1P, &
            3X, E12.5,' : pCi/m^3              : EC%PARTIC          : Particulate concentration in air'/ &
            3X, E12.5,' : m^3/kg wet           : ESD_SPC%KPA2       : Plant-air partition coefficient for above-ground plant'/&
            3X, E12.5,' : pCi/kg wet           : TMPCPAP            : Concentration of contaminant in above-ground plant')
        END IF
!
! ***   Calculate Root Uptake
        KPS2 = 7.7 * (10.0**ESD_ANA(IANA)%KOW(IREL))**(-0.58)
        IF( ESD_SPC(ISPC)%EMERGENT ) THEN
          TMPCPAU = EC(IREL)%SEDI * KPS2
        ELSE
          TMPCPAU = EC(IREL)%SOIL * KPS2
        END IF
        IF( BGOSTP ) THEN
          WRITE(IRPT,1070) ESD_ANA(IANA)%KOW(IREL), EC(IREL)%SOIL, EC(IREL)%SEDI, KPS2, TMPCPAU
 1070     FORMAT(1P, &
            3X, E12.5,' : unitless             : ESD_ANA%KOW        : log base 10 of octanol-water partition coefficient'/&
            3X, E12.5,' : pCi/kg               : EC%SOIL            : Soil concentration'/ &
            3X, E12.5,' : pCi/kg               : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : kg-soil/kg plant dry : KPS2               : Calculated bioconcentration factor'/ &
            3X, E12.5,' : pCi/kg wet           : TMPCPAU            : Concentration in above-ground plant from root uptake')
        END IF
!
! ***   Calculate foliar uptake from vapor
        IF( ESD_ANA(IANA)%HENRY > 0.0 ) THEN
          KPA1 = ( ESD_SPC(ISPC)%FPA + ( ESD_SPC(ISPC)%FPW + ESD_SPC(ISPC)%FPL * &
            (10.0**ESD_ANA(IANA)%KOW(IREL)) ) * &
            (( GASCON * ( ESD_LOC(ILOC)%TEMP + 273.15 )) / ESD_ANA(IANA)%HENRY )) * &
            ( 1.0 / ESD_SPC(ISPC)%RHOP )
          TMPCPAV = (EC(IREL)%VAPOR+EC(IREL)%AIRC) * KPA1
          IF( BGORTP ) THEN
            WRITE(IRPT,1080) ESD_SPC(ISPC)%FPA, ESD_SPC(ISPC)%FPW, ESD_SPC(ISPC)%FPL, ESD_ANA(IANA)%KOW(IREL), &
              GASCON, ESD_LOC(ILOC)%TEMP, ESD_ANA(IANA)%HENRY, ESD_SPC(ISPC)%RHOP, EC(IREL)%VAPOR, &
              EC(IREL)%AIRC, KPA1, TMPCPAV
 1080       FORMAT(1P, &
              3X, E12.5,' : unitless             : ESD_SPC%FPA        : Volume fraction of plant species tissue is air'/&
              3X, E12.5,' : unitless             : ESD_SPC%FPW        : Volume fraction of plant tissue that is water'/&
              3X, E12.5,' : unitless             : ESD_SPC%FPL        : Volume fraction of plant tissue that is lipid'/&
              3X, E12.5,' : unitless             : ESD_ANA%KOW        : log base 10 of octanol-water partition coefficient'/&
              3X, E12.5,' : Pa-m^3/mol-K         : GASCON             : Universal gas constant'/&
              3X, E12.5,' : Centigrade           : ESD_LOC%TEMP       : Temperature'/&
              3X, E12.5,' : Pa-m^3/mol           : ESD_ANA%HENRY      : Henrys law constant'/&
              3X, E12.5,' : kg/m^3               : ESD_SPC%RHOP       : Plant tissue density'/&
              3X, E12.5,' : pCi/m^3              : EC%VAPOR           : Vapor concentration in gas phase'/&
              3X, E12.5,' : pCi/m^3              : EC%AIRC            : Air concentration'/&
              3X, E12.5,' : m^3/kg wet           : KPA1               : Plant-air partition coefficient to above-ground parts'/&
              3X, E12.5,' : pCi/kg wet           : TMPCPAV            : Concentration in above-ground plant from vapor uptake')
          END IF
        ELSE
          TMPCPAV = 0.0
          IF( BGORTP ) THEN
            WRITE(IRPT,1090) ESD_ANA(IANA)%HENRY, TMPCPAV
 1090       FORMAT(1P, &
              3X, E12.5,' : Pa-m^3/mol           : ESD_ANA%HENRY      : Henrys law constant'/&
              3X, E12.5,' : pCi/kg wet           : TMPCPAV            : Concentration in above-ground plant from vapor uptake')
          END IF
        END IF
!
! ***   Calculate internal plant burden
!       Vapor uptake + Root uptake + Leaf absorption (dry) + Leaf absorption (wet)
        CPAI(ISPC,IREL) = TMPCPAV + TMPCPAU + (TMPCPAR+TMPCPAP+TMPCPAD)*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) + &
          TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL)
!
! ***   Calculate total plant burden (as consumed by predators)
!       Leaf surface (rain + particulates) + internal burden
        CPAT(ISPC,IREL) = TMPCPAV + TMPCPAU + TMPCPAR + TMPCPAP + TMPCPAD + TMPCPAW
!
! ***   Save the concentration for human consumption (wet weight basis)
        IF( ESD_SPC(ISPC)%INTERNAL ) THEN
          FWORK(IREL) = CPAI(ISPC,IREL)
        ELSE
          FWORK(IREL) = CPAT(ISPC,IREL)
        END IF
!
! ***   Body burden and concentration
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = CPAI(ISPC,IREL)
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
!
        IF( BGORTP ) THEN
          WRITE(IRPT,1100) ESD_SPC(ISPC)%KLIDRY(IANA,IREL), ESD_SPC(ISPC)%KLIWET(IANA,IREL), &
            CPAI(ISPC,IREL), CPAT(ISPC,IREL), ESD_SPC(ISPC)%BODYBURDEN(IREL), FWORK(IREL), &
            ESD_SPC(ISPC)%INTERNAL
 1100     FORMAT(1P, &
            3X, E12.5,' : unitless             : ESD_SPC%KLIDRY     : Leaf to internal plant transfer factor for dry deposition'/&
            3X, E12.5,' : unitless             : ESD_SPC%KLIWET     : Leaf to internal plant transfer factor for wet deposition'/&
            3X, E12.5,' : pCi/kg wet           : CPAI               : Internal plant burden'/&
            3X, E12.5,' : pCi/kg wet           : CPAT               : Total plant burden (as consumed by predators)'/&
            3X, E12.5,' : pCi/kg wet           : ESD_SPC%BODYBURDEN : Plant body burden'/&
            3X, E12.5,' : pCi/kg wet           : FWORK              : Concentration for human consumption'/&
            3X, L12  ,' : none                 : ESD_SPC%INTERNAL   : Flag for internal (true) or total concentration - human food')
        END IF
!
!       Radiation dose from internal exposure
        DINT = CWORK(IREL) * ESD_SPC(ISPC)%EFFENG(IANA) * ENGCONV
!
!       Radiation dose from immersion
        DIMM = EC(IREL)%WATER * ESD_ANA(IANA)%DFIMM * &
            ESD_SPC(ISPC)%FWATER * ESD_SPC(ISPC)%ETWATER * TIMMFAC
!
!       Radiation dose from soil exposure
        IF( ESD_SPC(ISPC)%DIFFHT .LT. 1.0 ) THEN
          CFELEV = 2.0
        ELSE
          CFELEV = 1.0
        END IF
        DSOI_ABOVE = EC(IREL)%SOIL * ESD_SPC(ISPC)%FABOVE(IREL) * ESD_ANA(IANA)%DFSED * CFELEV * CFABOVE * CFDRF
        DSOI_BELOW = EC(IREL)%SOIL * (1.0-ESD_SPC(ISPC)%FABOVE(IREL)) * ESD_ANA(IANA)%GAMMA * CFBELOW * CFMCF
        DSOI = DSOI_ABOVE + DSOI_BELOW
!
!       Total radiation dose (units of rad/day)
        RDWORK(IREL) = DINT + DIMM + DSOI
!
!       Optional radiation dose benchmark calculation
        IF( DET_BMRDOS ) EHQ(IREL) = RDWORK(IREL) / TP_BMR
!
        IF( BGORTP ) THEN
          WRITE(IRPT,1110) ESD_SPC(ISPC)%EFFENG(IANA), DINT, ESD_ANA(IANA)%DFIMM, ESD_SPC(ISPC)%FWATER, ESD_SPC(ISPC)%ETWATER, &
            DIMM, CFELEV, ESD_SPC(ISPC)%FABOVE(IREL), ESD_SPC(ISPC)%DIFFHT, ESD_ANA(IANA)%DFSED, DSOI_ABOVE, ESD_ANA(IANA)%GAMMA, &
            DSOI_BELOW, DSOI, RDWORK(IREL)
 1110     FORMAT(1P,&
            3X,12('-'),3X,20('-'),3X,18('-'),3X,72('-')  /&
            3X, E12.5,' : MeV/disintegration   : ESD_SPC%EFFENG     : Effective energy from radiological decay'/ &
            3X, E12.5,' : rad/day              : DINT               : Radiological dose from internal contamination'/ &
            3X, E12.5,' : mrad/yr per uCi/m^3  : ESD_ANA%DFIMM      : Dose factor for immersion'/ &
            3X, E12.5,' : unitless             : ESD_SPC%FWATER     : Faction of time the species is immersed in water'/ &
            3X, E12.5,' : hour/day             : ESD_SPC%ETWATER    : Exposure time to water'/ &
            3X, E12.5,' : rad/day              : DIMM               : Radiological dose from water immersion'/ &
            3X, E12.5,' : unitless             : CFELEV             : Conversion factor based on time in pore water'/ &
            3X, E12.5,' : unitless             : FABOVE             : Fraction time spent above sediment'/ &
            3X, E12.5,' : m                    : ESD_SPC%DIFFHT     : Diffusion height of species'/ &
            3X, E12.5,' : Sv-m^3/sec-Bq        : ESD_ANA%DFSED      : Dose factor for external exposure to sediment'/ &
            3X, E12.5,' : rad/day              : DSOI_ABOVE         : Radiation dose from soil - above'/ &
            3X, E12.5,' : MeV/nt               : ESD_ANA%GAMMA      : Gamma (photon) energy'/ &
            3X, E12.5,' : rad/day              : DSOI_BELOW         : Radiation dose from soil - below'/ &
            3X, E12.5,' : rad/day              : DSOI               : Total radiation dose from soil'/ &
            3X, E12.5,' : rad/day              : RDWORK             : Total radiation dose'/&
            3X,12('-'),3X,20('-'),3X,18('-'),3X,72('-') )
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1120) TP_BMR, EHQ(IREL)
 1120     FORMAT(1P, &
            3X, E12.5,' : rad/day              : TP_BMR             : Rad benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless             : EHQ                : Environmental hazard quotient')
        END IF
!
        IF( BGORTP ) THEN
!
          WRITE(IRPT,1220) '   Internal plant burden (pCi/kg wet plant weight) for '//TRIM(ESD_ANA(IANA)%ID)//' on '//&
            TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from rainsplash'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from soil particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from ambient air particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL), PCT, 'Foliar adsorption from wet deposition'
          WRITE(IRPT,1240) CPAI(ISPC,IREL), 'Total internal plant burden'
 1220     FORMAT(/A)
 1230     FORMAT(3X,1P,E12.5,0P,' : ',F8.4,'% : ',A)
 1240     FORMAT(3X,1P,E12.5,' : ',A)
!
          WRITE(IRPT,1220) '   Plant burden (pCi/kg wet plant weight) as consumed by predators for '//&
            TRIM(ESD_ANA(IANA)%ID)//' on '//TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR, PCT, 'Rainsplash to above ground plant parts'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP, PCT, 'Foliar interception from soil particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD, PCT, 'Foliar interception ambient air particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW, PCT, 'Foliar interception from wet deposition'
          WRITE(IRPT,1240) CPAT(ISPC,IREL), 'Total plant burden'
!
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGORTP .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN', &
          UNITS_ORG_RAD_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSRAD', &
          UNITS_ORG_RAD_DOS_TER,NREAL,RDWORK)
        IF( DET_BMRDOS) CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BMRDOS', &
          UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_ORG_RAD_BUR_TER, CWORK )
      IF( DET_DOSRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_TER, RDWORK )
!
      IF( DET_BMRDOS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BMRDOS', UNITS_NONE, EHQ )
!
! *** Optionally write the burden (as consumed by predators) to the food file
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Optionally ouput the body burden and threshold statistics
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_RAD_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_RAD_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally output the radioactive dose and thhreshold statistics
      IF( STA_DOSRAD .OR. THRESHOLD%EXIST1(ISPC,IANA,5) .OR. THRESHOLD%EXIST2(ISPC,IANA,5) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,5)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,5)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,5)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,5)
        CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSRAD', UNITS_ORG_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSRAD', UNITS_ORG_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Radiation dose benchmark (Optional)
      IF( STA_BMRDOS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BMRDOS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OR_TP

      SUBROUTINE OS_QA( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Organic, Nonradioactive (stable)
!!      Species: Aquatic, Animal
!!
!!  History:
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS
!!    Paul W. Eslinger : 24 May 2002 : Change debug writes
!!    Paul W. Eslinger :  3 Jun 2002 : Change sediment ingestion equation.
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger : 15 Aug 2006 : (SCR-1140) Reformat outputs
!!                                     Simplify the equations for food ingestion
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS      ! Logical whether skip all data outputs
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OS_QA' ! Name of this subroutine
      CHARACTER(LEN=6) :: LOC_ID ! Local location ID
!
      INTEGER :: IREL ! Realization index
      INTEGER :: JSPC ! Species index
      INTEGER :: NREC ! Record number for writing to the foods file
!
      REAL :: TMPKU  ! Temporary uptake rate
      REAL :: TMPBCF ! Temporary bioconcentration factor
      REAL :: TMPWAT ! Body burden from water exposure
      REAL :: TMPING ! Body burden from food ingestion
      REAL :: TMPSED ! Body burden from sediment ingestion
      REAL :: TMPAIR ! Body burden from air inhalation
!
      REAL :: IIJ    ! Temporary variable in ingestion equations (feeding rate)
      REAL :: BIOACC ! Temporary bioaccumulation value in ingestion equations
!
      LOGICAL :: EAT_FOOD ! Local flag whether this animal eats food
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGOSQA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Nonradioactive '/ &
          'Species : ',A,' : Aquatic, Animal         '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
      FORALL(IREL=1:NREAL) TWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
! *** Get the Chemical transfer efficiency as a function of KOW for all realizations
      CALL GET_CHEM_EFF( ISPC, IANA, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Determine whether this animal eats any food.  This calculation doesn't depend on any specific
!     realizations and simply makes it easier to write more elegant debug statements
      EAT_FOOD = .FALSE.
      DO JSPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) EAT_FOOD = .TRUE.
      END DO
!
! *** Start the calculations depending on realization
      DO IREL = 1, NREAL
!
!       Uptake of dissolved sources of contaminant for predator species (L/g lipid/day)
        TMPKU  = ( 2.67 * ESD_SPC(ISPC)%FOC * ESD_SPC(ISPC)%RESPRATE * ESD_SPC(ISPC)%CHEM(IREL)) / &
                 ( ESD_SPC(ISPC)%AWD * ESD_LOC(ILOC)%COXYGEN * ESD_SPC(ISPC)%FLIPID )
!
!       Loss rate of contaminant including depuration and metabolism (1/day)
        ESD_SPC(ISPC)%LOSSRATE = TMPKU / ( (10.0**ESD_ANA(IANA)%KOW(IREL))/1000.0 )+ESD_SPC(ISPC)%METBLOSS(IANA,IREL)
!
!       Bioconcentration factor for an organic contaminant (L/g lipid)
        TMPBCF = TMPKU / ( ESD_SPC(ISPC)%LOSSRATE + ESD_SPC(ISPC)%GROWRATE )
!
!       Body burden due to water exposure (ug/g lipid)
        TMPWAT = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * TMPBCF
!
!       Body burden from air inhalation
        IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
          TMPAIR = (EC(IREL)%AIRC+EC(IREL)%VAPOR)*ESD_SPC(ISPC)%INHRATE / (ESD_SPC(ISPC)%WBMASS/ESD_SPC(ISPC)%AWD)
          TMPAIR = TMPAIR * ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / (ESD_SPC(ISPC)%DEPRATE(IANA,IREL)+ESD_SPC(ISPC)%GROWRATE)
        ELSE
          TMPAIR = 0.0
        END IF
!
!       Initialize food ingestion and sediment ingestion body burdens
        TMPING = 0.0
        TMPSED = 0.0
!
!       Skip a lot of calculations if this animal doesn't eat any food
        IF( EAT_FOOD ) THEN
!
          IF( BGOSQA ) THEN
            WRITE(IRPT,1020)
 1020       FORMAT(/'   Food intake calculations for all prey ...'/&
                    '   IIJ = Feeding rate (g prey dry weight/g predator dry weight/day)'/ &
                    '   Index    ID     PREDATE      ALPHAIJ      GROWRATE     RESPRATE     OCAR        F lipid     FLipid(prey)',&
                    '  FOC (prey)     FOC-I         BIOACC       IIJ'/&
                    '   ------ ------ ------------ ------------ ------------ ------------ ------------ ------------ ------------',&
                    ' ------------ ------------ ------------ ------------')
          END IF
!
! ***     Food and sediment ingestion calculations
          BIOACC = 0.0
          DO JSPC = 1, ESD_NUM_SPC
!
!           Skip species not computed or not eaten by the predator
            IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
            IF( .NOT.(PREDATE(ISPC,JSPC).GT.0.0) ) CYCLE
!
!           Feeding rate (g prey dry weight/g predator dry weight/d)
            IIJ = (( ESD_SPC(ISPC)%GROWRATE+ESD_SPC(ISPC)%RESPRATE ) / ESD_SPC(ISPC)%OCAR ) * &
                ( ESD_SPC(JSPC)%AWD / ESD_SPC(ISPC)%AWD ) * ( ESD_SPC(JSPC)%FLIPID / ESD_SPC(ISPC)%FLIPID ) * &
                ( ESD_SPC(ISPC)%FOC / ESD_SPC(JSPC)%FOC )
!
!           Accumulation factor for prey body burden
            BIOACC = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL) * IIJ / (ESD_SPC(ISPC)%LOSSRATE+ESD_SPC(ISPC)%GROWRATE)
!
!           Increment the ingestion and sediment body burdens
            TMPING = TMPING + BIOACC * ESD_SPC(JSPC)%BODYBURDEN(IREL)
            TMPSED = TMPSED + BIOACC * (EC(IREL)%SEDI * ESD_SPC(ISPC)%SEDING)
!
!           Debug outputs
            IF( BGOSQA ) &
              WRITE(IRPT,1021) JSPC, ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL), &
                ESD_SPC(ISPC)%GROWRATE, ESD_SPC(ISPC)%RESPRATE, ESD_SPC(ISPC)%OCAR, ESD_SPC(ISPC)%FLIPID, ESD_SPC(JSPC)%FLIPID, &
                ESD_SPC(JSPC)%FOC, ESD_SPC(ISPC)%FOC, BIOACC, IIJ
 1021         FORMAT(3X,I6,1P,1X,A,12(1X,E12.5))
!
          END DO
!
        END IF
!
!       Total body burden (water exposure + food ingestion + sediment ingestion + air inhalation)
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = TMPWAT + TMPING + TMPSED + TMPAIR
!
! ***   Convert Body Burden to Concentration for terrestrial animal consumption
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
        CING(ISPC,IREL) = CWORK(IREL)
!
!       Optional environmental hazard quotient
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          EHQ(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
! ***   Save food for human consumption (wet weight basis)
        FWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
!
!       Debug outputs
        IF( BGOSQA ) THEN
!
          WRITE(IRPT,1050) IREL, EC(IREL)%PWAT, EC(IREL)%SWAT,EC(IREL)%SEDI, ESD_SPC(ISPC)%BPORE(IREL),&
            ESD_ANA(IANA)%KOW(IREL), ESD_LOC(ILOC)%COXYGEN, ESD_SPC(ISPC)%AWD, ESD_SPC(ISPC)%CHEM(IREL), &
            ESD_SPC(ISPC)%METBLOSS(IANA,IREL), TMPKU, TMPBCF, TMPWAT, TMPING, TMPSED, TMPAIR, CWORK(IREL), FWORK(IREL)
 1050     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units               : Variable           : Description'/ &
            3X,12('-'),3X,19('-'),3X,18('-'),3X,40('-')  /&
            3X,E12.5,' : ug/L                : EC%PWAT            : Pore water concentration'/ &
            3X,E12.5,' : ug/L                : EC%SWAT            : Surface water concentration'/ &
            3X,E12.5,' : ug/kg               : EC%SEDI            : Sediment concentration'/ &
            3X,E12.5,' : unitless            : ESD_SPC%BPORE      : Relative exposure to pore water'/ &
            3X,E12.5,' : unitless            : ESD_ANA%KOW        : log base 10 of the octanol-water partition coefficient'/ &
            3X,E12.5,' : g O2/L              : ESD_LOC%COXYGEN    : Oxygen concentration in surface water'/ &
            3X,E12.5,' : g wet/g dry         : ESD_SPC%AWD        : Wet to dry weight ratio'/&
            3X,E12.5,' : g/g per g O2/g lipid: ESD_SPC%CHEM       : Chemical transfer efficiency'/ &
            3X,E12.5,' : 1/day               : ESD_SPC%METBLOSS   : Metabolism loss rate'/  &
            3X,E12.5,' : L/g lipid/day       : TMPKU              : Uptake rate'/ &
            3X,E12.5,' : L/g lipid           : TMPBCF             : Bioconcentration factor'/ &
            3X,E12.5,' : ug/g lipid          : TMPWAT             : Water exposure part of body burden'/ &
            3X,E12.5,' : ug/g lipid          : TMPING             : Food ingestion part of body burden'/ &
            3X,E12.5,' : ug/g lipid          : TMPSED             : Sediment ingestion part of body burden'/ &
            3X,E12.5,' : ug/g lipid          : TMPAIR             : Inhalation part of body burden'/ &
            3X,E12.5,' : ug/kg wet           : CWORK              : Concentration consumed by terrestrial animals'/&
            3X,E12.5,' : ug/kg wet           : FWORK              : Concentration consumed by human')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1060) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1060     FORMAT(1P, &
            3X,E12.5,' : ug/kg wet           : ESD_SPC%BMTISS     : Tissue benchmard for the EHQ calculation'/ &
            3X,E12.5,' : unitless            : EHQ                : Environmental hazard quotient')
!
          IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
            WRITE(IRPT,1070) EC(IREL)%AIRC,EC(IREL)%VAPOR,ESD_SPC(ISPC)%INHRATE,ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL)
 1070       FORMAT(&
            3X,ES12.5,' : ug/m^3              : EC%AIRC            : Air concentration'/ &
            3X,ES12.5,' : ug/m^3              : EC%VAPOR           : Vapor concentration'/ &
            3X,ES12.5,' : m^3/day             : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
            3X,ES12.5,' : unitless            : ESD_SPC%ALPHAVAP   : Vapor inhalation assimilation efficiency')
          END IF
!
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGOSQA .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_ORG_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_ORG_BUR_TER,NREAL,CWORK)
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'EHQ',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the DETAILED values
      IF( DET_BURDEN ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS(ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'BURDEN',UNITS_ORG_BUR_AQ,CVEC)
      END IF
      IF( DET_EHQ ) CALL WRITE_DETAILS(ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'EHQ',UNITS_NONE,EHQ )
      IF( DET_TFOODS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_ORG_BUR_TER, CWORK )
!
! *** Optionally write the concentration (as consumed) to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold calculations)
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Environmental hazard quotient statistics
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_ORG_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OS_QA

      SUBROUTINE OS_QP( ITIM, ILOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Organic, Nonradioactive (stable)
!!      Species: Aquatic, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  3 Nov 1998 : Version 1.1
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Change to EHQ & reformat
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      LOGICAL :: SKIP_OUTPUTS ! Logical whether skip all data outputs
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OS_QP' ! Name of this subroutine
      INTEGER :: IREL ! Realization index
      INTEGER :: NREC ! Record number for writing to the foods file
      REAL :: TMPKU   ! Contaminant uptake from dissolved sources
      REAL :: TMPBCF  ! Bioconcentration factor
      CHARACTER(LEN=6) :: LOC_ID ! Temporary location ID
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      IF( BGOSQP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Nonradioactive '/ &
          'Species : ',A,' : Aquatic, Plant          '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize temporary vectors
!
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) FWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
! *** Get the Chemical transfer efficiency as a function of KOW for all realizations
!
      CALL GET_CHEM_EFF( ISPC, IANA, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine GET_CHEM_EFF'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      DO IREL = 1, NREAL
!
!       Contaminant uptake from dissolved sources (Thomann 1989)
        TMPKU  = ( 2.67 * ESD_SPC(ISPC)%FOC * ESD_SPC(ISPC)%RESPRATE * ESD_SPC(ISPC)%CHEM(IREL)) / &
                 ( ESD_SPC(ISPC)%AWD * ESD_LOC(ILOC)%COXYGEN * ESD_SPC(ISPC)%FLIPID )
!
!       Contaminant loss rate from metabolism
        ESD_SPC(ISPC)%LOSSRATE = TMPKU / ( (10.0**ESD_ANA(IANA)%KOW(IREL))/1000.0 )
!
!       Bioconcentration factor
        TMPBCF = TMPKU / ( ESD_SPC(ISPC)%LOSSRATE + ESD_SPC(ISPC)%GROWRATE )
!
!       Body burden (consumed by aquatic animals)
        ESD_SPC(ISPC)%BODYBURDEN(IREL)  = ( ESD_SPC(ISPC)%BPORE(IREL)*EC(IREL)%PWAT + &
           (1.0-ESD_SPC(ISPC)%BPORE(IREL))*EC(IREL)%SWAT ) * TMPBCF
!
!       Concentration for consumption by terrestrial animals (save in CPAT - wet weight basis)
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
        CPAT(ISPC,IREL) = CWORK(IREL)
!
!       Concentration in foods for human consumption (wet weight basis)
        FWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) * 1000.0 * ESD_SPC(ISPC)%FLIPID
!
!       Optional environmental hazard quotient
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          EHQ(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
!       Optional debug outputs
        IF( BGOSQP ) THEN
!
          WRITE(IRPT,1010) IREL, ESD_SPC(ISPC)%FOC, ESD_SPC(ISPC)%RESPRATE, ESD_SPC(ISPC)%CHEM(IREL), &
            ESD_SPC(ISPC)%AWD, ESD_LOC(ILOC)%COXYGEN, ESD_SPC(ISPC)%FLIPID, TMPKU, ESD_ANA(IANA)%KOW(IREL), &
            ESD_SPC(ISPC)%LOSSRATE, ESD_SPC(ISPC)%GROWRATE, TMPBCF, ESD_SPC(ISPC)%BPORE(IREL), EC(IREL)%PWAT, &
            EC(IREL)%SWAT, ESD_SPC(ISPC)%BODYBURDEN(IREL), CPAT(ISPC,IREL), FWORK(IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units                         : Variable           : Description'/ &
            3X,12('-'),3X,29('-'),3X,18('-'),3X,50('-')  /&
            3X, E12.5,' : g organic carbon/g dry weight : ESD_SPC%FOC        : Fraction organic carbon'/ &
            3X, E12.5,' : 1/day                         : ESD_SPC%RESPRATE   : Oxygen use rate'/ &
            3X, E12.5,' : g/g per g O2/g lipid          : ESD_SPC%CHEM       : Chemical transfer efficiency'/ &
            3X, E12.5,' : g wet/g dry                   : ESD_SPC%AWD        : Wet to dry weight ratio'/ &
            3X, E12.5,' : g Oxygen/L                    : ESD_LOC%COXYGEN    : Oxygen concentration in the surface water'/ &
            3X, E12.5,' : g lipid/g wet                 : ESD_SPC%FLIPID     : Fraction lipid'/ &
            3X, E12.5,' : L/g lipid/day                 : TMPKU              : Contaminant uptake from dissolved sources'/ &
            3X, E12.5,' : unitless                      : ESD_ANA%KOW        : Log base 10 of octanol-water partition coefficient'/&
            3X, E12.5,' : 1/day                         : ESD_SPC%LOSSRATE   : Loss rate from metabolism'/ &
            3X, E12.5,' : 1/day                         : ESD_SPC%GROWRATE   : Growth rate'/ &
            3X, E12.5,' : L/g lipid                     : TMPBCF             : Bioconcentration factor for contaminant'/ &
            3X, E12.5,' : unitless                      : ESD_SPC%BPORE      : Relative exposure to pore water'/ &
            3X, E12.5,' : ug/L                          : EC%PWAT            : Pore water concentration'/ &
            3X, E12.5,' : ug/L                          : EC%SWAT            : Surface water concentration'/ &
            3X, E12.5,' : ug/g lipid                    : ESD_SPC%BODYBURDEN : Body burden (consumed by aquatic animals)'/ &
            3X, E12.5,' : ug/kg wet                     : CPAT               : Concentration consumed by terrestrial animals'/ &
            3X, E12.5,' : ug/kg wet                     : FWORK              : Concentration consumed by humans')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1020) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1020     FORMAT(1P, &
            3X, E12.5,' : ug/kg wet                     : ESD_SPC%BMTISS     : Tissue benchmard for the EHQ calculation'/ &
            3X, E12.5,' : unitless                      : EHQ                : Environmental hazard quotient')
!
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
!     After the computations are done for all realizations
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGOSQP .AND. NREAL.GT.1 ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_ORG_BUR_AQ,NREAL,CVEC)
        CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'TFOODS',UNITS_ORG_BUR_TER,NREAL,CWORK)
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          CALL WRITE_BUGS(ILOC,LOC_ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'EHQ',UNITS_NONE,NREAL,EHQ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
      IF( SKIP_OUTPUTS ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,LOC_ID,SOIL_ID,ESD_ANA(IANA)%ID,ESD_SPC(ISPC)%ID,'BURDEN',UNITS_ORG_BUR_AQ,CVEC )
      END IF
      IF( DET_EHQ ) THEN
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, 'EHQ', UNITS_NONE, EHQ )
      END IF
      IF( DET_TFOODS ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'TFOODS', UNITS_ORG_BUR_TER, CWORK )
!
! *** Optionally write the concentration consumed by humans to the food file
      IF( FOODS .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC' .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lover level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Body burden statistics (and threshold limits)
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        FORALL(IREL=1:NREAL) CVEC(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CVEC, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_BUR_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          LOC_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_BUR_AQ, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Environmental hazard quotient statistics
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, LOC_ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Foods consumed by terrestrial species
      IF( STA_TFOODS ) THEN
        TL1 = 0.0
        TL2 = 0.0
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - TFOODS'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'TFOODS', UNITS_ORG_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OS_QP

      SUBROUTINE OS_TA( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Organic, Nonradioactive (stable)
!!      Species: Terrestrial, Animal
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 15 Mar 1999 : Version 1.2
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 29 May 2002 : Change debug writes
!!    Paul W. Eslinger : 28 Apr 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 26 Oct 2004 : Add optional secondary transfer factor
!!    Paul W. Eslinger : 20 May 2005 : Add particulate fraction for far-field air
!!    Paul W. Eslinger : 30 Jun 2005 : Add error checking on energy intake
!!    Paul W. Eslinger :  1 Sep 2005 : Change debug outputs
!!    Paul W. Eslinger :  6 Sep 2006 : (SCR-1140) Reformat outputs
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OS_TA' ! Name of this subroutine
      INTEGER :: IREL ! Realization index
      INTEGER :: JSPC ! Species index
!
      REAL :: DDERMS  ! Applied daily dose from dermal contact with soil
      REAL :: DDERMW  ! Applied daily dose from dermal contact with water
!
      REAL :: NIR    ! Normalized intake rate
      REAL :: DIV    ! Applied daily dose from vapor inhalation
      REAL :: DIP    ! Applied daily dose from particulate inhalation
      REAL :: CIV    ! Equilibrium body burden from vapor inhalation
      REAL :: CIP    ! Equilibrium body burden from particulate inhalation
!
      REAL :: DINGW  ! Applied daily dose from water ingestion
      REAL :: CINGW  ! Equilibrium body burden from water ingestion
      REAL :: DINGS  ! Applied daily dose from soil ingestion
      REAL :: CINGS  ! Equilibrium body burden from soil ingestion
      REAL :: DINGF  ! Applied daily dose from food ingestion
      REAL :: CINGF  ! Equilibrium body burden from food ingestion
!
      REAL :: SUMPROD ! Temporary sum variable
      REAL :: PROD    ! Temporary - metabilizable energy from one prey species
      REAL :: CONJ    ! Concentration in prey species
!
      INTEGER :: NREC ! Record number for writing to the foods file
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGOSTA ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//,A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Nonradioactive '/ &
          'Species : ',A,' : Terrestrial, Animal     '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) FWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
! ***   Calculate Dermal Contact with Soil (absorbed daily dose)
        DDERMS = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCS * &
          ESD_SPC(ISPC)%SADHER * EC(IREL)%SOIL * CFDERMS * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * &
          ( ESD_SPC(ISPC)%ALPHADS(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT )
!
! ***   Calculate Dermal Contact with Water (absorbed daily dose)
        DDERMW = ESD_SPC(ISPC)%SA * ESD_SPC(ISPC)%PCW * &
          ESD_SPC(ISPC)%ETWATER * EC(IREL)%WATER * CFDERMW * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI * &
          ( ESD_SPC(ISPC)%ALPHADW(IANA,IREL) / ESD_SPC(ISPC)%WEIGHT )
!
!       Absorbed daily dose from dermal contact with contaminant soil
        DDER(IREL) = DDERMS + DDERMW
!
!       Equilibrium body burden from dermal contact with contaminant in soil
        CDER(ISPC,IREL) = DDER(IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL)
!
        IF( BGOSTA ) THEN
          WRITE(IRPT,1010) IREL, EC(IREL)%SOIL, ESD_SPC(ISPC)%SA, ESD_SPC(ISPC)%PCS, ESD_SPC(ISPC)%SADHER, &
            ESD_SPC(ISPC)%THETA, ESD_SPC(ISPC)%PSI, ESD_SPC(ISPC)%ALPHADS(IANA,IREL), ESD_SPC(ISPC)%WEIGHT, DDERMS, &
            ESD_SPC(ISPC)%PCW, ESD_SPC(ISPC)%ETWATER, EC(IREL)%WATER, ESD_SPC(ISPC)%ALPHADW(IANA,IREL), DDERMW, &
            DDER(IREL), ESD_SPC(ISPC)%DEPRATE(IANA,IREL), CDER(ISPC,IREL)
 1010     FORMAT(/'   Realization Number : ',I0,1P,/ &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : ug/kg                        : EC%SOIL            : Concentration of contaminant in soil'/ &
            3X,E12.5,' : cm^2                         : ESD_SPC%SA         : Surface area of animal species'/ &
            3X,E12.5,' : 1/day                        : ESD_SPC%PCS        : Fraction of species surface area contacting soil/day'/&
            3X,E12.5,' : mg/cm^2                      : ESD_SPC%SADHER     : Skin adherence factor for animal species'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%THETA      : Area use factor (contaminated/home-range area)'/&
            3X,E12.5,' : unitless                     : ESD_SPC%PSI        : Seasonality factor (fraction of year in area)'/ &
            3X,E12.5,' : unitless                     : ESD_SPC%ALPHADS    : Soil dermal absorption factor (contaminant, species)'/&
            3X,E12.5,' : kg wet                       : ESD_SPC%WEIGHT     : Body weight of animal species'/ &
            3X,E12.5,' : ug/kg/day                    : DDERMS             : Absorbed daily dose from dermal contact with soil'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%PCW        : Fraction of species surface area contacting water/dy'/&
            3X,E12.5,' : hr/day                       : ESD_SPC%ETWATER    : Average exposure time to water per day'/ &
            3X,E12.5,' : ug/L                         : EC%WATER           : Concentration of contaminant in water'/ &
            3X,E12.5,' : cm/hr                        : ESD_SPC%ALPHADW    : Water dermal absorption factor(contaminant, species)'/&
            3X,E12.5,' : ug/kg/day wet                : DDERMW             : Absorbed daily dose from dermal contact with water'/&
            3X,E12.5,' : ug/kg/day wet                : DDER               : Absorbed daily dose from dermal contact'/&
            3X,E12.5,' : 1/day                        : ESD_SPC%DEPRATE    : Depuration rate of contaminant by animal species'/&
            3X,E12.5,' : ug/kg wet                    : CDER               : Equilibrium body burden from dermal exposure')
        END IF
!
! ***   Calculate Inhalation Dose from Vapor
!
        IF( ESD_ANA(IANA)%HENRY>0.0) THEN
!
!         Absorbed daily dose from inhalation of vapor
          DIV = ((ESD_SPC(ISPC)%INHRATE * (EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)+EC(IREL)%VAPOR) ) / &
            ESD_SPC(ISPC)%WEIGHT) * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equilibrium body burden from inhalation of vapor
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGOSTA ) THEN
            WRITE(IRPT,1030) EC(IREL)%AIRC, ESD_ANA(IANA)%PARTICLE, ESD_ANA(IANA)%HENRY, EC(IREL)%VAPOR, ESD_SPC(ISPC)%INHRATE, &
              ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL), DIV, CIV
 1030       FORMAT( 1P, &
              3X,E12.5,' : ug/m^3                       : EC%AIRC            : Concentration of contaminant in air'/ &
              3X,E12.5,' : ug/m^3                       : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : Pa-m^3/mol                   : ESD_ANA%HENRY      : Henrys law constant'/&
              3X,E12.5,' : ug/m^3                       : EC%VAPOR           : Concentration of contaminant in vapor'/ &
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : unitless                     : ESD_SPC%ALPHAVAP   : Inhalation absorption factor of contaminant'/ &
              3X,E12.5,' : ug/kg/day wet                : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : ug/kg wet                    : CIV                : Equilibrium body burden from vapor inhalation')
           END IF
        ELSE
!
!         Absorbed daily dose from inhalation of vapor
          DIV = ((ESD_SPC(ISPC)%INHRATE * EC(IREL)%AIRC*(1.0-ESD_ANA(IANA)%PARTICLE)) / ESD_SPC(ISPC)%WEIGHT) * &
            ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equilibrium body burden from inhalation of vapor
          CIV = DIV * ( ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGOSTA ) THEN
            WRITE(IRPT,1040) ESD_SPC(ISPC)%INHRATE, EC(IREL)%AIRC, ESD_ANA(IANA)%PARTICLE,  DIV, CIV
 1040       FORMAT( 1P, &
              3X,E12.5,' : m^3/day                      : ESD_SPC%INHRATE    : Resting inhalation rate for animal species'/ &
              3X,E12.5,' : ug/m^3                       : EC%AIRC            : Concentration of contaminant in air'/ &
              3X,E12.5,' : ug/m^3                       : ESD_ANA%PARTICLE   : Air concentration of particulate-bound contaminant'/&
              3X,E12.5,' : ug/kg/day wet                : DIV                : Applied daily dose from vapor inhalation'/ &
              3X,E12.5,' : ug/kg wet                    : CIV                : Equilibrium body burden from vapor inhalation')
          END IF
        END IF
!
!       Absorbed daily dose from inhalation of particualtes
        DIP = ESD_SPC(ISPC)%INHRATE * (EC(IREL)%PARTIC+EC(IREL)%AIRC*ESD_ANA(IANA)%PARTICLE) * ESD_SPC(ISPC)%THETA * &
          ESD_SPC(ISPC)%PSI / ESD_SPC(ISPC)%WEIGHT
!
!       Equilibrium body burden from inhalation of particulates
        CIP = DIP * ( ESD_SPC(ISPC)%ALPHAPAR(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
! ***   Total absorbed daily dose from inhalation
        DINH(IREL) = DIV + DIP
!
!       Total equilibrium body burden from inhalation
        CINH(ISPC,IREL) = CIV + CIP
!
        IF( BGOSTA ) THEN
          WRITE(IRPT,1050) DIP, CIP, DINH(IREL), CINH(ISPC,IREL)
 1050     FORMAT( 1P, &
            3X,E12.5,' : ug/kg/day wet                : DIP                : Applied daily dose from particulate inhalation'/ &
            3X,E12.5,' : ug/kg wet                    : CIP                : Equilibrium body burden from particulate inhalation'/ &
            3X,E12.5,' : ug/kg/day wet                : DINH               : Applied daily dose from all inhalation'/ &
            3X,E12.5,' : ug/kg wet                    : CINH               : Equilibrium body burden from all inhalation')
        END IF
!
! ***   Total absorbed daily dose from water
        DINGW = ( ( ESD_SPC(ISPC)%WATERING * EC(IREL)%WATER ) / ESD_SPC(ISPC)%WEIGHT ) * &
          ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!       Total equilibrium body burden from inhalation
        CINGW = DINGW * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
        IF( BGOSTA ) THEN
          WRITE(IRPT,1060) ESD_SPC(ISPC)%WATERING, EC(IREL)%WATER, DINGW, CINGW
 1060     FORMAT( 1P, &
            3X,E12.5,' : L/day                        : ESD_SPC%WATERING   : Water ingestion rate for animal species'/ &
            3X,E12.5,' : ug/kg                        : EC%WATER           : Concentration of contaminant in water'/ &
            3X,E12.5,' : ug/kg/day wet                : DINGW              : Applied daily dose from water ingestion'/ &
            3X,E12.5,' : ug/kg wet                    : CINGW              : Equilibrium body burden from water ingestion')
        END IF
!
! ***   Calculate the total metabilizable energy from food (kcal/kg prey wet wt) [Use variable SUMPROD]
!       This is used in both food ingestion and soil ingestion calculations
        IF( BGOSTA ) THEN
          WRITE(IRPT,1070) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1070     FORMAT(/'   Species ',A,' (',A,') Metabilizable Energy From Prey'/&
            3X,'Prey   Predation',13X,'Gross Energy',20X,'Assimilation Efficiency',1X,'Metabilizable Energy'/&
            3X,'------',1X,21('-'),1X,31('-'),1X,23('-'),1X,32('-'))
        END IF
!
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate the metabilizable energy this prey (kcal/kg prey wet wt)
          PROD = PREDATE(ISPC,JSPC) * ESD_SPC(JSPC)%GE * ESD_SPC(JSPC)%AE
          IF( PROD .LE. 0.0 ) THEN
            IERR = 2
            MESSAG(1) = 'Food is ingested but no metabilizable energy is defined'
            MESSAG(2) = 'Location '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Species  '//ESD_SPC(ISPC)%ID //' Eating '//ESD_SPC(JSPC)%ID
            MESSAG(5) = 'Check the AE and GE values on the SPECIES keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Sum the metabilizable energy over all prey
          SUMPROD = SUMPROD + PROD
!
          IF( BGOSTA ) THEN
            WRITE(IRPT,1080) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), ESD_SPC(JSPC)%GE, ESD_SPC(JSPC)%AE, PROD
 1080       FORMAT(1P,3X,A, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kcal/kg wet weight', &
              1X,E12.5,' unitless', &
              3X,E12.5,' kcal/kg prey wet wt')
          END IF
        END DO
!
!       Total normalized intake rate (kg prey wet weight/kg predator body weight/day)
        IF( SUMPROD .GT. 0.0 ) THEN
          ESD_SPC(ISPC)%NIRTOT(IREL) = ESD_SPC(ISPC)%FMR / ( SUMPROD * ESD_SPC(ISPC)%WEIGHT )
        ELSE
          ESD_SPC(ISPC)%NIRTOT(IREL) = 0.0
        END IF
!
! ***    Calculate food ingestion dose from all species eaten
        IF( BGOSTA ) THEN
          WRITE(IRPT,1090) TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME)
 1090     FORMAT(/'   Species ',A,' (',A,') Food ingestion calculations'/&
            3X,'Prey   Predation',13X,'Normalized Intake',32X,'Prey Concentration'/&
            3X,'------',1X,21('-'),1X,48('-'),1X,31('-'))
        END IF
!
        SUMPROD = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!
!         Skip species not computed or not eaten
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
!
!         Calculate normalized intake rate for this prey species
          NIR = PREDATE(ISPC,JSPC) * ESD_SPC(ISPC)%NIRTOT(IREL)
!
!         Obtain the concentration of contaminant in the prey species
          IF( ESD_SPC(JSPC)%TYPE.EQ.'TP' .OR. ESD_SPC(JSPC)%TYPE.EQ.'QP' ) THEN
            CONJ = CPAT(JSPC,IREL)
          ELSE
            CONJ = CING(JSPC,IREL) + CINH(JSPC,IREL) + CDER(JSPC,IREL)
          END IF
!
!         Add the contaminant intake for this prey species to the total intake
          SUMPROD = SUMPROD + (CONJ * NIR)
!
          IF( BGOSTA ) THEN
            WRITE(IRPT,1110) ESD_SPC(JSPC)%ID, PREDATE(ISPC,JSPC), NIR, CONJ
 1110       FORMAT(3X,A,1P, &
              1X,E12.5,' Fraction', &
              1X,E12.5,' kg prey wet wt/kg predator wet wt/d', &
              1X,E12.5,' ug/kg prey wet wt')
          END IF
!
        END DO
!
!       Applied daily dose from all prey consumption (ug/kg/day)
        DINGF = SUMPROD * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!       Equilibrium body burden from all prey consumption (ug/kg body wet weight)
        CINGF = DINGF * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
        IF( BGOSTA ) THEN
          WRITE(IRPT,1120) DINGF, CINGF
 1120     FORMAT(/ 1P, &
            '      Value     : Units                        : Variable           : Description'/ &
            3X,12('-'),3X,28('-'),3X,18('-'),3X,65('-')  /&
            3X,E12.5,' : ug/kg/day wet                : DINGF              : Applied daily dose from food ingestion'/ &
            3X,E12.5,' : ug/kg wet                    : CINGF              : Equilibrium body burden from food ingestion')
        END IF
!
! ***   Calculate soil ingestion dose
        IF( ESD_SPC(ISPC)%NIRTOT(IREL) .GT. 0.0 ) THEN
!
!         Applied daily dose from soil consumption (ug/kg/day)
          DINGS = ESD_SPC(ISPC)%SOILING * ESD_SPC(ISPC)%NIRTOT(IREL) * EC(IREL)%SOIL * &
            ESD_SPC(ISPC)%FDW * ESD_SPC(ISPC)%THETA * ESD_SPC(ISPC)%PSI
!
!         Equilibrium body burden from soil consumption (ug/kg body wet weight)
          CINGS = DINGS * ( ESD_SPC(ISPC)%ALPHAING(IANA,IREL) / ESD_SPC(ISPC)%DEPRATE(IANA,IREL) )
!
          IF( BGOSTA ) THEN
            WRITE(IRPT,1130) ESD_SPC(ISPC)%FMR, ESD_SPC(ISPC)%NIRTOT(IREL), ESD_SPC(ISPC)%SOILING, EC(IREL)%SOIL, &
              ESD_SPC(ISPC)%FDW, DINGS, CINGS
 1130       FORMAT(1P, &
              3X,E12.5,' : kcal/day                     : ESD_SPC%FMR        : free-living metabolic rate of predator species'/ &
              3X,E12.5,' : kg prey wt/kg predator wt/d  : ESD_SPC%NIRTOT     : Total normalized intake rate'/ &
              3X,E12.5,' : kg soil ingested/kg dry diet : ESD_SPC%SOILING    : Soil ingestion rate'/ &
              3X,E12.5,' : ug/kg                        : EC%SOIL            : Concentration of contaminant in soil'/ &
              3X,E12.5,' : kg dry/kg wet                : ESD_SPC%FDW        : Conversion factor, dry diet to wet diet'/ &
              3X,E12.5,' : ug/kg/day wet                : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : ug/kg wet                    : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
!
        ELSE
!
!         Applied daily dose from soil consumption (ug/kg/day)
          DINGS = 0.0
!
!         Equilibrium body burden from soil consumption (ug/kg body wet weight)
          CINGS = 0.0
!
          IF( BGOSTA ) THEN
            WRITE(IRPT,1140) DINGS, CINGS
 1140       FORMAT( 1P, &
              3X,E12.5,' : ug/kg/day                    : DINGS              : Applied daily dose from soil ingestion'/ &
              3X,E12.5,' : ug/kg                        : CINGS              : Equilibrium body burden from soil ingestion')
          END IF
!
        END IF
!
! ***   Applied daily dose from all sources (ug/kg/day)
        DING(IREL) = DINGF + DINGS + DINGW
!
!       Equilibrium body burden from all sources (ug/kg body wet weight)
        CING(ISPC,IREL) = CINGF + CINGS + CINGW
!
! ***   Calculate Total Absorbed Dose
        DHAZ(IREL)  = DING(IREL) + DINH(IREL) + DDER(IREL)
        CWORK(IREL) = CING(ISPC,IREL) + CINH(ISPC,IREL) + CDER(ISPC,IREL)
        IF( ESD_SPC(ISPC)%TRANS ) CWORK(IREL) = CWORK(IREL)*ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
!
!       Optional environmental hazard quotient
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) EHQ(IREL) = CWORK(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
! ***   Save the concentration for human consumption (wet weight basis)
        FWORK(IREL) = CWORK(IREL)
!
        IF( BGOSTA ) THEN
          WRITE(IRPT,1150) DING(IREL), CING(ISPC,IREL), DHAZ(IREL), CWORK(IREL), FWORK(IREL)
 1150     FORMAT( 1P, &
            3X,E12.5,' : ug/kg/day wet                : DING               : Applied daily dose from all ingestion'/ &
            3X,E12.5,' : ug/kg wet                    : CING               : Equilibrium body burden from all ingestion'/ &
            3X,E12.5,' : ug/kg/day wet                : DHAZ               : Total absorbed daily dose'/ &
            3X,E12.5,' : ug/kg wet                    : CWORK              : Total equilibrium body burden'/ &
            3X,E12.5,' : ug/kg wet                    : FWORK              : Total equilibrium body burden for human consumption')
          IF( ESD_SPC(ISPC)%TRANS ) WRITE(IRPT,1160) ESD_SPC(ISPC)%TRANSVAL(IANA,IREL)
 1160     FORMAT( 1P, &
            3X,E12.5,' : ug second/ug primary         : TRANSVAL           : Transfer factor')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1170) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1170     FORMAT(1P,&
            3X,E12.5,' : ug/kg wet                    : ESD_SPC%BMTISS     : Tissue benchmark' /&
            3X,E12.5,' : unitless                     : EHQ                : Environmental hazard quotient')
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGOSTA .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_ORG_BUR_TER,NREAL,CWORK)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSDER',UNITS_ORG_DOS_TER,NREAL,DDER)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSINH',UNITS_ORG_DOS_TER,NREAL,DINH)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSING',UNITS_ORG_DOS_TER,NREAL,DING)
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'DOSHAZ',UNITS_ORG_DOS_TER,NREAL,DHAZ)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_ORG_BUR_TER, CWORK )
      IF( DET_DOSDER ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSDER', UNITS_ORG_DOS_TER, DDER )
      IF( DET_DOSINH ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSINH', UNITS_ORG_DOS_TER, DINH )
      IF( DET_DOSING ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSING', UNITS_ORG_DOS_TER, DING )
      IF( DET_DOSHAZ ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'DOSHAZ', UNITS_ORG_DOS_TER, DHAZ )
      IF( DET_EHQ ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,ESD_LOC(ILOC)%ID,SOIL_ID,ESD_ANA(IANA)%ID,&
        ESD_SPC(ISPC)%ID,'EHQ',UNITS_NONE,EHQ )
!
! *** Optionally write the burden (as consumed) to the food file
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Optionally output body burden statistics and threshold values
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally output dermal dose statistics and threshold values
      IF( STA_DOSDER .OR. THRESHOLD%EXIST1(ISPC,IANA,2) .OR. THRESHOLD%EXIST2(ISPC,IANA,2) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,2)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,2)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,2)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,2)
        CALL USTAT_ECEM( DDER, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSDER'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSDER ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSDER', UNITS_ORG_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSDER', UNITS_ORG_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally output inhalation dose statistics and threshold values
      IF( STA_DOSINH .OR. THRESHOLD%EXIST1(ISPC,IANA,4) .OR. THRESHOLD%EXIST2(ISPC,IANA,4) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,4)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,4)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,4)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,4)
        CALL USTAT_ECEM( DINH, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSINH'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSINH ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSINH', UNITS_ORG_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSINH', UNITS_ORG_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally output ingestion dose statistics and threshold values
      IF( STA_DOSING .OR. THRESHOLD%EXIST1(ISPC,IANA,3) .OR. THRESHOLD%EXIST2(ISPC,IANA,3) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,3)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,3)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,3)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,3)
        CALL USTAT_ECEM( DING, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSING'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_DOSING ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'DOSING', UNITS_ORG_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'DOSING', UNITS_ORG_DOS_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Optionally output total dose statistics and threshold values
      IF( STA_DOSHAZ ) THEN
        CALL USTAT( DHAZ, NREAL, TWORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - DOSHAZ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
          'DOSHAZ', UNITS_ORG_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
! *** Environmental hazard quotient statistics
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OS_TA

      SUBROUTINE OS_TP( ITIM, ILOC, ISPC, IANA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the impact equation for the following case:
!!      Analyte: Organic, Nonradioactive (stable)
!!      Species: Terrestrial, Plant
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 15 Mar 1999 : Version 1.2
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 18 Nov 2004 : Change leaf deposition equations
!!    Paul W. Eslinger :  6 Dec 2004 : Change dependencies for IFRACDRY,
!!                                     IFRACWET, LWEATHDRY, and LWEATHWET.
!!    Paul W. Eslinger : 13 Jan 2005 : (SCR-1070) Fix riparian/aquatic pairing
!!    Paul W. Eslinger : 17 Jan 2005 : Wet leaf deposition is applied
!!                                     only for UPLAND locations
!!    Paul W. Eslinger : 20 Jan 2005 : Adjust leaf deposition totals
!!    Paul W. Eslinger :  9 Feb 2005 : Adjust leaf deposition totals again
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Adjust leaf deposition totals again
!!    Paul W. Eslinger : 15 Jun 2005 : SCR-1080 - Change HUMAN food options
!!    Paul W. Eslinger : 31 Aug 2006 : (SCR-1140) Reformat outputs
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Make consistent output labels
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!********************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Media_Mod
      USE Threshold_Mod
      USE Results_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: ISPC ! Species index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'OS_TP' ! Name of this subroutine
      INTEGER :: IREL ! Realization index
!
      REAL :: TMPCPAR ! Concentration on leaf surface from rain splash
      REAL :: TMPCPAD ! Concentration on leaf surface from dry deposition
      REAL :: TMPCPAU ! Concentration in plant from root uptake
      REAL :: TMPCPAV ! Concentration in plant from vapor uptake
      REAL :: TMPCPAP ! Concentration on leaf surface from particulate deposition
      REAL :: TMPCPAW ! Concentration on leaf surface from wet deposition
!
      REAL :: KPA1 ! Plant-air partition coefficient to above-ground parts
      REAL :: KPS2 ! Calculated bioconcentration factor
!
      REAL :: PCT     ! Temporary percent variable
      INTEGER :: NREC ! Record number for writing to the foods file
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug entry message
!
      IF( BGOSTP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_SPC(ISPC)%ID, ESD_TIM(ITIM)%TIME
 1000   FORMAT(//,A,' - Detailed Equation Calculations'/ &
          '------------------------------------------'/ &
          'Location: ',A                              / &
          'Analyte : ',A,' : Organic, Nonradioactive '/ &
          'Species : ',A,' : Terrestrial, Plant      '/ &
          'Time    : ',I0 / &
          '------------------------------------------')
      END IF
!
!---------------------------------------------------------------------------------------------------
! Start the computations
!---------------------------------------------------------------------------------------------------
!
! *** Initialize
!
      FORALL(IREL=1:NREAL) CWORK(IREL) = 0.0
      FORALL(IREL=1:NREAL) EHQ(IREL)   = 0.0
!
! *** Compute the vapor and particulate concentrations
!
      CALL COMPUTE_VAPOR_SOIL( ILOC, IANA, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL COMPUTE_PARTIC( ILOC, ISPC, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------------------------------------
! *** Loop over all realizations
!-----------------------------------------------------------------------------------------------------
!
      DO IREL = 1, NREAL
!
! ***   Calculate rain splash effect on leaf external surface
        IF( ESD_SPC(ISPC)%EMERGENT ) THEN
          TMPCPAR = 0.0
          IF( BGOSTP ) THEN
            WRITE(IRPT,1010) IREL, ESD_SPC(ISPC)%KPS1(IREL), TMPCPAR
 1010       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value     : Units                : Variable           : Description'/ &
              3X,12('-'),3X,19('-'),3X,18('-'),3X,50('-')  /&
              3X, E12.5,' : pCi/kg wet           : TMPCPAR            : Rain splash concentration on leaf surface')
          END IF
        ELSE
          TMPCPAR = EC(IREL)%SOIL * ESD_SPC(ISPC)%KPS1(IREL)
          IF( BGOSTP ) THEN
            WRITE(IRPT,1020) IREL, EC(IREL)%SOIL, ESD_SPC(ISPC)%KPS1(IREL), TMPCPAR
 1020       FORMAT(/'   Realization Number : ',I0,1P,/ &
              '      Value     : Units                : Variable           : Description'/ &
              3X,12('-'),3X,19('-'),3X,18('-'),3X,50('-')  /&
              3X, E12.5,' : pCi/kg               : EC%SOIL            : Soil concentration'/ &
              3X, E12.5,' : kg soil/kg plant wet : ESD_SPC%KPS1       : Plant-soil partition from rain splash'/ &
              3X, E12.5,' : pCi/kg wet           : TMPCPAR            : Rain splash concentration on leaf surface')
          END IF
        END IF
!
! ***   Calculate dry deposition effect on leaf external surface
        TMPCPAD = EC(IREL)%AIRD * ESD_SPC(ISPC)%IFRACDRY(IANA,IREL) / &
                  (ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL) )
        IF( BGOSTP ) THEN
          WRITE(IRPT,1030) EC(IREL)%AIRD, ESD_SPC(ISPC)%IFRACDRY(IANA,IREL), ESD_SPC(ISPC)%BIOMASS(IREL), &
            ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL), TMPCPAD
 1030     FORMAT(1P, &
            3X, E12.5,' : kg/m^2/yr            : EC%AIRD            : Air deposition'/ &
            3X, E12.5,' : unitless             : ESD_SPC%IFRACDRY   : Leaf intersection fraction - dry deposition'/ &
            3X, E12.5,' : kg/m^2               : ESD_SPC%BIOMASS    : Standing plant biomass per unit area'/ &
            3X, E12.5,' : 1/yr                 : ESD_SPC%LWEATHDRY  : Leaf weathering rate - dry deposition'/ &
            3X, E12.5,' : ug/kg                : TMPCPAD            : Concentration in above-ground plant from air deposition')
        END IF
!
! ***   Calculate wet deposition effect on leaf external surface (only for upland irrigated locations)
        IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
          TMPCPAW = EC(IREL)%WATER * (IRIG_AMT/SEASON_GROW) * ESD_SPC(ISPC)%IFRACWET(IANA,IREL) * 10.0 / &
                    ( ESD_SPC(ISPC)%BIOMASS(IREL) * ESD_SPC(ISPC)%LWEATHWET(IANA,IREL) )
          IF( BGOSTP ) THEN
            WRITE(IRPT,1040) EC(IREL)%WATER, IRIG_AMT, SEASON_GROW, ESD_SPC(ISPC)%IFRACWET(IANA,IREL), &
              ESD_SPC(ISPC)%LWEATHWET(IANA,IREL), TMPCPAW
 1040       FORMAT(1P, &
              3X, E12.5,' : ug/L                 : EC%WATER           : Irrigation water concentration'/ &
              3X, E12.5,' : cm                   : IRIG_AMT           : Amount of irrigation water applied in a year'/&
              3X, E12.5,' : yr                   : SEASON_GROW        : Length of the growing season'/&
              3X, E12.5,' : unitless             : ESD_SPC%IFRACWET   : Leaf intersection fraction - wet deposition'/ &
              3X, E12.5,' : 1/yr                 : ESD_SPC%LWEATHWET  : Leaf weathering rate - wet deposition'/ &
              3X, E12.5,' : ug/kg wet            : TMPCPAW            : Concentration in above-ground plant from irrigation water')
          END IF
        ELSE
          TMPCPAW = 0.0
          IF( BGOSTP ) THEN
            WRITE(IRPT,1050) TMPCPAW
 1050       FORMAT(1P, &
              3X, E12.5,' : ug/kg wet            : TMPCPAW            : Concentration in above-ground plant from irrigation water')
          END IF
        END IF
!
! ***   Calculate local particulate deposition effect on leaf external surface
        TMPCPAP = EC(IREL)%PARTIC * ESD_SPC(ISPC)%KPA2(IREL)
        IF( BGOSTP ) THEN
          WRITE(IRPT,1060) EC(IREL)%PARTIC, ESD_SPC(ISPC)%KPA2(IREL), TMPCPAP
 1060     FORMAT(1P, &
            3X, E12.5,' : ug/m^3               : EC%PARTIC          : Particulate concentration in air'/ &
            3X, E12.5,' : m^3/kg wet           : ESD_SPC%KPA2       : Plant-air partition coefficient for above-ground plant'/&
            3X, E12.5,' : ug/kg wet            : TMPCPAP            : Concentration of contaminant in above-ground plant')
        END IF
!
! ***   Calculate Root Uptake
        KPS2 = 7.7 * (10.0**ESD_ANA(IANA)%KOW(IREL))**(-0.58)
        IF( ESD_SPC(ISPC)%EMERGENT ) THEN
          TMPCPAU = EC(IREL)%SEDI * KPS2
        ELSE
          TMPCPAU = EC(IREL)%SOIL * KPS2
        END IF
        IF( BGOSTP ) THEN
          WRITE(IRPT,1070) ESD_ANA(IANA)%KOW(IREL), EC(IREL)%SOIL, EC(IREL)%SEDI, KPS2, TMPCPAU
 1070     FORMAT(1P, &
            3X, E12.5,' : none                 : ESD_ANA%KOW        : log base 10 of octanol-water partition coefficient'/&
            3X, E12.5,' : ug/kg                : EC%SOIL            : Soil concentration'/ &
            3X, E12.5,' : ug/kg                : EC%SEDI            : Sediment concentration'/ &
            3X, E12.5,' : kg-soil/kg plant dry : KPS2               : Calculated bioconcentration factor'/ &
            3X, E12.5,' : ug/kg wet            : TMPCPAU            : Concentration in above-ground plant from root uptake')
        END IF
!
! ***   Calculate foliar uptake from vapor
        IF( ESD_ANA(IANA)%HENRY > 0.0 ) THEN
          KPA1 = ( ESD_SPC(ISPC)%FPA + ( ESD_SPC(ISPC)%FPW + ESD_SPC(ISPC)%FPL * &
            (10.0**ESD_ANA(IANA)%KOW(IREL)) ) * &
            (( GASCON * ( ESD_LOC(ILOC)%TEMP + 273.15 )) / ESD_ANA(IANA)%HENRY )) * &
            ( 1.0 / ESD_SPC(ISPC)%RHOP )
          TMPCPAV = (EC(IREL)%VAPOR+EC(IREL)%AIRC) * KPA1
          IF( BGOSTP ) THEN
            WRITE(IRPT,1080) ESD_SPC(ISPC)%FPA, ESD_SPC(ISPC)%FPW, ESD_SPC(ISPC)%FPL, ESD_ANA(IANA)%KOW(IREL), &
              GASCON, ESD_LOC(ILOC)%TEMP, ESD_ANA(IANA)%HENRY, ESD_SPC(ISPC)%RHOP, EC(IREL)%VAPOR, &
              EC(IREL)%AIRC, KPA1, TMPCPAV
 1080       FORMAT(1P, &
              3X, E12.5,' : unitless             : ESD_SPC%FPA        : Volume fraction of plant species tissue is air'/&
              3X, E12.5,' : unitless             : ESD_SPC%FPW        : Volume fraction of plant tissue that is water'/&
              3X, E12.5,' : unitless             : ESD_SPC%FPL        : Volume fraction of plant tissue that is lipid'/&
              3X, E12.5,' : unitless             : ESD_ANA%KOW        : log base 10 of octanol-water partition coefficient'/&
              3X, E12.5,' : Pa-m^3/mol-K         : GASCON             : Universal gas constant'/&
              3X, E12.5,' : Centigrade           : ESD_LOC%TEMP       : Temperature'/&
              3X, E12.5,' : Pa-m^3/mol           : ESD_ANA%HENRY      : Henrys law constant'/&
              3X, E12.5,' : kg/m^3               : ESD_SPC%RHOP       : Plant tissue density'/&
              3X, E12.5,' : ug/m^3               : EC%VAPOR           : Vapor concentration in gas phase'/&
              3X, E12.5,' : ug/m^3               : EC%AIRC            : Air concentration'/&
              3X, E12.5,' : m^3/kg wet weight    : KPA1               : Plant-air partition coefficient to above-ground parts'/&
              3X, E12.5,' : ug/kg wet            : TMPCPAV            : Concentration in above-ground plant from vapor uptake')
          END IF
        ELSE
          TMPCPAV = 0.0
          IF( BGOSTP ) THEN
            WRITE(IRPT,1090) ESD_ANA(IANA)%HENRY, TMPCPAV
 1090       FORMAT(1P, &
              3X, E12.5,' : Pa-m^3/mol           : ESD_ANA%HENRY      : Henrys law constant'/&
              3X, E12.5,' : ug/kg wet            : TMPCPAV            : Concentration in above-ground plant from vapor uptake')
          END IF
        END IF
!
! ***   Calculate internal plant burden
!       Vapor uptake + Root uptake + Leaf absorption (dry) + Leaf absorption (wet)
        CPAI(ISPC,IREL) = TMPCPAV + TMPCPAU + (TMPCPAR+TMPCPAP+TMPCPAD)*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) + &
          TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL)
!
! ***   Calculate total plant burden (as consumed by predators)
!       Leaf surface (rain + particulates) + internal burden
        CPAT(ISPC,IREL) = TMPCPAV + TMPCPAU + TMPCPAR + TMPCPAP + TMPCPAD + TMPCPAW
!
! ***   Save the concentration for human consumption (wet weight basis)
        IF( ESD_SPC(ISPC)%INTERNAL ) THEN
          FWORK(IREL) = CPAI(ISPC,IREL)
        ELSE
          FWORK(IREL) = CPAT(ISPC,IREL)
        END IF
!
! ***   Save plant internal body burden
        ESD_SPC(ISPC)%BODYBURDEN(IREL) = CPAI(ISPC,IREL)
        CWORK(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL)
!
!       Optional environmental hazard quotient
        IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) &
          EHQ(IREL) = ESD_SPC(ISPC)%BODYBURDEN(IREL) / ESD_SPC(ISPC)%BMTISS(IANA,IREL)
!
        IF( BGOSTP ) THEN
          WRITE(IRPT,1100) ESD_SPC(ISPC)%KLIDRY(IANA,IREL), ESD_SPC(ISPC)%KLIWET(IANA,IREL), &
            CPAI(ISPC,IREL), CPAT(ISPC,IREL), ESD_SPC(ISPC)%BODYBURDEN(IREL), FWORK(IREL), &
            ESD_SPC(ISPC)%INTERNAL
 1100     FORMAT(1P, &
            3X, E12.5,' : unitless             : ESD_SPC%KLIDRY     : Leaf to internal plant transfer factor for dry deposition'/&
            3X, E12.5,' : unitless             : ESD_SPC%KLIWET     : Leaf to internal plant transfer factor for wet deposition'/&
            3X, E12.5,' : ug/kg wet            : CPAI               : Internal plant burden'/&
            3X, E12.5,' : ug/kg wet            : CPAT               : Total plant burden (as consumed by predators)'/&
            3X, E12.5,' : ug/kg wet            : ESD_SPC%BODYBURDEN : Plant body burden'/&
            3X, E12.5,' : ug/kg wet            : FWORK              : Concentration for human consumption'/&
            3X, L12  ,' : none                 : ESD_SPC%INTERNAL   : Flag for internal (true) or total concentration - human food')
!
          IF( (DET_EHQ.OR.STA_EHQ) .AND. ESD_SPC(ISPC)%OUTPUT ) WRITE(IRPT,1170) ESD_SPC(ISPC)%BMTISS(IANA,IREL), EHQ(IREL)
 1170     FORMAT(1P, &
            3X, E12.5,' : ug/kg wet            : ESD_SPC%BMTISS     : Tissue benchmark' /&
            3X, E12.5,' : unitless             : EHQ                : Environmental hazard quotient')
        END IF
!
        IF( BGOSTP ) THEN
!
          WRITE(IRPT,1220) '   Internal plant burden (ug/kg wet plant weight) for '//TRIM(ESD_ANA(IANA)%ID)//' on '//&
            TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from rainsplash'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from soil particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD*ESD_SPC(ISPC)%KLIDRY(IANA,IREL), PCT, 'Foliar adsorption from ambient air particulates'
          IF( CPAI(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL) / CPAI(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW*ESD_SPC(ISPC)%KLIWET(IANA,IREL), PCT, 'Foliar adsorption from wet deposition'
          WRITE(IRPT,1240) CPAI(ISPC,IREL), 'Total internal plant burden'
 1220     FORMAT(/A)
 1230     FORMAT(3X,1P,E12.5,0P,' : ',F8.4,'% : ',A)
 1240     FORMAT(3X,1P,E12.5,' : ',A)
!
          WRITE(IRPT,1220) '   Plant burden (ug/kg wet plant weight) as consumed by predators for '//&
            TRIM(ESD_ANA(IANA)%ID)//' on '//TRIM(ESD_SPC(ISPC)%ID)//' : '//TRIM(ESD_SPC(ISPC)%NAME)
          PCT = 0.0
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAV / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAV, PCT, 'Vapor-phase uptake'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAU / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAU, PCT, 'Root uptake from soil'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAR / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAR, PCT, 'Rainsplash to above ground plant parts'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAP / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAP, PCT, 'Foliar interception from soil particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAD / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAD, PCT, 'Foliar interception ambient air particulates'
          IF( CPAT(ISPC,IREL) .GT. 0.0 ) PCT = 100.0 * TMPCPAW / CPAT(ISPC,IREL)
          WRITE(IRPT,1230) TMPCPAW, PCT, 'Foliar interception from wet deposition'
          WRITE(IRPT,1240) CPAT(ISPC,IREL), 'Total plant burden'
!
        END IF
!
      END DO
!
!---------------------------------------------------------------------------------------------------
! After the computations are done
!---------------------------------------------------------------------------------------------------
!
! *** Optional detailed debug output written to the report file
      IF( BGOSTP .AND. NREAL.GT.1 ) THEN
        CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IANA,ESD_ANA(IANA)%ID,'BURDEN',UNITS_ORG_BUR_TER,NREAL,CWORK)
      END IF
!
! *** Skip all further outputs unless the 'OUTPUT' modifiers were invoked
      IF( .NOT.ESD_SPC(ISPC)%OUTPUT ) RETURN
!
! *** Optionally write out the detailed values
      IF( DET_BURDEN ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID, 'BURDEN', UNITS_ORG_BUR_TER, CWORK )
      IF( DET_EHQ ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME,ESD_LOC(ILOC)%ID,SOIL_ID,ESD_ANA(IANA)%ID, &
        ESD_SPC(ISPC)%ID,'EHQ',UNITS_NONE,EHQ )
!
! *** Optionally write the burden (as consumed) to the food file
      IF( FOODS .AND. ESD_ANA(IANA)%OUTPUT ) THEN
        NREC = -1
        IF( ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN' ) THEN
          CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' ) THEN
          CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lover level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
        IF( NREC .LT. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        CALL FCDA_WRITE( NREC, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, FWORK, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = ' '
          WRITE(MESSAG(2),'(A,I0)') 'ITIM=', ITIM
          MESSAG(3) = ' '
          WRITE(MESSAG(3),'(A,I0)') 'ILOC=', ILOC
          MESSAG(4) = ' '
          WRITE(MESSAG(4),'(A,I0)') 'ISPC=', ISPC
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
!
! *** Optional output of body burden statistics and threshold calculations
      IF( STA_BURDEN .OR. THRESHOLD%EXIST1(ISPC,IANA,1) .OR. THRESHOLD%EXIST2(ISPC,IANA,1) ) THEN
        EX1 = THRESHOLD%EXIST1(ISPC,IANA,1)
        EX2 = THRESHOLD%EXIST2(ISPC,IANA,1)
        TL1 = THRESHOLD%LIMIT1(ISPC,IANA,1)
        TL2 = THRESHOLD%LIMIT2(ISPC,IANA,1)
        CALL USTAT_ECEM( CWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          EX1, TL1, PR1, EX2, TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - BURDEN'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        IF( STA_BURDEN ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'BURDEN', UNITS_ORG_BUR_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
        IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
          ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, 'BURDEN', UNITS_ORG_BUR_TER, TL1, PR1, TL2, PR2 )
      END IF
!
! *** Environmental hazard quotient statistics
      IF( STA_EHQ ) THEN
        CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
          .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine - EHQ'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, ESD_ANA(IANA)%ID, &
          'EHQ', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
      END IF
!
      RETURN
      END SUBROUTINE OS_TP

      SUBROUTINE PRED_CHECK( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks for errors in the predation matrix.  If
!!    no errors are found, it returns the order in which the species
!!    information should be evaluated to maintain computability
!!    concerns.
!!
!!  Error Checking:
!!
!!    Individual consumption must be in the range 0 to 1
!!    Total consumption must be 0 for a plant
!!    Plants do not eat soil or sediment
!!    Total consumption must be 0 or 1 for an animal
!!    Cannibal species are not allowed
!!    Aquatic species cannot eat terrestrial species
!!    Upland species only eat upland species
!!    Riparian species cannot eat upland species
!!    Computations can be carried out in sequential order
!!
!!  History:
!!
!!    Paul W. Eslinger : 18 Dec 1997 : Version 1.0
!!    Paul W. Eslinger : 15 May 2002 : Only aquatic animals eat sediment
!!    Paul W. Eslinger :  3 Jun 2002 : Eliminate sediment for aquatic animals
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1 - revise extensively
!!    Paul W. Eslinger :  3 Jul 2003 : Add soil and sediment check for plants
!!    Paul W. Eslinger : 11 Jan 2005 : Add info message on skipped specied
!!    Paul W. Eslinger :  2 Feb 2005 : Change debug outputs
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Terminal error encountered
!!
!!  Sum Logic:
!!
!!    SMALL : If the row sum is within SMALL of 1, the row sum will be
!!            treated as if it were 1.
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Param_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Debug_Mod
      USE Files_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'PRED_CHECK' ! Name of this subroutine
!
      INTEGER :: ISPC  ! Species looping index
      INTEGER :: JSPC  ! Species looping index
      INTEGER :: NYES  ! Counter on possible computes
      INTEGER :: NLOOP ! Looping index counter
      REAL :: ROWSUM   ! Sum of entries in a row
!
      LOGICAL :: LTMP  ! Temporary logical variable
      REAL, SAVE :: SMALL = 1.0E-5 ! If the row sum is within SMALL of 1,
!                                    it will be treated as if it were 1
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
!----------------------------------------------------------------------------------
!     Check for species in the ESD library that are not being computed
!----------------------------------------------------------------------------------
!     This is not an error but it may help find errors
!
!     Count the skipped species
      JSPC = 0
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) JSPC = JSPC + 1
      END DO
!
!     Output information on the skipped species
      IF( JSPC .GT. 0 ) THEN
!
        MESSAG(1) = 'There were '
        WRITE(MESSAG(1)(12:),'(I0)') JSPC
        MESSAG(1) = TRIM(MESSAG(1)) // ' species in the ESD keyword file that were not computed'
        CALL PRTERR( IERR, CALLER, 1 )
!
        WRITE(IRPT,1010)
 1010   FORMAT(/ &
          '  ID    Long Name'/ &
          '------  ',48('-'))
        DO ISPC = 1, ESD_NUM_SPC
          IF( ESD_SPC(ISPC)%COMP ) CYCLE
          WRITE(IRPT,1070) ESD_SPC(ISPC)%ID, TRIM(ESD_SPC(ISPC)%NAME)
 1070     FORMAT(A,'  "',A,'"')
        END DO
!
      END IF
!
!----------------------------------------------------------------------------------
!     Check the individual entries (allowed values are between 0 and 1, inclusive)
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        DO JSPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).LT.0.0 .OR. PREDATE(ISPC,JSPC).GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Consumption for a species is invalid (valid range is [0,1]).'
            MESSAG(2) = 'Predator was: ' // ESD_SPC(ISPC)%ID
            MESSAG(3) = 'Victim was: ' // ESD_SPC(JSPC)%ID
            MESSAG(4) = 'Value was: '
            MESSAG(5) = 'Change the CONSUME keyword'
            WRITE(MESSAG(4)(12:21),'(1P,E10.3)') PREDATE(ISPC,JSPC)
            CALL PRTERR( IERR, CALLER, 5 )
          END IF
        END DO
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
!----------------------------------------------------------------------------------
!     Check the row sums (allowed values are 0 for plants and 0 or 1 for animals)
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, ESD_NUM_SPC
!
!       Skip species not computed in this run
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
!
        ROWSUM = 0.0
        DO JSPC = 1, ESD_NUM_SPC
!         Skip species not computed in this run
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          ROWSUM = ROWSUM + PREDATE(ISPC,JSPC)
        END DO
!
!       Check that plants do not eat soil or sediment
        IF( ESD_SPC(ISPC)%TYPE(2:2) .EQ. 'P' ) THEN
          IF( ESD_SPC(ISPC)%SEDING .GT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Sediment consumption for species: '// ESD_SPC(ISPC)%ID //' is not 0'
            MESSAG(2) = 'Species type is a plant'
            MESSAG(3) = 'The species must have a sediment consumption of 0'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            CYCLE
          END IF
          IF( ESD_SPC(ISPC)%SOILING .GT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Soil consumption for species: '// ESD_SPC(ISPC)%ID //' is not 0'
            MESSAG(2) = 'Species type is a plant'
            MESSAG(3) = 'The species must have a soil consumption of 0'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            CYCLE
          END IF
        END IF
!
!       Check that plants have a rowsum of 0
        IF( ESD_SPC(ISPC)%TYPE(2:2) .EQ. 'P' ) THEN
          IF( ROWSUM .EQ. 0.0 ) THEN
            CYCLE
          ELSE
            IERR = IERR + 1
            MESSAG(1) = 'Total consumption for species: '// ESD_SPC(ISPC)%ID //' is not 0'
            MESSAG(2) = 'Species type is a plant'
            MESSAG(3) = 'The species must have a total consumption of 0'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            CYCLE
          END IF
        END IF
!
!       Check that animals have a rowsum of 0 or 1
        IF( ESD_SPC(ISPC)%TYPE(2:2) .EQ. 'A' ) THEN
          IF( ABS(ROWSUM).LT.SMALL .OR. ABS(ROWSUM-1.0).LT.SMALL ) THEN
            IF( ABS(ROWSUM).LT.SMALL ) THEN
!             Sediment and soil ingestion requires food ingestion
              IF( ESD_SPC(ISPC)%TYPE.EQ.'TA' .AND. ESD_SPC(ISPC)%SOILING.GT.0.0 ) THEN
                IERR = IERR + 1
                MESSAG(1) = 'Total consumption for species: '// ESD_SPC(ISPC)%ID //' is 0'
                MESSAG(2) = 'Species type is a terrestrial animal'
                MESSAG(3) = 'The species must also have a soil consumption of 0'
                MESSAG(4) = 'Change the CONSUME keyword'
                CALL PRTERR( IERR, CALLER, 4 )
                CYCLE
              END IF
              IF( ESD_SPC(ISPC)%TYPE.EQ.'QA' .AND. ESD_SPC(ISPC)%SEDING.GT.0.0 ) THEN
                IERR = IERR + 1
                MESSAG(1) = 'Total consumption for species: '// ESD_SPC(ISPC)%ID //' is 0'
                MESSAG(2) = 'Species type is an aquatic animal'
                MESSAG(3) = 'The species must also have a sediment consumption of 0'
                MESSAG(4) = 'Change the CONSUME keyword'
                CALL PRTERR( IERR, CALLER, 4 )
                CYCLE
              END IF
            END IF
            CYCLE
          ELSE
            IERR = IERR + 1
            MESSAG(1) = 'Total consumption for species: '// ESD_SPC(ISPC)%ID //' is not 0 or 1'
            MESSAG(2) = 'Species type is an animal'
            MESSAG(3) = 'The species must have a total consumption of 0 or 1'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            CYCLE
          END IF
        END IF
!
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
!----------------------------------------------------------------------------------
!     Cannibal species are not allowed
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,ISPC) .NE. 0.0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'Cannibal species not allowed. Species: '//ESD_SPC(ISPC)%ID
          MESSAG(2) = 'Change the CONSUME keyword'
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
!----------------------------------------------------------------------------------
!     Check that aquatic species do not eat terrestrial species
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        DO JSPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. ESD_SPC(ISPC)%TYPE(1:1).EQ.'Q' &
            .AND. ESD_SPC(JSPC)%TYPE(1:1).EQ.'T' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species: ' // ESD_SPC(ISPC)%ID // ' cannot eat species: ' // ESD_SPC(JSPC)%ID
            MESSAG(2) = 'Aquatic species cannot eat terrestrial species'
            MESSAG(3) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END DO
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
!----------------------------------------------------------------------------------
!     Check that upland species only eat upland species
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        DO JSPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. ESD_SPC(ISPC)%HABITAT.EQ.'UPLAND' &
            .AND. ESD_SPC(JSPC)%HABITAT.NE.'UPLAND' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species: ' // ESD_SPC(ISPC)%ID // ' cannot eat species: ' // ESD_SPC(JSPC)%ID
            MESSAG(2) = 'Upland species can only eat upland species'
            MESSAG(3) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END DO
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
!----------------------------------------------------------------------------------
!     Check that riparian species do not eat upland species
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        DO JSPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. ESD_SPC(ISPC)%HABITAT.EQ.'RIPARIAN' &
            .AND. ESD_SPC(JSPC)%HABITAT.EQ.'UPLAND' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species: ' // ESD_SPC(ISPC)%ID // ' cannot eat species: ' // ESD_SPC(JSPC)%ID
            MESSAG(2) = 'Riparian species cannot eat upland species'
            MESSAG(3) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END DO
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
!----------------------------------------------------------------------------------
!     Check that body burdens or concentrations can be computed in sequence
!----------------------------------------------------------------------------------
!
      NYES = 0
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        ISPCTMP(ISPC) = 0
      END DO
!
! *** If a row sum is 0 the species can always be computed
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        ROWSUM = 0.0
        DO JSPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
          ROWSUM = ROWSUM + PREDATE(ISPC,JSPC)
        END DO
        IF( ROWSUM .EQ. 0.0 ) THEN
          NYES = NYES + 1
          ISPCTMP(ISPC) = NYES
        END IF
      END DO
!
! *** Use multiple passes to check that all food species are available
!
      DO NLOOP = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(NLOOP)%COMP ) CYCLE
        DO ISPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
          IF( ISPCTMP(ISPC) .GT. 0 ) CYCLE
          LTMP = .TRUE.
          DO JSPC = 1, ESD_NUM_SPC
            IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
            IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. ISPCTMP(JSPC).EQ.0 ) THEN
              LTMP = .FALSE.
            END IF
          END DO
          IF( LTMP ) THEN
            NYES = NYES + 1
            ISPCTMP(ISPC) = NYES
          END IF
        END DO
        IF( NYES .EQ. ESD_NUM_SPC ) EXIT
      END DO
!
! *** Another look that consumed species are being modeled
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        DO JSPC = 1, ESD_NUM_SPC
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
          IF( ESD_SPC(JSPC)%COMP ) CYCLE
          IERR = IERR + 1
          MESSAG(1) = 'Species: "' // TRIM(ESD_SPC(ISPC)%ID) // '" cannot be computed'
          MESSAG(2) = 'The food web requests consumption of species: "' // TRIM(ESD_SPC(JSPC)%ID) // '"'
          MESSAG(3) = 'The requested prey species is not being computed.'
          CALL PRTERR( IERR, CALLER, 3 )
        END DO
      END DO
!
! *** Final check on being able to complete the computations
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        IF( ISPCTMP(ISPC) .EQ. 0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'Species: ' // ESD_SPC(ISPC)%ID // ' cannot be computed'
          MESSAG(2) = 'The food web is inconsistent for sequential calculations'
          MESSAG(3) = 'Change the sequence of CONSUME keywords'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
!----------------------------------------------------------------------------------
!     Set the species computation order
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
        ESD_SPC( ISPCTMP(ISPC) )%ORDER = ISPC
      END DO
!
      RETURN
      END SUBROUTINE PRED_CHECK

      SUBROUTINE QA_INTAKE( ISPC )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the intake of food products in kg/day
!!    (dry weight basis) for a single aquatic animal species.
!!
!!  Call List Variables:
!!
!!    ISPC  : Species index
!!
!!  History:
!!
!!    Paul W. Eslinger : 24 Jan 2005 : Original Code
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ISPC
!
! *** Local variables
      INTEGER :: JSPC ! Species index
!
!---- Executable code --------------------------------------------
!
! *** Set food consumption to zero for all species
      DO JSPC = 1, ESD_NUM_SPC
        FOOD(JSPC) = 0.0
      END DO
!
! *** Calculate feeding rate (not dependent on realization) for the predator (wet weight)
      DO JSPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) THEN
          FOOD(JSPC) = PREDATE(ISPC,JSPC) * ESD_SPC(JSPC)%AWD * ESD_SPC(ISPC)%WBMASS * &
                       (ESD_SPC(ISPC)%GROWRATE+ESD_SPC(ISPC)%RESPRATE) * &
                       (ESD_SPC(ISPC)%FOC/ESD_SPC(JSPC)%FOC) / ESD_SPC(ISPC)%OCAR
        END IF
      END DO
!
      RETURN
      END SUBROUTINE QA_INTAKE

      SUBROUTINE RADENERGY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine sets the effective energies for the requested
!!    radionuclides analytes using the information in the radius library.
!!    The energy depends on the species radius and the radionuclide.
!!
!!  History:
!!
!!    Paul W. Eslinger :  9 Dec 1997 : Version 1.0
!!    Paul W. Eslinger : 18 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Radius_Mod
      USE Species_Mod
      USE Errors_Mod
      USE Debug_Mod
      USE Esd_Mod
      USE Files_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'RADENERGY' ! Name of this subroutine
      INTEGER :: IANA ! Analyte looping variable
      INTEGER :: ISPC ! Species looping variable
      INTEGER :: IDXR ! Temporary radionuclide index
      INTEGER :: IDXA ! Temporary analyte index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      IF( BGCOST ) THEN
        WRITE(IRPT,1000) CALLER
 1000   FORMAT(/'Entering: ',A)
      END IF
!
      IF( .NOT.USE_RADS ) RETURN ! Exit if radionuclides are not being computed
!
! *** Loop over all species
!
      DO ISPC = 1, ESD_NUM_SPC
!
        IF( .NOT.ESD_SPC(ISPC)%COMP ) CYCLE ! Skip species not computed
!
! ***   Find the radius index for this species (this applies to all analytes)
!
        CALL FIND_RADIUS_INDEX( ISPC, IDXR, IERR )
        IF( IDXR.EQ.0 .OR. IERR.NE.0 ) THEN
          IERR = 1
          MESSAG(1) = 'Radius index not found for species ID: ' // ESD_SPC(ISPC)%ID
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
! ***   Loop over all analytes
!
        DO IANA = 1, ESD_NUM_ANA
!
! ***     Skip analyes not computed
!
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
!
! ***     Skip nonradioactive analytes
!
          IF( ESD_ANA(IANA)%TYPE.EQ.'OS' .OR. ESD_ANA(IANA)%TYPE.EQ.'NS' ) CYCLE
!
! ***     Find the analyte in the radius library that matches this analyte
!
          CALL FIND_RADIUS_ANALYTE( ESD_ANA(IANA)%ID, IDXA )
          IF( IDXA .EQ. 0 ) THEN
            IERR = 2
            MESSAG(1) = 'Radius value not found for analye ID ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
! ***     Set the effective energy for this species and analyte
!
          ESD_SPC(ISPC)%EFFENG(IANA) = RLIBANA(IDXA)%RLIBVAL(IDXR)
!
          IF( BGCOST ) THEN
            WRITE(IRPT,1010) ESD_SPC(ISPC)%ID, ESD_SPC(ISPC)%RADIUS, ESD_ANA(IANA)%ID, &
              ESD_SPC(ISPC)%EFFENG(IANA), IDXA, IDXR
 1010       FORMAT('Species ID: ',A,' with radius ',1P,E12.5,' cm and Radionuclide ID: ',A, &
              ' had an effective energy of ',E12.5,' :: Radius Index=',I4,' Analyte Index=',I4)
          END IF
!
        END DO
!
      END DO
!
      RETURN
      END SUBROUTINE RADENERGY

      SUBROUTINE RADMEMORY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine allocates memory for the radionuclide
!!    radius library.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Jan 2005 : Original source
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Radius_Mod
      USE Errors_Mod
      USE Debug_Mod
      USE Esd_Mod
      USE Files_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'RADMEMORY' ! Name of this subroutine
      INTEGER :: IANA ! Analyte looping variable
      INTEGER :: IERA ! Temporary error variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      IF( BGLOOP ) THEN
        WRITE(IRPT,1000) CALLER
 1000   FORMAT(/'Entering: ',A)
      END IF
!
      IF( .NOT.USE_RADS ) RETURN ! Exit if radionuclides are not being computed
!
! *** RLIBANA: Top-level radionuclide radius library
!
      ALLOCATE( RLIBANA(NANA_RLIB+NHED_RLIB), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error allocating memory for structure RLIBANA'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Components of the radionuclide radius library
!
      DO IANA = 1, NANA_RLIB + NHED_RLIB
        ALLOCATE( RLIBANA(IANA)%RLIBVAL(NRAD_RLIB), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 14
          MESSAG(1) = 'Error allocating memory for RLIBVAL in structure RLIBANA'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE RADMEMORY

      SUBROUTINE RADSUM_STATS( ITIM, ILOC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates and outputs the summary statistics for
!!    the sum of all radionuclides for all species at a single location.
!!
!!    Different calculations are needed whether the location is
!!    aquatic, riparian, or upland.
!!
!!  Call List Variables:
!!
!!    ITIM  :
!!    ILOC  :
!!    IERR  : Output integer error flag.
!!             0 = Normal execution
!!            >0 = Terminal error encountered
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Mar 1998 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 11 May 2005 : Change so aquatic data are not
!!                                     output at riparian locations
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Add output debug units
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Modify output labels
!!                                     and add BMRDOS logic
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Results_Mod
      USE Threshold_Mod
      USE Media_Mod, ONLY: SOIL_ID, SOIL_IDX
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Input time index
      INTEGER, INTENT (IN) :: ILOC ! Input location index
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'RADSUM_STATS' ! Name of this subroutine
      CHARACTER(LEN= 6) :: TMPAID = '-Rads-'       ! Temporary label for output
      CHARACTER(LEN= 6) :: TMPBMR = 'BMRDOS'       ! Temporary label for output
!
      INTEGER :: IRAD = -1 ! Temporary rad indicator
      INTEGER :: ISPC      ! Species looping variable
      INTEGER :: IREL      ! Realization looping variable
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Exit if no radionuclides are being used
!
      IF( .NOT.USE_RADS ) RETURN
!
! *** Debug entry message
!
      IF( BGRADS ) THEN
        WRITE(IRPT,1000) CALLER, ITIM, ESD_TIM(ITIM)%TIME, ILOC, ESD_LOC(ILOC)%ID
 1000   FORMAT(/A,' Summary radionuclide statistics'/ &
          '   Time index, year  : ',I3,1X,I4/ &
          '   Location index, ID: ',I3,1X,A)
      END IF
!
!-------------------------------------------------------------------------------------------------
!     Aquatic locations
!-------------------------------------------------------------------------------------------------
!
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'AQUATIC' ) THEN
!
        SOIL_ID = 'NONE'
        SOIL_IDX = 0
!
! ***   Loop over all of the species
        DO ISPC = 1, ESD_NUM_SPC
!
! ***     Skip the species that were not computed
!         Skip the species where output is not requested
          IF( .NOT. ESD_SPC(ISPC)%COMP   ) CYCLE
          IF( .NOT. ESD_SPC(ISPC)%OUTPUT ) CYCLE
          IF( ESD_SPC(ISPC)%HABITAT .NE. 'AQUATIC' ) CYCLE
          IF( ESD_SPC(ISPC)%TYPE(1:1) .NE. 'Q' ) CYCLE
!
! ***     Pull the radionuclide sum for a single species into a local vector
          FORALL(IREL=1:NREAL) RDWORK(IREL) = RADSUM_AQ(ISPC,IREL)
!
! ***     Optional detailed output going to the report file
          IF( BGRADS ) &
            CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,'SUMRAD',UNITS_RAD_DOS_AQ,NREAL,RDWORK )
!
! ***     Optionally write out the detailed values
          IF( DET_SUMRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, &
            ESD_SPC(ISPC)%ID, 'SUMRAD', UNITS_RAD_DOS_AQ, RDWORK )
!
! ***     Optional radiation dose benchmark calculation
          IF( DET_BMRDOS .OR. STA_BMRDOS ) THEN
            IF( ESD_SPC(ISPC)%TYPE .EQ. 'QP' ) THEN
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / QP_BMR
              END DO
            ELSE
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / QA_BMR
              END DO
            END IF
          END IF
!
          IF( DET_BMRDOS ) THEN
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, ESD_SPC(ISPC)%ID, TMPBMR, UNITS_NONE, EHQ )
            IF( BGRADS ) &
              CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,TMPBMR,UNITS_NONE,NREAL,EHQ )
          END IF
!
! ***     Sum of radioactive dose to a species - and threshold information
          IF( STA_SUMRAD .OR. THRESHOLD%SREXIST1(ISPC) .OR. THRESHOLD%SREXIST2(ISPC) ) THEN
            EX1 = THRESHOLD%SREXIST1(ISPC)
            EX2 = THRESHOLD%SREXIST2(ISPC)
            TL1 = THRESHOLD%SRLIMIT1(ISPC)
            TL2 = THRESHOLD%SRLIMIT2(ISPC)
            CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              EX1, TL1, PR1, EX2, TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - SUMRAD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( STA_SUMRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'SUMRAD', UNITS_RAD_DOS_AQ, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
            IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
              ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, TMPAID, 'SUMRAD', UNITS_RAD_DOS_AQ, TL1, PR1, TL2, PR2 )
          END IF
!
! ***     Radiation dose benchmark (Optional)
          IF( STA_BMRDOS ) THEN
            TL1 = 0.0
            TL2 = 0.0
            CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - BMRDOS'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
          END IF
!
        END DO
!
      END IF
!
!-------------------------------------------------------------------------------------------------
!     Riparian locations
!-------------------------------------------------------------------------------------------------
!
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
!
! ***   Loop over all of the species
        DO ISPC = 1, ESD_NUM_SPC
!
! ***     Skip the species that were not computed
!         Skip the species where output is not requested
!         Skip the associated aquatic species where output is not requested
          IF( .NOT. ESD_SPC(ISPC)%COMP   ) CYCLE
          IF( .NOT. ESD_SPC(ISPC)%OUTPUT ) CYCLE
          IF( .NOT. ESD_SPC(ISPC)%HABITAT.EQ.'RIPARIAN' ) CYCLE
!
          SOIL_ID = 'SORP'
          SOIL_IDX = 0
!
! ***     Pull the radionuclide sum for a single species into a local vector
          FORALL(IREL=1:NREAL) RDWORK(IREL) = RADSUM_RP(ISPC,IREL)
!
! ***     Optional detailed output going to the report file
          IF( BGRADS ) &
            CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,'SUMRAD',UNITS_RAD_DOS_TER,NREAL,RDWORK )
!
! ***     Optionally write out the detailed values
          IF( DET_SUMRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, &
            ESD_SPC(ISPC)%ID, 'SUMRAD', UNITS_RAD_DOS_TER, RDWORK )
!
! ***     Optional radiation dose benchmark calculation
          IF( DET_BMRDOS .OR. STA_BMRDOS ) THEN
            IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TP_BMR
              END DO
            ELSE
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TA_BMR
              END DO
            END IF
          END IF
!
          IF( DET_BMRDOS ) THEN
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, ESD_SPC(ISPC)%ID, TMPBMR, UNITS_NONE, EHQ )
            IF( BGRADS ) &
              CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,TMPBMR,UNITS_NONE,NREAL,EHQ )
          END IF
!
! ***     Sum of radioactive dose to a species - and threshold information
          IF( STA_SUMRAD .OR. THRESHOLD%SREXIST1(ISPC) .OR. THRESHOLD%SREXIST2(ISPC) ) THEN
            EX1 = THRESHOLD%SREXIST1(ISPC)
            EX2 = THRESHOLD%SREXIST2(ISPC)
            TL1 = THRESHOLD%SRLIMIT1(ISPC)
            TL2 = THRESHOLD%SRLIMIT2(ISPC)
            CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              EX1, TL1, PR1, EX2, TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - SUMRAD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( STA_SUMRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'SUMRAD', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
            IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
              ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, TMPAID, 'SUMRAD', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
          END IF
!
! ***     Radiation dose benchmark (Optional)
          IF( STA_BMRDOS ) THEN
            TL1 = 0.0
            TL2 = 0.0
            CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - BMRDOS'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
          END IF
!
        END DO
!
      END IF
!
!-------------------------------------------------------------------------------------------------
!     Upland locations and dry soil type
!-------------------------------------------------------------------------------------------------
!
      IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SODR ) THEN
!
        SOIL_ID = 'SODR'
        SOIL_IDX = 1
!
! ***   Loop over all of the species
        DO ISPC = 1, ESD_NUM_SPC
!
! ***     Skip the species that were not computed
!         Skip the species where output is not requested
!         Skip the species that are not upland
!         Skip the species that are not terrestrial
          IF( .NOT. ESD_SPC(ISPC)%COMP   ) CYCLE
          IF( .NOT. ESD_SPC(ISPC)%OUTPUT ) CYCLE
          IF( ESD_SPC(ISPC)%HABITAT .NE. 'UPLAND' ) CYCLE
          IF( ESD_SPC(ISPC)%TYPE(1:1) .NE. 'T' ) CYCLE
!
! ***     Pull the radionuclide sum for a single species into a local vector
          FORALL(IREL=1:NREAL) RDWORK(IREL) = RADSUM_UP(ISPC,IREL,SOIL_IDX)
!
! ***     Optional detailed output going to the report file
          IF( BGRADS ) &
            CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,'SUMRAD',UNITS_RAD_DOS_TER,NREAL,RDWORK )
!
! ***     Optionally write out the detailed values
          IF( DET_SUMRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, &
            ESD_SPC(ISPC)%ID, 'SUMRAD', UNITS_RAD_DOS_TER, RDWORK )
!
! ***     Optional radiation dose benchmark calculation
          IF( DET_BMRDOS .OR. STA_BMRDOS ) THEN
            IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TP_BMR
              END DO
            ELSE
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TA_BMR
              END DO
            END IF
          END IF
!
          IF( DET_BMRDOS ) THEN
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, ESD_SPC(ISPC)%ID, TMPBMR, UNITS_NONE, EHQ )
            IF( BGRADS ) &
              CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,TMPBMR,UNITS_NONE,NREAL,EHQ )
          END IF
!
! ***     Sum of radioactive dose to a species - and threshold information
          IF( STA_SUMRAD .OR. THRESHOLD%SREXIST1(ISPC) .OR. THRESHOLD%SREXIST2(ISPC) ) THEN
            EX1 = THRESHOLD%SREXIST1(ISPC)
            EX2 = THRESHOLD%SREXIST2(ISPC)
            TL1 = THRESHOLD%SRLIMIT1(ISPC)
            TL2 = THRESHOLD%SRLIMIT2(ISPC)
            CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              EX1, TL1, PR1, EX2, TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - SUMRAD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( STA_SUMRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'SUMRAD', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
            IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
              ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, TMPAID, 'SUMRAD', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
          END IF
!
! ***     Radiation dose benchmark (Optional)
          IF( STA_BMRDOS ) THEN
            TL1 = 0.0
            TL2 = 0.0
            CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - BMRDOS'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
          END IF
!
        END DO
!
      END IF
!
!-------------------------------------------------------------------------------------------------
!     Upland locations and groundwater irrigated soil type
!-------------------------------------------------------------------------------------------------
!
      IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SOGW ) THEN
!
        SOIL_ID = 'SOGW'
        SOIL_IDX = 2
!
! ***   Loop over all of the species
        DO ISPC = 1, ESD_NUM_SPC
!
! ***     Skip the species that were not computed
!         Skip the species where output is not requested
!         Skip the species that are not upland
!         Skip the species that are not terrestrial
          IF( .NOT. ESD_SPC(ISPC)%COMP   ) CYCLE
          IF( .NOT. ESD_SPC(ISPC)%OUTPUT ) CYCLE
          IF( ESD_SPC(ISPC)%HABITAT .NE. 'UPLAND' ) CYCLE
          IF( ESD_SPC(ISPC)%TYPE(1:1) .NE. 'T' ) CYCLE
!
! ***     Pull the radionuclide sum for a single species into a local vector
          FORALL(IREL=1:NREAL) RDWORK(IREL) = RADSUM_UP(ISPC,IREL,SOIL_IDX)
!
! ***     Optional detailed output going to the report file
          IF( BGRADS ) &
            CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,'SUMRAD',UNITS_RAD_DOS_TER,NREAL,RDWORK )
!
! ***     Optionally write out the detailed values
          IF( DET_SUMRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, &
            ESD_SPC(ISPC)%ID, 'SUMRAD', UNITS_RAD_DOS_TER, RDWORK )
!
! ***     Optional radiation dose benchmark calculation
          IF( DET_BMRDOS .OR. STA_BMRDOS ) THEN
            IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TP_BMR
              END DO
            ELSE
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TA_BMR
              END DO
            END IF
          END IF
!
          IF( DET_BMRDOS ) THEN
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, ESD_SPC(ISPC)%ID, TMPBMR, UNITS_NONE, EHQ )
            IF( BGRADS ) &
              CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,TMPBMR,UNITS_NONE,NREAL,EHQ )
          END IF
!
! ***     Sum of radioactive dose to a species - and threshold information
          IF( STA_SUMRAD .OR. THRESHOLD%SREXIST1(ISPC) .OR. THRESHOLD%SREXIST2(ISPC) ) THEN
            EX1 = THRESHOLD%SREXIST1(ISPC)
            EX2 = THRESHOLD%SREXIST2(ISPC)
            TL1 = THRESHOLD%SRLIMIT1(ISPC)
            TL2 = THRESHOLD%SRLIMIT2(ISPC)
            CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              EX1, TL1, PR1, EX2, TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - SUMRAD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( STA_SUMRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'SUMRAD', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
            IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
              ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, TMPAID, 'SUMRAD', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
          END IF
!
! ***     Radiation dose benchmark (Optional)
          IF( STA_BMRDOS ) THEN
            TL1 = 0.0
            TL2 = 0.0
            CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - BMRDOS'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
          END IF
!
        END DO
!
      END IF
!
!-------------------------------------------------------------------------------------------------
!     Upland locations and surface water irrigated soil type
!-------------------------------------------------------------------------------------------------
!
      IF( ESD_LOC(ILOC)%TYPE.EQ.'UPLAND' .AND. ESD_LOC(ILOC)%SOSW ) THEN
!
        SOIL_ID = 'SOSW'
        SOIL_IDX = 3
!
! ***   Loop over all of the species
        DO ISPC = 1, ESD_NUM_SPC
!
! ***     Skip the species that were not computed
!         Skip the species where output is not requested
!         Skip the species that are not upland
!         Skip the species that are not terrestrial
          IF( .NOT. ESD_SPC(ISPC)%COMP   ) CYCLE
          IF( .NOT. ESD_SPC(ISPC)%OUTPUT ) CYCLE
          IF( ESD_SPC(ISPC)%HABITAT .NE. 'UPLAND' ) CYCLE
          IF( ESD_SPC(ISPC)%TYPE(1:1) .NE. 'T' ) CYCLE
!
! ***     Pull the radionuclide sum for a single species into a local vector
          FORALL(IREL=1:NREAL) RDWORK(IREL) = RADSUM_UP(ISPC,IREL,SOIL_IDX)
!
! ***     Optional detailed output going to the report file
          IF( BGRADS ) &
            CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,'SUMRAD',UNITS_RAD_DOS_TER,NREAL,RDWORK )
!
! ***     Optionally write out the detailed values
          IF( DET_SUMRAD ) CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, &
            ESD_SPC(ISPC)%ID, 'SUMRAD', UNITS_RAD_DOS_TER, RDWORK )
!
! ***     Optional radiation dose benchmark calculation
          IF( DET_BMRDOS .OR. STA_BMRDOS ) THEN
            IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TP_BMR
              END DO
            ELSE
              DO IREL = 1, NREAL
                EHQ(IREL) = RDWORK(IREL) / TA_BMR
              END DO
            END IF
          END IF
!
          IF( DET_BMRDOS ) THEN
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, TMPAID, ESD_SPC(ISPC)%ID, TMPBMR, UNITS_NONE, EHQ )
            IF( BGRADS ) &
              CALL WRITE_BUGS(ILOC,ESD_LOC(ILOC)%ID,ISPC,ESD_SPC(ISPC)%ID,IRAD,TMPAID,TMPBMR,UNITS_NONE,NREAL,EHQ )
          END IF
!
! ***     Sum of radioactive dose to a species - and threshold information
          IF( STA_SUMRAD .OR. THRESHOLD%SREXIST1(ISPC) .OR. THRESHOLD%SREXIST2(ISPC) ) THEN
            EX1 = THRESHOLD%SREXIST1(ISPC)
            EX2 = THRESHOLD%SREXIST2(ISPC)
            TL1 = THRESHOLD%SRLIMIT1(ISPC)
            TL2 = THRESHOLD%SRLIMIT2(ISPC)
            CALL USTAT_ECEM( RDWORK, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              EX1, TL1, PR1, EX2, TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - SUMRAD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( STA_SUMRAD ) CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'SUMRAD', UNITS_RAD_DOS_TER, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
            IF( EX1 .OR. EX2 ) CALL WRITE_THRESH( ESD_TIM(ITIM)%TIME, &
              ESD_LOC(ILOC)%ID, ESD_SPC(ISPC)%ID, TMPAID, 'SUMRAD', UNITS_RAD_DOS_TER, TL1, PR1, TL2, PR2 )
          END IF
!
! ***     Radiation dose benchmark (Optional)
          IF( STA_BMRDOS ) THEN
            TL1 = 0.0
            TL2 = 0.0
            CALL USTAT_ECEM( EHQ, NREAL, TWORK, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, &
              .FALSE., TL1, PR1, .FALSE., TL2, PR2, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - BMRDOS'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, SOIL_ID, ESD_SPC(ISPC)%ID, TMPAID, &
              'BMRDOS', UNITS_NONE, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
          END IF
!
        END DO
!
      END IF
!
      RETURN
      END SUBROUTINE RADSUM_STATS

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
      END SUBROUTINE READ_KEYS_REPORT

      SUBROUTINE REPORT_ANAL( )
!!*************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the analyte definitions
!!    to the report file for debug purposes.
!!
!!  History:
!!
!!    Paul W. Eslinger : 15 Aug 2007 : Version 1.0
!!
!!*************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Debug_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: IREL ! realization looping index
      INTEGER :: IDX  ! Index variable
!
!---- First executable code --------------------------------------------
!
! *** Analyte Definitions
!
      WRITE(IRPT,1010)
 1010 FORMAT(/'!',117('-')/'!',40X,'Additional analyte information'/'!',117('-'))
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
        WRITE(IRPT,1020) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%ELEM_ID, TRIM(ESD_ANA(IANA)%NAME)
 1020   FORMAT('!'/'!',117('-')/&
          'Index:       ',I0/&
          'ID:          ',A/&
          'Element:     ',A/&
          'Description: ','"',A,'"')
!
 1030   FORMAT(A)
 1040   FORMAT(A,1X,1P,E13.6,1X,A,1X,A)
 1050   FORMAT(5X,0P,I4,1P,1X,E12.5)
 1060   FORMAT(&
          'Variable                   Value      Units              Description'/&
          '----------------------  ------------- ------------------ -------------------------------------------------------------')
!
        SELECT CASE( ESD_ANA(IANA)%TYPE )
!
          CASE('OR')
            WRITE(IRPT,1030) 'Type:        OR - Organic, radioactive elements'
            WRITE(IRPT,1060)
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%HENRY    ', ESD_ANA(IANA)%HENRY,    'Pa-m^3/mole       ', "Henry's law coefficient"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%MOLDIFF  ', ESD_ANA(IANA)%MOLDIFF,  'cm^2/sec          ', "Molecular diffusivity"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%PARTICLE ', ESD_ANA(IANA)%PARTICLE, 'none              ', &
              "Fraction of air far-field concentration that is particulate"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%DFIMM    ', ESD_ANA(IANA)%DFIMM,    'mrad m^3/uCi/year ', "Water immersion dose factor"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%DFSED    ', ESD_ANA(IANA)%DFSED,    'Sv m^3/sec Bq     ', "Sediment dose factor"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%GAMMA    ', ESD_ANA(IANA)%GAMMA,    'MeV/disintigration', "Gamma energy"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%KOW(IREL)', ESD_ANA(IANA)%KOW(1),   'none              ', &
              "Log of the Octonal-water partition coefficient by realization"
            IF( NREAL .GT. 1 ) THEN
              DO IREL = 1, NREAL
                WRITE(IRPT,1050) IREL, ESD_ANA(IANA)%KOW(IREL)
              END DO
            END IF
!
          CASE('OS')
            WRITE(IRPT,1030) 'Type:        OS - Organic, stable elements'
            WRITE(IRPT,1060)
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%HENRY    ', ESD_ANA(IANA)%HENRY,    'Pa-m^3/mole       ', "Henry's law coefficient"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%MOLDIFF  ', ESD_ANA(IANA)%MOLDIFF,  'cm^2/sec          ', "Molecular diffusivity"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%PARTICLE ', ESD_ANA(IANA)%PARTICLE, 'none              ', &
              "Fraction of air far-field concentration that is particulate"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%KOW(IREL)', ESD_ANA(IANA)%KOW(1),   'none              ', &
              "Log of the Octonal-water partition coefficient by realization"
            IF( NREAL .GT. 1 ) THEN
              DO IREL = 1, NREAL
                WRITE(IRPT,1050) IREL, ESD_ANA(IANA)%KOW(IREL)
              END DO
            END IF
!
          CASE('NR')
            WRITE(IRPT,1030) 'Type:        NR - Inorganic, radioactive elements'
            WRITE(IRPT,1060)
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%HENRY    ', ESD_ANA(IANA)%HENRY,    'Pa-m^3/mole       ', "Henry's law coefficient"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%MOLDIFF  ', ESD_ANA(IANA)%MOLDIFF,  'cm^2/sec          ', "Molecular diffusivity"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%PARTICLE ', ESD_ANA(IANA)%PARTICLE, 'none              ', &
              "Fraction of air far-field concentration that is particulate"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%DFIMM    ', ESD_ANA(IANA)%DFIMM,    'mrad m^3/uCi/year ', "Water immersion dose factor"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%DFSED    ', ESD_ANA(IANA)%DFSED,    'Sv m^3/sec Bq     ', "Sediment dose factor"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%GAMMA    ', ESD_ANA(IANA)%GAMMA,    'MeV/disintigration', "Gamma energy"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%KOW(IREL)', ESD_ANA(IANA)%KOW(1),   'none              ', &
              "Log of the Octonal-water partition coefficient by realization"
            IF( NREAL .GT. 1 ) THEN
              DO IREL = 1, NREAL
                WRITE(IRPT,1050) IREL, ESD_ANA(IANA)%KOW(IREL)
              END DO
            END IF
!
          CASE('NS')
            WRITE(IRPT,1030) 'Type:        NS - Inorganic, stable elements'
            WRITE(IRPT,1060)
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%HENRY    ', ESD_ANA(IANA)%HENRY,    'Pa-m^3/mole       ', "Henry's law coefficient"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%MOLDIFF  ', ESD_ANA(IANA)%MOLDIFF,  'cm^2/sec          ', "Molecular diffusivity"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%PARTICLE ', ESD_ANA(IANA)%PARTICLE, 'none              ', &
              "Fraction of air far-field concentration that is particulate"
            WRITE(IRPT,1040) 'ESD_ANA(IANA)%KOW(IREL)', ESD_ANA(IANA)%KOW(1),   'none              ', &
              "Log of the Octonal-water partition coefficient by realization"
            IF( NREAL .GT. 1 ) THEN
              DO IREL = 1, NREAL
                WRITE(IRPT,1050) IREL, ESD_ANA(IANA)%KOW(IREL)
              END DO
            END IF
!
          CASE DEFAULT
! ***       Other types
!
        END SELECT
!
      END DO
!
      RETURN
      END SUBROUTINE REPORT_ANAL

      SUBROUTINE REPORT_SPECIES( ISPC )
!!*************************************************************************************************
!!
!!  Purpose:
!!    This subroutine handles writing an echo of the definitions of a
!!    species to the report file.
!!
!!  History:
!!    Paul W. Eslinger :  3 Feb 2005 : Original source
!!    Paul W. Eslinger : 30 Jun 2005 : Add soil and sediment information
!!    Paul W. Eslinger :  1 Sep 2005 : Add AE and GE to aquatic species
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Revise output format, add GROWRATE, RESPRATE
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Minor output format change
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger : 15 Aug 2007 : Make RADIUS output depend on using radionuclides
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!*************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Debug_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ISPC ! Species index for reporting
!
! *** Local variables
      INTEGER :: JSPC           ! Species looping index
      CHARACTER(LEN=20) :: CTMP ! Temporary output variable
      REAL :: SEDVAL, SOILVAL   ! Temporary sediment and soil values
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1070)
 1070 FORMAT('!'/'!',100('-'))
!
      SELECT CASE( ESD_SPC(ISPC)%TYPE )
!
        CASE('QA') !-------------------------------------------------------------------------------------------------------
          CALL QA_INTAKE( ISPC )
          CTMP = 'Aquatic Animal'
          WRITE(IRPT,1000) TRIM(CTMP), TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME), TRIM(ESD_SPC(ISPC)%HABITAT)
 1000     FORMAT(A,' species "',A,'" : "',A,'"'/'Habitat : ',A)
!
!         Deterministic parameter definitions
          WRITE(IRPT,1050)
 1050     FORMAT('Deterministic parameter definitions')
!
!         Consumption for aquatic animals
          WRITE(IRPT,1010)
 1010     FORMAT('Consumption information')
          SEDVAL = 0.0
          DO JSPC = 1, ESD_NUM_SPC
            IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) THEN
              WRITE(IRPT,1020) PREDATE(ISPC,JSPC), JSPC, ESD_SPC(JSPC)%ID, FOOD(JSPC), TRIM(ESD_SPC(JSPC)%NAME)
 1020         FORMAT(2X,F8.5,' of species ',I3,' : "',A,'" : intake (g wet) ',1P,E11.4,' : "',A,'"')
              SEDVAL = SEDVAL + FOOD(JSPC)
            END IF
          END DO
          IF( ESD_SPC(ISPC)%SEDING .GT. 0.0 ) THEN
            SEDVAL = SEDVAL * ESD_SPC(ISPC)%SEDING
            WRITE(IRPT,1030) ESD_SPC(ISPC)%SEDING,  ' of "sediment"             : intake (g wet) ', SEDVAL
 1030       FORMAT(2X,F8.5,A,1P,E11.4)
          END IF
!
!         Deterministic parameter definitions
          WRITE(IRPT,1050)
          WRITE(IRPT,1060) 'AWD',      ESD_SPC(ISPC)%AWD,      'wet-to-dry weight ratio   ', 'g wet/g dry'
          WRITE(IRPT,1060) 'FLIPID',   ESD_SPC(ISPC)%FLIPID,   'fraction lipid            ', 'g lipid/g wet wt'
          WRITE(IRPT,1060) 'FOC',      ESD_SPC(ISPC)%FOC,      'fraction organic carbon   ', 'g org carb/g dry wt'
          WRITE(IRPT,1060) 'OCAR',     ESD_SPC(ISPC)%OCAR,     'organic carbon assim. rate', 'g organic carbon assim/g ingested'
          IF( USE_RADS ) WRITE(IRPT,1060) 'RADIUS',   ESD_SPC(ISPC)%RADIUS,   'radius                    ', 'cm'
          WRITE(IRPT,1060) 'WBMASS',   ESD_SPC(ISPC)%WBMASS,   'wet body mass             ', 'grams'
          WRITE(IRPT,1060) 'AE',       ESD_SPC(ISPC)%AE,       'assimilation efficiency   ', 'unitless'
          WRITE(IRPT,1060) 'GE',       ESD_SPC(ISPC)%GE,       'gross energy              ', 'kcal/kg wet weight'
          WRITE(IRPT,1060) 'GROWRATE', ESD_SPC(ISPC)%GROWRATE, 'growth rate               ', '1/day'
          WRITE(IRPT,1060) 'RESPRATE', ESD_SPC(ISPC)%RESPRATE, 'respiration rate          ', '1/day'
          IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
            WRITE(IRPT,1060) 'INHRATE', ESD_SPC(ISPC)%INHRATE, 'inhalation rate           ', 'm^3/day'
          END IF
!
 1060     FORMAT(A8,' : ',1P,E12.5,' : "',A,'" : Units = "',A,'"')
!
        CASE('QP') !-------------------------------------------------------------------------------------------------------
          CTMP = 'Aquatic Plant'
          WRITE(IRPT,1000) TRIM(CTMP), TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME), TRIM(ESD_SPC(ISPC)%HABITAT)
!
!         Deterministic parameter definitions
          WRITE(IRPT,1050)
          WRITE(IRPT,1060) 'AWD',      ESD_SPC(ISPC)%AWD,      'wet-to-dry weight ratio   ', 'g wet/g dry'
          WRITE(IRPT,1060) 'FLIPID',   ESD_SPC(ISPC)%FLIPID,   'fraction lipid            ', 'g lipid/g wet wt'
          WRITE(IRPT,1060) 'FOC',      ESD_SPC(ISPC)%FOC,      'fraction organic carbon   ', 'g org carb/g dry wt'
          IF( USE_RADS ) WRITE(IRPT,1060) 'RADIUS',   ESD_SPC(ISPC)%RADIUS,   'radius                    ', 'cm'
          WRITE(IRPT,1060) 'WBMASS',   ESD_SPC(ISPC)%WBMASS,   'wet body mass             ', 'grams'
          WRITE(IRPT,1060) 'AE',       ESD_SPC(ISPC)%AE,       'assimilation efficiency   ', 'unitless'
          WRITE(IRPT,1060) 'GE',       ESD_SPC(ISPC)%GE,       'gross energy              ', 'kcal/kg wet weight'
          WRITE(IRPT,1060) 'GROWRATE', ESD_SPC(ISPC)%GROWRATE, 'growth rate               ', '1/day'
          WRITE(IRPT,1060) 'RESPRATE', ESD_SPC(ISPC)%RESPRATE, 'Oxygen use rate           ', '1/day'
!
        CASE('TA') !-------------------------------------------------------------------------------------------------------
          CALL TA_INTAKE( ISPC )
          CTMP = 'Terrestrial Animal'
          WRITE(IRPT,1000) TRIM(CTMP), TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME), TRIM(ESD_SPC(ISPC)%HABITAT)
!
!         Consumption for terrestrial animals
          WRITE(IRPT,1010)
          SOILVAL = 0.0
          DO JSPC = 1, ESD_NUM_SPC
            IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) THEN
              WRITE(IRPT,1040) PREDATE(ISPC,JSPC), JSPC, ESD_SPC(JSPC)%ID, FOOD(JSPC), TRIM(ESD_SPC(JSPC)%NAME)
 1040         FORMAT(2X,F8.5,' of species ',I3,' : "',A,'" : intake (kg wet) ',1P,E11.4,' : "',A,'"')
              SOILVAL = SOILVAL + FOOD(JSPC)
            END IF
          END DO
          IF( ESD_SPC(ISPC)%SOILING .GT. 0.0 ) THEN
            SOILVAL = SOILVAL * ESD_SPC(ISPC)%SOILING
            WRITE(IRPT,1030) ESD_SPC(ISPC)%SOILING, ' of "soil"                 : intake (kg wet) ', SOILVAL
          END IF
!
!         Deterministic parameter definitions
          WRITE(IRPT,1050)
          WRITE(IRPT,1060) 'AE',       ESD_SPC(ISPC)%AE,       'assimilation efficiency            ', 'unitless'
          WRITE(IRPT,1060) 'DIFFHT',   ESD_SPC(ISPC)%DIFFHT,   'diffusion height                   ', 'm'
          WRITE(IRPT,1060) 'ETWATER',  ESD_SPC(ISPC)%ETWATER,  'exposure time to water             ', 'hr/day'
          WRITE(IRPT,1060) 'FMR',      ESD_SPC(ISPC)%FMR,      'metabolic rate of predator         ', 'kcal/day'
          WRITE(IRPT,1060) 'FWATER',   ESD_SPC(ISPC)%FWATER,   'frac exposure to water             ', 'unitless'
          WRITE(IRPT,1060) 'FDW',      ESD_SPC(ISPC)%FDW,      'conversion dry to wet weight       ', 'kg dry/kg wet'
          WRITE(IRPT,1060) 'GE',       ESD_SPC(ISPC)%GE,       'gross energy                       ', 'kcal/kg wet weight'
          WRITE(IRPT,1060) 'INHRATE',  ESD_SPC(ISPC)%INHRATE,  'resting inhalation rate            ', 'm^3/day'
          WRITE(IRPT,1060) 'PCS',      ESD_SPC(ISPC)%PCS,      'fraction of SA in contact w/soil   ', '1/day'
          WRITE(IRPT,1060) 'PCW',      ESD_SPC(ISPC)%PCW,      'fraction of SA available to water  ', 'unitless'
          WRITE(IRPT,1060) 'PSI',      ESD_SPC(ISPC)%PSI,      'seasonality factor                 ', 'unitless'
          IF( USE_RADS ) WRITE(IRPT,1060) 'RADIUS',   ESD_SPC(ISPC)%RADIUS,   'radius                             ', 'cm'
          WRITE(IRPT,1060) 'SA',       ESD_SPC(ISPC)%SA,       'surface area                       ', 'cm^2'
          WRITE(IRPT,1060) 'SADHER',   ESD_SPC(ISPC)%SADHER,   'skin adherance factor              ', 'mg/cm^2'
          WRITE(IRPT,1060) 'THETA',    ESD_SPC(ISPC)%THETA,    'area use factor                    ', 'unitless'
          WRITE(IRPT,1060) 'WATERING', ESD_SPC(ISPC)%WATERING, 'water ingestion rate               ', 'L/day'
          WRITE(IRPT,1060) 'WEIGHT',   ESD_SPC(ISPC)%WEIGHT,   'body weight                        ', 'kg wet'
!
        CASE('TP') !-------------------------------------------------------------------------------------------------------
          CTMP = 'Terrestrial Plant'
          WRITE(IRPT,1000) TRIM(CTMP), TRIM(ESD_SPC(ISPC)%ID), TRIM(ESD_SPC(ISPC)%NAME), TRIM(ESD_SPC(ISPC)%HABITAT)
!
!         Deterministic parameter definitions
          WRITE(IRPT,1050)
          WRITE(IRPT,1060) 'AE',      ESD_SPC(ISPC)%AE,      'assimilation efficiency              ', 'unitless'
          WRITE(IRPT,1060) 'GE',      ESD_SPC(ISPC)%GE,      'gross energy                         ', 'kcal/kg wet weight'
          WRITE(IRPT,1060) 'DIFFHT',  ESD_SPC(ISPC)%DIFFHT,  'diffusion height                     ', 'm'
          WRITE(IRPT,1060) 'FPA',     ESD_SPC(ISPC)%FPA,     'volume fraction of plant tissue air  ', 'unitless'
          WRITE(IRPT,1060) 'FPL',     ESD_SPC(ISPC)%FPL,     'volume fraction of plant tissue lipid', 'unitless'
          WRITE(IRPT,1060) 'FPW',     ESD_SPC(ISPC)%FPW,     'volume fraction of plant tissue water', 'unitless'
          WRITE(IRPT,1060) 'FW',      ESD_SPC(ISPC)%FW,      'water weight fraction of plant tissue', 'unitless'
          WRITE(IRPT,1060) 'FWATER',  ESD_SPC(ISPC)%FWATER,  'fraction exposure to water           ', 'unitless'
          IF( USE_RADS ) WRITE(IRPT,1060) 'RADIUS',  ESD_SPC(ISPC)%RADIUS,  'radius                               ', 'cm'
          WRITE(IRPT,1060) 'RHOP',    ESD_SPC(ISPC)%RHOP,    'plant tissue density                 ', 'kg/m^3'
          WRITE(IRPT,1060) 'ETWATER', ESD_SPC(ISPC)%ETWATER, 'exposure time to water               ', 'hr/day'
!
        CASE DEFAULT
! ***     Other types are not defined
!
      END SELECT
!
      RETURN
      END SUBROUTINE REPORT_SPECIES

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
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Air Deposit. ', CONVERT_UNITS(IANA,IAIRD), &
            CONVERT_LABELS(IANA,IAIRD,1), CONVERT_LABELS(IANA,IAIRD,2)
 1010     FORMAT(3X,I2,' : ',A,' : ',A,' : ',A,' : ',1P,E12.5,' : From ',A,' to ',A)
        END IF
      END DO
!
      RETURN
      END SUBROUTINE REPORT_UNITS

      SUBROUTINE RIPARIAN_SPECIES( ITIM, ILOC, IANA, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules impact calculations for all species at
!!    river or riparian locations.
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 13 Jan 2005 : Fix logic error on location indices
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
!!
!!**************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Debug_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Media_Mod
      USE Results_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR              ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=16) :: CALLER = 'RIPARIAN_SPECIES' ! Name of this subroutine
!
      CHARACTER(LEN=5) :: CTMP ! Selection variable for impacts routine
      INTEGER :: IREL ! Realization index
      INTEGER :: ISPC ! Species index
      INTEGER :: IDX  ! Species loop indexing variable
      INTEGER :: SLOC ! Secondary location index (for associated aquatic location)
      LOGICAL :: USE_SLOC ! Logical whether a secondary aquatic location is used
      LOGICAL :: SKIP_OUTPUTS ! Logical whether skip all data outputs in aquatic subroutines
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      IF( BGLOOP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID
 1000   FORMAT(/'In subroutine: ',A,' : Time = ',I0,' : Location = ',A,' : Analyte = ',A)
      END IF
!
! *** Set the water and soil concentrations for terrestrial species
!
      FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = EC(IREL)%SEEP
      FORALL(IREL=1:ESD_NREAL) EC(IREL)%SOIL  = EC(IREL)%SORP
!
! *** Initialize the entire food web for this location
!
      CING = 0.0
      CINH = 0.0
      CDER = 0.0
!
! *** Determine whether this riparian location uses a secondary aquatic location
!
      IF( ESD_LOC(ILOC)%SECOND .GT. 0 ) THEN
        SLOC = ESD_LOC(ILOC)%SECOND
        USE_SLOC = .TRUE.
      ELSE
        SLOC = 0
        USE_SLOC = .FALSE.
      END IF
      SKIP_OUTPUTS = .TRUE.
!
! *** Compute vapor concentrations from surface water concentrations if needed
      IF( NEED_VAPOR .AND. USE_SLOC ) THEN
        CALL COMPUTE_VAPOR_WATER( SLOC, IANA, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
! *** Loop over all species
      SPECIES_LOOP: DO IDX = 1, ESD_NUM_SPC
!
! ***   Compute the species in the permuted order
!       Skip the species not used in this scenario
!       Skip upland species
        ISPC = ESD_SPC(IDX)%ORDER
        IF( ISPC .LT. 0 ) CYCLE
        IF( .NOT.(ESD_SPC(ISPC)%HABITAT.EQ.'AQUATIC' .OR. ESD_SPC(ISPC)%HABITAT.EQ.'RIPARIAN') ) CYCLE
!
        IF( BGLOOP ) WRITE(IRPT,1010) IDX, ISPC, ESD_SPC(ISPC)%ID
 1010   FORMAT(' Processing Species: Loop Index = ',I0,' Species index = ',I0,' Species ID = ',A)
!
!       Set the soil type for labeling outputs
        IF( ESD_SPC(ISPC)%HABITAT .EQ. 'RIPARIAN' ) THEN
          SOIL_ID = 'SORP'
          SOIL_IDX = 0
        END IF
        IF( ESD_SPC(ISPC)%HABITAT .EQ. 'AQUATIC' ) THEN
          SOIL_ID = 'NONE'
          SOIL_IDX = 0
        END IF
!
        CTMP = ESD_ANA(IANA)%TYPE // '_' // ESD_SPC(ISPC)%TYPE
!        IF( BGLOOP ) WRITE(IRPT,'(A)') 'CTMP = '//CTMP
        SELECT CASE( CTMP )
!
          CASE('OR_QP')
! ***       Equations for ORGANIC RADIOACTIVE analytes and AQUATIC PLANTS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OR_QP'
              CALL OR_QP( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              DO IREL = 1, NREAL
                RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
              END DO
            END IF
!
          CASE('OR_TP')
! ***       Equations for ORGANIC RADIOACTIVE analytes and TERRESTRIAL PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OR_TP'
            CALL OR_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('OR_QA')
! ***       Equations for ORGANIC RADIOACTIVE analytes and AQUATIC ANIMALS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OR_QA'
              CALL OR_QA( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              DO IREL = 1, NREAL
                RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
              END DO
            END IF
!
          CASE('OR_TA')
! ***       Equations for ORGANIC RADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OR_TA'
            CALL OR_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('OS_QP')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and AQUATIC PLANTS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_QP'
              CALL OS_QP( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
            END IF
!
          CASE('OS_TP')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and TERRESTRIAL PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_TP'
            CALL OS_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('OS_QA')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and AQUATIC ANIMALS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_QA'
              CALL OS_QA( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
            END IF
!
          CASE('OS_TA')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_TA'
            CALL OS_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('NR_QP')
! ***       Equations for NONORGANIC RADIOACTIVE analytes and AQUATIC PLANTS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_QP'
              CALL NR_QP( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              DO IREL = 1, NREAL
                RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
              END DO
            END IF
!
          CASE('NR_TP')
! ***       Equations for NONORGANIC RADIOACTIVE analytes and TERRESTRIAL PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_TP'
            CALL NR_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('NR_QA')
! ***       Equations for NONORGANIC RADIOACTIVE analytes AQUATIC ANIMALS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_QA'
              CALL NR_QA( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              DO IREL = 1, NREAL
                RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
              END DO
            END IF
!
          CASE('NR_TA')
! ***       Equations for NONORGANIC RADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_TA'
            CALL NR_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_RP(ISPC,IREL) = RADSUM_RP(ISPC,IREL) + RDWORK(IREL)
            END DO
!
          CASE('NS_QP')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and AQUATIC PLANTS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_QP'
              CALL NS_QP( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
            END IF
!
          CASE('NS_TP')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and TERRESTRIAL PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_TP'
            CALL NS_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('NS_QA')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and AQUATIC ANIMALS
            IF( USE_SLOC ) THEN
              IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_TA'
              CALL NS_QA( ITIM, SLOC, ISPC, IANA, SKIP_OUTPUTS, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
            END IF
!
          CASE('NS_TA')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_TA'
            CALL NS_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE DEFAULT
! ***       Other types
!
        END SELECT
!
      END DO SPECIES_LOOP
!
      RETURN
      END SUBROUTINE RIPARIAN_SPECIES

      SUBROUTINE SET_GROWTH_RESPIRE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine sets the growth rate and respiration rates for
!!    all aquatic species.   These values are a constant for each species.
!!
!!  History:
!!
!!    Paul W. Eslinger : 12 Nov 1997 : Version 1.0
!!    Paul W. Eslinger : 18 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Revise comments
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variable
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number variable
!
! *** Local variables
      CHARACTER(LEN=18) :: CALLER = 'SET_GROWTH_RESPIRE' ! Name of this subroutine
      INTEGER :: ISPC ! Species index variable
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Debug message
!
      IF( BGCOST ) THEN
        WRITE(IRPT,1000) CALLER
 1000   FORMAT(/'Entering ',A)
        WRITE(IRPT,1010) GROWTH_DELTA, GROWTH_BETA, RESPIRE_PHI, RESPIRE_GAMMA
 1010   FORMAT(1P, &
          5X,'GROWTH_DELTA  = ',E11.4/&
          5X,'GROWTH_BETA   = ',E11.4/&
          5X,'RESPIRE_PHI   = ',E11.4/&
          5X,'RESPIRE_GAMMA = ',E11.4)
        WRITE(IRPT,1020)
 1020   FORMAT(/'Index    ID     Growth Rate  Respire Rate  Wet Body Mass'/ &
                '-----  ------  ------------  ------------  ------------')
      END IF
!
      DO ISPC = 1, ESD_NUM_SPC
!
! ***   Skip if the species is not being computed
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
!
! ***   Skip the species that are not aquatic
        IF( .NOT.(ESD_SPC(ISPC)%TYPE.EQ.'QA' .OR. ESD_SPC(ISPC)%TYPE.EQ.'QP') ) CYCLE
!
! ***   Check for valid species wet body mass
        IF( ESD_SPC(ISPC)%WBMASS .LE. 0.0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'Species ' // ESD_SPC(ISPC)%ID // ' has a 0 or negative wet body mass'
          CALL PRTERR( IERR, CALLER, 1 )
        END IF
!
! ***   Growth rate
        ESD_SPC(ISPC)%GROWRATE = GROWTH_DELTA * ESD_SPC(ISPC)%WBMASS**(-GROWTH_BETA)
!
! ***   Respiration rate (oxygen use rate)
        ESD_SPC(ISPC)%RESPRATE = RESPIRE_PHI * ESD_SPC(ISPC)%WBMASS**(-RESPIRE_GAMMA)
!
        IF( BGCOST ) THEN
          WRITE(IRPT,1030) ISPC, ESD_SPC(ISPC)%ID, ESD_SPC(ISPC)%GROWRATE, ESD_SPC(ISPC)%RESPRATE, ESD_SPC(ISPC)%WBMASS
 1030     FORMAT(I5,2X,A,1P,3(2X,E12.5))
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE SET_GROWTH_RESPIRE

      SUBROUTINE SET_LOCATIONS( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine sets the values of location constants for all locations.  This subroutine 
!!    is called from SET_ESD_2 to reduce the number of lines of code in that subroutine.
!!
!!  History:
!!    Paul W. Eslinger : 22 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2003 : SCR 1005 - Data changes on the
!!      LOCATION keywords to distinguish between aquatic and terrestrial
!!      location data needs
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Revise comments and error messages
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Function from RDBLK routines
!
! *** Call list variables
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'SET_LOCATIONS' ! Name of this subroutine
      INTEGER :: IDX  ! Indexing variable
      REAL :: RTMP    ! Temporary real variable
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize all values
!
      RTMP = -HUGE(1.0)
!
!     All locations
      ESD_LOC(ESD_NUM_LOC)%EASTING  = RTMP
      ESD_LOC(ESD_NUM_LOC)%NORTHING = RTMP
!
!     Aquatic locations
      ESD_LOC(ESD_NUM_LOC)%COXYGEN  = RTMP
!
!     Terrestrial locations
      ESD_LOC(ESD_NUM_LOC)%APSD     = RTMP
      ESD_LOC(ESD_NUM_LOC)%FOC      = RTMP
      ESD_LOC(ESD_NUM_LOC)%MSWIND   = RTMP
      ESD_LOC(ESD_NUM_LOC)%MZWIND   = RTMP
      ESD_LOC(ESD_NUM_LOC)%NECF     = RTMP
      ESD_LOC(ESD_NUM_LOC)%POROSITY = RTMP
      ESD_LOC(ESD_NUM_LOC)%RHOS     = RTMP
      ESD_LOC(ESD_NUM_LOC)%TEMP     = RTMP
      ESD_LOC(ESD_NUM_LOC)%VEGCOV   = RTMP
!
!-----------------------------------------------------------------------
!     Data required for all location types
!-----------------------------------------------------------------------
!
!     EASTING : Easting coordinate
      IF( CEXIST('EASTING') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_LOC(ESD_NUM_LOC)%EASTING = RTMP
        ELSE
          IERR = 1
          MESSAG(1) = 'Location EASTING modifier missing value'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      ELSE
        IERR = 2
        MESSAG(1) = 'EASTING modifier not entered on the LOCATION keyword'
        MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     NORTHING : Northing coordinate
      IF( CEXIST('NORTHING ') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_LOC(ESD_NUM_LOC)%NORTHING = RTMP
        ELSE
          IERR = 3
          MESSAG(1) = 'Location NORTHING modifier missing value'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      ELSE
        IERR = 4
        MESSAG(1) = 'NORTHING modifier not entered on the LOCATION keyword'
        MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!-----------------------------------------------------------------------
!     Data required for aquatic locations
!-----------------------------------------------------------------------
!
      IF( ESD_LOC(ESD_NUM_LOC)%SWAT .OR. ESD_LOC(ESD_NUM_LOC)%PWAT .OR. &
          ESD_LOC(ESD_NUM_LOC)%SEDI ) THEN
!
!       COXYEGN : Oxygen concentration in surface water
        IF( CEXIST('COXYGEN') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%COXYGEN = RTMP
          ELSE
            IERR = 5
            MESSAG(1) = 'Location COXYGEN modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 6
          MESSAG(1) = 'COXYGEN modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       TEMP : Temperature (in Centigrade) (needed for some data configurations, but not all)
        IF( CEXIST('TEMP') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%TEMP = RTMP
          ELSE
            IERR = 123
            MESSAG(1) = 'Location TEMP modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            MESSAG(3) = 'Modify the LOCATION keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        END IF
!
      END IF
!
!-----------------------------------------------------------------------
!     Data required for terrestrial locations
!-----------------------------------------------------------------------
!
      IF( ESD_LOC(ESD_NUM_LOC)%GWAT .OR. ESD_LOC(ESD_NUM_LOC)%SEEP .OR. &
          ESD_LOC(ESD_NUM_LOC)%SORP .OR. ESD_LOC(ESD_NUM_LOC)%SODR .OR. &
          ESD_LOC(ESD_NUM_LOC)%SOGW .OR. ESD_LOC(ESD_NUM_LOC)%SOSW ) THEN
!
!       APSD : Aggregate particle size distribution
        IF( CEXIST('APSD') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%APSD = RTMP
          ELSE
            IERR = 7
            MESSAG(1) = 'Location APSD modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 8
          MESSAG(1) = 'APSD modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       FOC : Fraction soil organic carbon content
        IF( CEXIST('FOC') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%FOC = RTMP
          ELSE
            IERR = 9
            MESSAG(1) = 'Location FOC modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 10
          MESSAG(1) = 'FOC modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       MSWIND : Mean annual wind speed
        IF( CEXIST('MSWIND') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%MSWIND = RTMP
          ELSE
            IERR = 11
            MESSAG(1) = 'Location MSWIND modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 12
          MESSAG(1) = 'MSWIND modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       MZWIND : Mixing zone wind speed
        IF( CEXIST('MZWIND') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%MZWIND = RTMP
          ELSE
            IERR = 13
            MESSAG(1) = 'Location MZWIND modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 14
          MESSAG(1) = 'MZWIND modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       NECF : Nonerodible elements corr factor
        IF( CEXIST('NECF') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%NECF = RTMP
          ELSE
            IERR = 15
            MESSAG(1) = 'Location NECF modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 16
          MESSAG(1) = 'NECF modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       POROSITY : Soil porosity
        IF( CEXIST('POROSITY') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%POROSITY = RTMP
          ELSE
            IERR = 17
            MESSAG(1) = 'Location POROSITY modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 18
          MESSAG(1) = 'POROSITY modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       RHOS : Soil density
        IF( CEXIST('RHOS') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%RHOS = RTMP
          ELSE
            IERR = 19
            MESSAG(1) = 'Location RHOS modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 20
          MESSAG(1) = 'RHOS modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       SRH : Surface roughness height
        IF( CEXIST('SRH') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%SRH = RTMP
          ELSE
            IERR = 21
            MESSAG(1) = 'Location SRH modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 22
          MESSAG(1) = 'SRH modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       TEMP : Temperature (in Centigrade)
        IF( CEXIST('TEMP') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%TEMP = RTMP
          ELSE
            IERR = 23
            MESSAG(1) = 'Location TEMP modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            MESSAG(3) = 'Modify the LOCATION keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          IERR = 24
          MESSAG(1) = 'TEMP modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          MESSAG(3) = 'Modify the LOCATION keyword in the ESD keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
!       VEGCOV : Fraction of vegetation cover
        IF( CEXIST('VEGCOV') ) THEN
          CALL NXTVAL( IDX, RTMP )
          IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%VEGCOV = RTMP
          ELSE
            IERR = 25
            MESSAG(1) = 'Location VEGCOV modifier missing value'
            MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
        ELSE
          IERR = 26
          MESSAG(1) = 'VEGCOV modifier not entered on the LOCATION keyword'
          MESSAG(2) = 'Location ID = "'//TRIM(ESD_LOC(ESD_NUM_LOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
      END IF
!
      RETURN
      END SUBROUTINE SET_LOCATIONS

      SUBROUTINE SET_SPECIES( IERR )
!!**************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine sets the values of constants for all species using keywords
!!    in the ESD keyword file.  This subroutine is called from KEY_ESD_2 to reduce
!!    the number of lines of code in that subroutine.
!!
!!    Many of the parameters do not apply to all species, therefore each of the
!!    data entries are treated as optional.
!!
!!  History:
!!
!!    Paul W. Eslinger : 22 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 26 Oct 2004 : Add terrestrial animal secondary transfer factor
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**************************************************************************************
!
! *** Global variables
      USE Param_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Rdblk_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Function from RDBLK routines
!
! *** Call List variables
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'SET_SPECIES' ! Name of this subroutine
      REAL :: RTMP   ! Temporary real variable
      INTEGER :: IDX ! Indexing variable
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
!     AE : Assimilation efficiency
      IF( CEXIST('AE') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%AE = RTMP
        ELSE
          IERR = 1
          MESSAG(1) = 'Species AE modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     AWD : Species weight to dry weight ratio
      IF( CEXIST('AWD') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%AWD = RTMP
        ELSE
          IERR = 2
          MESSAG(1) = 'Species AWD modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     DIFFHT : Plant diffusion height
      IF( CEXIST('DIFFHT') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%DIFFHT = RTMP
        ELSE
          IERR = 3
          MESSAG(1) = 'Species DIFFHT modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     EMERGENT : Water emergent plants
      IF( CEXIST('EMERGENT') ) THEN
        ESD_SPC(ESD_NUM_SPC)%EMERGENT = .TRUE.
      END IF
!
!     TRANSFER : Terrestrial animal secondary transfer factors
      IF( CEXIST('TRANSFER') ) THEN
        ESD_SPC(ESD_NUM_SPC)%TRANS = .TRUE.
      END IF
!
!     ETWATER : Exposure time to water
      IF( CEXIST('ETWATER') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%ETWATER = RTMP
        ELSE
          IERR = 4
          MESSAG(1) = 'Species ETWATER modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FDW : Conversion, dry weight to wet weight
      IF( CEXIST('FDW') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FDW = RTMP
        ELSE
          IERR = 5
          MESSAG(1) = 'Species FDW modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FLIPID : Species fraction lipid
      IF( CEXIST('FLIPID') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FLIPID = RTMP
        ELSE
          IERR = 6
          MESSAG(1) = 'Species FLIPID modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FMR : Metabolic rate of predator
      IF( CEXIST('FMR') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FMR = RTMP
        ELSE
          IERR = 7
          MESSAG(1) = 'Species FMR modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FOC : Species fraction organic carbon
      IF( CEXIST('FOC') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FOC = RTMP
        ELSE
          IERR = 8
          MESSAG(1) = 'Species FOC modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FPA : Volume fraction of plant tissue that is air
      IF( CEXIST('FPA') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FPA = RTMP
        ELSE
          IERR = 9
          MESSAG(1) = 'Species FPA modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FPL : Volume fraction of plant tissue that is lipid
      IF( CEXIST('FPL') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FPL = RTMP
        ELSE
          IERR = 10
          MESSAG(1) = 'Species FPL modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FPW : Volume fraction of plant tissue that is water
      IF( CEXIST('FPW') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FPW = RTMP
        ELSE
          IERR = 11
          MESSAG(1) = 'Species FPW modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FW : Water weight fraction of plant tissue
      IF( CEXIST('FW') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FW = RTMP
        ELSE
          IERR = 12
          MESSAG(1) = 'Species FW modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     FWATER : Fraction body exposure to water
      IF( CEXIST('FWATER') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%FWATER = RTMP
        ELSE
          IERR = 13
          MESSAG(1) = 'Species FWATER modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     GE : Gross energy
      IF( CEXIST('GE') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%GE = RTMP
        ELSE
          IERR = 14
          MESSAG(1) = 'Species GE modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     INHRATE : Resting inhalation rate
      IF( CEXIST('INHRATE') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%INHRATE = RTMP
        ELSE
          IERR = 15
          MESSAG(1) = 'Species INHRATE modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     OCAR : Organic carbon assimilation rate
      IF( CEXIST('OCAR') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%OCAR = RTMP
        ELSE
          IERR = 16
          MESSAG(1) = 'Species OCAR modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     PCS : Fraction of surface area in contact with soil
      IF( CEXIST('PCS') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%PCS = RTMP
        ELSE
          IERR = 17
          MESSAG(1) = 'Species PCS modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     PCW : Fraction of surface area available to water
      IF( CEXIST('PCW') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%PCW = RTMP
        ELSE
          IERR = 18
          MESSAG(1) = 'Species PCW modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     PSI : Seasonality factor
      IF( CEXIST('PSI') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%PSI = RTMP
        ELSE
          IERR = 19
          MESSAG(1) = 'Species PSI modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     RADIUS : Radius (for radioactive dose)
      IF( CEXIST('RADIUS') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%RADIUS = RTMP
        ELSE
          IERR = 20
          MESSAG(1) = 'Species RADIUS modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     RHOP : Plant tissue density
      IF( CEXIST('RHOP') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%RHOP = RTMP
        ELSE
          IERR = 21
          MESSAG(1) = 'Species RHOP modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     SA : Surface area
      IF( CEXIST('SA') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%SA = RTMP
        ELSE
          IERR = 22
          MESSAG(1) = 'Species SA modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     SADHER : Skin adherence factor
      IF( CEXIST('SADHER') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%SADHER = RTMP
        ELSE
          IERR = 23
          MESSAG(1) = 'Species SADHER modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     SEDING : Sediment ingestion fraction
      IF( CEXIST('SEDING') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%SEDING = RTMP
        ELSE
          IERR = 24
          MESSAG(1) = 'Species SEDING modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     SOILING : Soil ingestion rate
      IF( CEXIST('SOILING') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%SOILING = RTMP
        ELSE
          IERR = 25
          MESSAG(1) = 'Species SOILING modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     THETA : Area use factor
      IF( CEXIST('THETA') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%THETA = RTMP
        ELSE
          IERR = 27
          MESSAG(1) = 'Species THETA modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     WATERING : Water ingestion rate
      IF( CEXIST('WATERING') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%WATERING = RTMP
        ELSE
          IERR = 28
          MESSAG(1) = 'Species WATERING modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     WBMASS : Wet body mass
      IF( CEXIST('WBMASS') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%WBMASS = RTMP
        ELSE
          IERR = 29
          MESSAG(1) = 'Species WBMASS modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
!     WEIGHT : Body weight
      IF( CEXIST('WEIGHT') ) THEN
        CALL NXTVAL( IDX, RTMP )
        IF( IDX .GT. 0 ) THEN
          ESD_SPC(ESD_NUM_SPC)%WEIGHT = RTMP
        ELSE
          IERR = 30
          MESSAG(1) = 'Species WEIGHT modifier missing value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
      RETURN
      END SUBROUTINE SET_SPECIES

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
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger : 22 Jan 2014 : Upgrade error trapping
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
      CHARACTER(LEN=9) :: CALLER = 'SET_UNITS' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
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
            MESSAG(4) = 'Analyte is ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 4 )
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
!         Radioactive - air deposition
          IF( ECDA_UNITS(IAIRD) == 'Ci/m^2/yr'  ) THEN
            CONVERT_UNITS(IANA,IAIRD) = 1.0E12
            CONVERT_LABELS(IANA,IAIRD,1) = TRIM(ECDA_UNITS(IAIRD))
            CONVERT_LABELS(IANA,IAIRD,2) = 'pCi/m^2/yr'
          ELSE
            IERR = 12
            MESSAG(1) = 'Unexpected units for air deposition in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IAIRD))
            MESSAG(3) = 'Units expected were ' // 'Ci/m^2/yr'
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
            MESSAG(4) = 'Analyte is ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 4 )
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
!         Stable - air deposition
          IF( ECDA_UNITS(IAIRD) == 'kg/m^2/yr'  ) THEN
            CONVERT_UNITS(IANA,IAIRD) = 1.0E9
            CONVERT_LABELS(IANA,IAIRD,1) = TRIM(ECDA_UNITS(IAIRD))
            CONVERT_LABELS(IANA,IAIRD,2) = 'ug/m^2/yr'
          ELSE
            IERR = 23
            MESSAG(1) = 'Unexpected units for air deposition in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IAIRD))
            MESSAG(3) = 'Units expected were ' // 'kg/m^2/yr'
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
      END SUBROUTINE SET_UNITS

      SUBROUTINE STOGEN_ANALYTES( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the stochastic
!!    variables that depend only on analyte.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jun 1998 : Version 1.0
!!    Paul W. Eslinger : 13 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 20 Jan 2004 : Change BCFVEG and KOW to element
!!      specific basis rather than isotope specific basis
!!    Paul W. Eslinger :  3 May 2004 : Not require BCFVEG for tritium
!!    Paul W. Eslinger : 28 Jul 2004 : Change comments
!!    Paul W. Eslinger : 25 Oct 2004 : Change BCFVEG to have plant and
!!                                     analyte dependency
!!    Paul W. Eslinger :  6 Dec 2004 : Change length of VNAME to 24
!!    Paul W. Eslinger : 11 Jan 2005 : (SCR-1070) KOW now generated only for terrestrial plants
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Stats_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'STOGEN_ANALYTES' ! Name of this subroutine
!
      CHARACTER(LEN=24) :: VNAME ! Variable name (ID) for determining the stochastic distribution
!
      INTEGER :: IREL ! Realization looping index
      INTEGER :: VIDX ! Stochastic variable index
      INTEGER :: IANA ! Analyte looping index
!
      INTEGER :: IERA    ! Error status variable from the allocate action
      INTEGER :: ISTO    ! Stochastic variable looping index
      INTEGER :: NUM_KOW ! Number of stochastic KOW variables
      LOGICAL :: FOUND_KOW ! Flag for matching with a KOW value
      CHARACTER(LEN=6), ALLOCATABLE :: ELEMENT_ID_KOW(:) ! Element list for ID's for KOW
      REAL, ALLOCATABLE :: ELEMENT_KOW(:,:)    ! Temporary stochastic values for KOW
!
!---- Executable code ---------------------------------------------------
!
      IERR   = 0
!
      IF( BGLOOP ) WRITE(IRPT,1000) CALLER
 1000 FORMAT(/'Entering: ',A)
!
! *** Determine the number of stochastic KOW definitions
!
      NUM_KOW = 0
      DO ISTO = 1, INDSTO
        IF( VLABEL(ISTO)(7: 9) .EQ. 'KOW'    ) NUM_KOW = NUM_KOW + 1
      END DO
!
!-----------------------------------------------------------------------------------------
!     Generate the temporary values for KOW (by element)
!-----------------------------------------------------------------------------------------
!
      IF( NUM_KOW .LE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Inconsistent inputs: Terrestrial plants are being computed'
        MESSAG(2) = 'but no stochastic definitions containing KOW were detected.'
        MESSAG(3) = 'Error in the ECEM keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      ALLOCATE( ELEMENT_ID_KOW(NUM_KOW), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating memory for ELEMENT_ID_KOW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ELEMENT_KOW(NUM_KOW,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for ELEMENT_KOW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      NUM_KOW = 0
      DO ISTO = 1, INDSTO
!       Skip all stochastic variables that are not KOW identifiers
        IF( VLABEL(ISTO)(7:9) .NE. 'KOW' ) CYCLE
!       Increment the counter
        NUM_KOW = NUM_KOW + 1
!       Save the element ID from the STOCHASTIC keyword
        ELEMENT_ID_KOW(NUM_KOW) = VLABEL(ISTO)(1:6)
!       Set up and generate the stochastic values
        VNAME = ELEMENT_ID_KOW(NUM_KOW)//'KOW'
!        WRITE(*,*) 'Generating variable "' // TRIM(VNAME) // '"'
        VIDX = ISTO
        CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!       Save the generated values in a temporary array
        DO IREL = 1, NREAL
          ELEMENT_KOW(NUM_KOW,IREL) = WORK(IREL)
        END DO
      END DO
!
!-----------------------------------------------------------------------------------------
!     Assign the temporary values to the use locations
!-----------------------------------------------------------------------------------------
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( BGLOOP ) THEN
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%COMP
 1010     FORMAT('STOGEN_ANALYTES, IANA=',I3,' ID=',A,' Compute=',L1)
        END IF
!
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
! ***   Octonal-water partition coefficient
!
        VNAME = ESD_ANA(IANA)%ELEM_ID//'KOW'
        FOUND_KOW = .FALSE.
        DO ISTO = 1, NUM_KOW
          IF( ELEMENT_ID_KOW(ISTO) .EQ. ESD_ANA(IANA)%ELEM_ID ) THEN
            FORALL(IREL=1:NREAL) ESD_ANA(IANA)%KOW(IREL) = ELEMENT_KOW(ISTO,IREL)
            FOUND_KOW = .TRUE.
            EXIT ! Terminate the DO loop after data is found
          END IF
        END DO
        IF( .NOT.FOUND_KOW ) THEN
          IERR = 7
          MESSAG(1) = 'Data mismatch for analyte '//TRIM(ESD_ANA(IANA)%ID)
          MESSAG(2) = 'Trying to get stochastic KOW using the string "'//TRIM(VNAME)//'"'
          MESSAG(3) = 'Problem in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
      END DO
!
! *** Free up the memory in the temporary work locations
      DEALLOCATE( ELEMENT_ID_KOW )
      DEALLOCATE( ELEMENT_KOW )
!
!      STOP 'TEMPORARY STOP'
      RETURN
      END SUBROUTINE STOGEN_ANALYTES

      SUBROUTINE STOGEN_ANALYTES_SPECIES( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the stochastic
!!    variables that depend on both analyte and species.
!!
!!  History:
!!
!!    Paul W. Eslinger :  8 Jun 1998 : Version 1.0
!!    Paul W. Eslinger : 13 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 10 May 2002 : Add BMTISS
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 25 Oct 2004 : Change BCFVEG to have plant and
!!                                     analyte dependency
!!    Paul W. Eslinger : 26 Oct 2004 : Add optional transfer factor for
!!                                     terrestrial animals
!!    Paul W. Eslinger : 18 Nov 2004 : Add KLIDRY and KLIWET
!!    Paul W. Eslinger :  6 Dec 2004 : Add IFRACDRY, IFRAACWET,
!!                                     LWEATHDRY and LWEATHWET
!!    Paul W. Eslinger :  6 Dec 2004 : Change length of VNAME to 24
!!    Paul W. Eslinger :  6 Jul 2005 : Remove BCFVEG for organics
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Change BMTISS to all nonrad
!!                                     Make dependent on output selections
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : Add inhalation term to the body burden for aquatic animals
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
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Stats_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=23) :: CALLER = 'STOGEN_ANALYTES_SPECIES' ! Name of this subroutine
      INTEGER :: VIDX ! Stochastic variable index
      CHARACTER(LEN=24) :: VNAME ! Temporary stochastic variable name
!
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ISPC ! Species looping index
      INTEGER :: IREL ! Realization looping index
!
      INTEGER :: NUM_BCFVEG   ! Number of BCFVEG stochastic variables
      INTEGER :: NUM_TRANSFER ! Number of TRANSFER stochastic variables
!
!---- Executable code ---------------------------------------------------
!
      IERR   = 0
!
      IF( BGLOOP ) WRITE(IRPT,1000) CALLER
 1000 FORMAT(/'Entering: ',A)
!
! *** Generate all values for BCFVEG (Plant species by Element)
      CALL GEN_BCFVEG( NUM_BCFVEG, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Generate all values for TRANSFER (terrestrial animal species by element)
!     This is an optional input
!
      CALL GEN_TRANSFER( NUM_TRANSFER, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
        DO ISPC = 1, ESD_NUM_SPC
!
          IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
!
! ------- Compute things needed by terrestrial plant species
!
          IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
!
! ***       Transfer factor from leaf (dry) to plant internal for terrestrial plants
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'KLIDRY'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%KLIDRY(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Transfer factor from leaf (wet) to plant internal for terrestrial plants
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'KLIWET'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%KLIWET(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Leaf weathering rate (dry deposition) for terrestrial plant species
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'LWEATHDRY'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%LWEATHDRY(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Leaf weathering rate (wet deposition) for terrestrial plant species
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'LWEATHWET'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%LWEATHWET(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Leaf interception fraction (dry deposition) for terrestrial plant species
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'IFRACDRY'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%IFRACDRY(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Leaf interception fraction (wet deposition) for terrestrial plant species
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'IFRACWET'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%IFRACWET(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Vegetation bioconcentration factor (except for tritium)
            IF( ESD_ANA(IANA)%ID .NE. 'H3' ) THEN
              IF( ESD_ANA(IANA)%TYPE(1:1) .NE. 'O' ) THEN
                CALL EXTRACT_BCFVEG( IANA, ISPC, NUM_BCFVEG, WORK, IERR )
                IF( IERR .NE. 0 ) THEN
                  IERR = 999
                  MESSAG(1) = 'Error in lower level routine'
                  CALL PRTERR( IERR, CALLER, 1 )
                  RETURN
                END IF
                DO IREL = 1, NREAL
                  ESD_SPC(ISPC)%BCFVEG(IANA,IREL) = WORK(IREL)
                END DO
              END IF
            END IF
!
          END IF
!
! ------- Compute things needed by terrestrial animal species
!
          IF( ESD_SPC(ISPC)%TYPE .EQ. 'TA' ) THEN
!
! ***       Ingestion absorption factor
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'ALPHAING'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%ALPHAING(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Inhalation particulate absorption factor
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'ALPHAPAR'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%ALPHAPAR(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Inhalation vapor absorption factor
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'ALPHAVAP'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Dermal absorption from water
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'ALPHADW'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%ALPHADW(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Dermal absorption from soil
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'ALPHADS'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%ALPHADS(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Secondary transfer factor
            IF( ESD_SPC(ISPC)%TRANS ) THEN
              CALL EXTRACT_TRANSFER( IANA, ISPC, NUM_TRANSFER, WORK, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              DO IREL = 1, NREAL
                ESD_SPC(ISPC)%TRANSVAL(IANA,IREL) = WORK(IREL)
              END DO
            END IF
!
          END IF
!
! ------- Compute things needed by aquatic animal species
!
          IF( ESD_SPC(ISPC)%TYPE .EQ. 'QA' ) THEN
!
! ***       Inhalation vapor absorption factor (only needed for air breathing aquatic species)
            IF( ESD_SPC(ISPC)%INHRATE .GT. 0.0 ) THEN
              VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'ALPHAVAP'
              VIDX = 0
              CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              DO IREL = 1, NREAL
                ESD_SPC(ISPC)%ALPHAVAP(IANA,IREL) = WORK(IREL)
              END DO
            END IF
!
! ***       Chemical assimilation efficiency
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'ALPHAIJ'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%ALPHAIJ(IANA,IREL) = WORK(IREL)
            END DO
!
! ***       Metabolism loss rate (for organic analytes)
            IF( ESD_ANA(IANA)%TYPE.EQ.'OR' .OR. ESD_ANA(IANA)%TYPE.EQ.'OS' ) THEN
              VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'METBLOSS'
              VIDX = 0
              CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error in lower level routine'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              DO IREL = 1, NREAL
                ESD_SPC(ISPC)%METBLOSS(IANA,IREL) = WORK(IREL)
              END DO
            END IF
!
          END IF
!
! ------- Compute tissue benchmarks for nonrad analytes for species with outputs turned on
          IF( (STA_EHQ.OR.DET_EHQ) .AND. (ESD_ANA(IANA)%TYPE(2:2).EQ.'S') .AND. ESD_SPC(ISPC)%OUTPUT ) THEN
! ***       Tissue benchmark for aquatic species
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'BMTISS'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%BMTISS(IANA,IREL) = WORK(IREL)
              IF( WORK(IREL) .LE. 0 ) THEN
                IERR = 1
                MESSAG(1) = 'Value must be greater than 0'
                MESSAG(2) = 'Variable: ' // TRIM(VNAME)
                MESSAG(3) = 'Value = '
                WRITE(MESSAG(3)(9:),*) WORK(IREL)
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END DO
!
          END IF
!
! ------- Compute things needed by aquatic plant and animal species for nonorganic analytes
!
          IF( ESD_SPC(ISPC)%TYPE(1:1).EQ.'Q' .AND. ESD_ANA(IANA)%TYPE(1:1).EQ.'N' ) THEN
!
! ***       Bioconcentration factor
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'BCF'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              ESD_SPC(ISPC)%BCF(IANA,IREL) = WORK(IREL)
            END DO
!
          END IF
!
! ------- Compute things needed by (all terrestrial animals) and
!         (aquatic animal species for nonorganic analytes)
!
!          IF( ESD_SPC(ISPC)%TYPE(2:2).EQ.'A' ) THEN
          IF( ESD_SPC(ISPC)%TYPE.EQ.'TA' .OR. (ESD_SPC(ISPC)%TYPE.EQ.'QA'.AND.ESD_ANA(IANA)%TYPE(1:1).EQ.'N') ) THEN
!
! ***       Depuration rate
            VNAME = ESD_ANA(IANA)%ID//ESD_SPC(ISPC)%ID//'DEPRATE'
            VIDX = 0
            CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              IF( WORK(IREL) .LE. 0.0 ) THEN
                IERR = 1
                MESSAG(1) = 'Depuration rate must be greater than zero'
                MESSAG(2) = 'Variable: "' // TRIM(VNAME) // '"'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
              ESD_SPC(ISPC)%DEPRATE(IANA,IREL) = WORK(IREL)
            END DO
!
         END IF
!
        END DO
      END DO
!
! *** Close the BCFVEG and TRANSFER scratch files
!
      IF( NUM_BCFVEG   .GT. 0 ) CLOSE( IBCF )
      IF( NUM_TRANSFER .GT. 0 ) CLOSE( ITRN )
!
      RETURN
      END SUBROUTINE STOGEN_ANALYTES_SPECIES

      SUBROUTINE STOGEN_SPECIES( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the stochastic
!!    variables that depend only on species.
!!
!!  History:
!!
!!    Paul W. Eslinger :  2 Apr 1998 : Version 1.0
!!    Paul W. Eslinger :  9 May 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 18 Nov 2005 : Add terrestrial plant variables
!!      BIOMASS, LWEATHDRY, LWEATHWET, IFRACDRY, IFRACWET
!!    Paul W. Eslinger :  6 Dec 2004 : Change length of VNAME to 24
!!    Paul W. Eslinger : 11 Jan 2005 : Change NEED_RAD to USE_RADS
!!    Paul W. Eslinger :  4 Jun 2005 : Apply FABOVE to all species
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
! *** Global variables
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Stats_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'STOGEN_SPECIES' ! Name of this subroutine
!
      INTEGER :: VIDX            ! Stochastic variable index
      CHARACTER(LEN=24) :: VNAME ! Stochastic variable name
!
      INTEGER :: IREL ! Realization looping index
      INTEGER :: ISPC ! Species looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR   = 0
!
      IF( BGLOOP ) WRITE(IRPT,1000) CALLER
 1000 FORMAT(/'Entering: ',A)
!
      DO ISPC = 1, ESD_NUM_SPC
!
        IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
!
! ***   Terrestrial plant plant standing biomass (kg/m^2 wet weight)
!
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
          VNAME = ESD_SPC(ISPC)%ID//'BIOMASS'
          VIDX = 0
          CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            ESD_SPC(ISPC)%BIOMASS(IREL) = WORK(IREL)
          END DO
        END IF
!
! ***   Plant-soil partition coefficient for terrestrial plant species
!
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
          VNAME = ESD_SPC(ISPC)%ID//'KPS1'
          VIDX = 0
          CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            ESD_SPC(ISPC)%KPS1(IREL) = WORK(IREL)
          END DO
        END IF
!
! ***   Plant-air partition coefficient for terrestrial plant species
!
        IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
          VNAME = ESD_SPC(ISPC)%ID//'KPA2'
          VIDX = 0
          CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            ESD_SPC(ISPC)%KPA2(IREL) = WORK(IREL)
          END DO
        END IF
!
! ***   Relative exposure time to pore water for aquatic species
!
        IF( ESD_SPC(ISPC)%TYPE.EQ.'QA' .OR. ESD_SPC(ISPC)%TYPE.EQ.'QP') THEN
          VNAME = ESD_SPC(ISPC)%ID//'BPORE'
          VIDX = 0
          CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            ESD_SPC(ISPC)%BPORE(IREL) = WORK(IREL)
          END DO
        END IF
!
! ***   Relative exposure time to above ground radiation for terrestrial species
!
        IF( USE_RADS ) THEN
          VNAME = ESD_SPC(ISPC)%ID//'FABOVE'
          VIDX = 0
          CALL STONE( VNAME, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            ESD_SPC(ISPC)%FABOVE(IREL) = WORK(IREL)
          END DO
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE STOGEN_SPECIES

      SUBROUTINE TA_INTAKE( ISPC )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine calculates the intake of food products in kg/day
!!    (wet weight basis) for a single terrestrial animal species.
!!
!!  Call List Variables:
!!
!!    ISPC  : Species index
!!
!!  History:
!!
!!    Paul W. Eslinger : 24 Jan 2005 : Original Code
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Species_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: ISPC
!
! *** Local variables
      INTEGER :: JSPC              ! Species index
      REAL :: SUMPROD, NIR, NIRTMP ! Temporary intake variables
!
!---- Executable code --------------------------------------------
!
! *** Set food consumption to zero for all species
      DO JSPC = 1, ESD_NUM_SPC
        FOOD(JSPC) = 0.0
      END DO
!
! *** Calculate total normalized intake rate (not dependent on realization) for the predator
      SUMPROD = 0.0
      DO JSPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
        SUMPROD = SUMPROD + PREDATE(ISPC,JSPC) * ESD_SPC(JSPC)%GE * ESD_SPC(JSPC)%AE
      END DO
      IF( SUMPROD .GT. 0.0 ) THEN
        NIRTMP = ESD_SPC(ISPC)%FMR / SUMPROD
      ELSE
        NIRTMP = 0.0
      END IF
!
! *** Calculate intake rate by prey species
      DO JSPC = 1, ESD_NUM_SPC
        IF( .NOT. ESD_SPC(JSPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
        NIR = PREDATE(ISPC,JSPC) * NIRTMP
        FOOD(JSPC) = NIR
      END DO
!
      RETURN
      END SUBROUTINE TA_INTAKE

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
      END SUBROUTINE TELLTIME

      SUBROUTINE UPLAND_SPECIES( WTYPE, ITIM, ILOC, IANA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules impact calculations for all terrestrial
!!    species at upland locations.
!!
!!  Logic:
!!
!!    The WTYPE variable controls the source of water and soil
!!    concentrations to be used by all terrestrial species.  The
!!    valid options are:
!!      SODR : No irrigation, soil=SODR and water is set to zero
!!      SOGW : Groundwater irrigation, soil=SOGW, water= GWAT
!!      SOSW : Surface water irrigation, soil=SOSW, water= SWAT
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 18 Jul 2006 : (SCR-1137) Change water and soil
!!      assignments depending on irrigation
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Param_Mod
      USE Debug_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Species_Mod
      USE Esd_Mod
      USE Media_Mod
      USE Results_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT (IN) :: WTYPE ! Water type indicator
      INTEGER, INTENT (IN) :: ITIM ! Time index
      INTEGER, INTENT (IN) :: ILOC ! Location index
      INTEGER, INTENT (IN) :: IANA ! Analyte index
      INTEGER :: IERR              ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'UPLAND_SPECIES' ! Name of this subroutine
!
      CHARACTER(LEN=5) :: CTMP ! Selection variable for impacts routine
      INTEGER :: IREL ! Realization index
      INTEGER :: ISPC ! Species index
      INTEGER :: IDX  ! Species loop indexing variable
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      IF( BGLOOP ) THEN
        WRITE(IRPT,1000) CALLER, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID
 1000   FORMAT(/'In subroutine: ',A,' : Time = ',I0,' : Location = ',A,' : Analyte = ',A)
      END IF
!
! *** Loop over all species
!
      SPECIES_LOOP: DO IDX = 1, ESD_NUM_SPC
!
! ***   Compute the species in the permuted order
!       Skip the species not used in this scenario
        ISPC = ESD_SPC(IDX)%ORDER
        IF( ISPC .LT. 0 ) CYCLE ! Species not used at all
        IF( ESD_SPC(ISPC)%HABITAT .NE. 'UPLAND' ) CYCLE ! Species is not upland
!
! ***   Set the water and soil concentrations to the appropriate source
!       The soil concentrations already account for the onset of irrigation
!       Plants are not irrigated until the onset of irrigation but animals
!       get their water from the irrigation source for all times
        SELECT CASE( WTYPE )
!
          CASE('SODR') ! No irrigation and no water source
            FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = 0.0
            FORALL(IREL=1:ESD_NREAL) EC(IREL)%SOIL  = EC(IREL)%SODR
            SOIL_ID = 'SODR'
            SOIL_IDX = 1
!
          CASE('SOGW')  ! Irrigation water source is groundwater
            IF( ESD_TIM(ITIM)%TIME .GE. IRIG_START ) THEN
              FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = EC(IREL)%GWAT
            ELSE
              IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
                FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = 0.0
              ELSE
                FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = EC(IREL)%GWAT
              END IF
            END IF
            FORALL(IREL=1:ESD_NREAL) EC(IREL)%SOIL  = EC(IREL)%SOGW
            SOIL_ID = 'SOGW'
            SOIL_IDX = 2
!
          CASE('SOSW')  ! Irrigation water source is surface water
            IF( ESD_TIM(ITIM)%TIME .GE. IRIG_START ) THEN
              FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = EC(IREL)%SWAT
            ELSE
              IF( ESD_SPC(ISPC)%TYPE .EQ. 'TP' ) THEN
                FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = 0.0
              ELSE
                FORALL(IREL=1:ESD_NREAL) EC(IREL)%WATER = EC(IREL)%SWAT
              END IF
            END IF
            FORALL(IREL=1:ESD_NREAL) EC(IREL)%SOIL  = EC(IREL)%SOSW
            SOIL_ID = 'SOSW'
            SOIL_IDX = 3
!
          CASE DEFAULT ! Invalid selection indicator
            IERR = 1
            MESSAG(1) = 'Invalid water and soil type selection parameter'
            MESSAG(2) = 'Value entered was "'//WTYPE//'"'
            MESSAG(3) = 'Valid values are SOGW, SOSW, or SODR'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
!
        END SELECT
!
        IF( BGLOOP ) WRITE(IRPT,1010) IDX, ISPC, ESD_SPC(ISPC)%ID
 1010   FORMAT(' Processing Species: Loop Index = ',I0,' Species index = ',I0,' Species ID = ',A)
!
        CTMP = ESD_ANA(IANA)%TYPE // '_' // ESD_SPC(ISPC)%TYPE
!        IF( BGLOOP ) WRITE(IRPT,'(A)') 'CTMP = '//CTMP
        SELECT CASE( CTMP )
!
          CASE('OR_TP')
! ***       Equations for ORGANIC RADIOACTIVE analytes and TERRESTRIAL PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OT_TP'
            CALL OR_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_UP(ISPC,IREL,SOIL_IDX) = RADSUM_UP(ISPC,IREL,SOIL_IDX) + RDWORK(IREL)
            END DO
!
          CASE('OR_TA')
! ***       Equations for ORGANIC RADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OT_TA'
            CALL OR_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_UP(ISPC,IREL,SOIL_IDX) = RADSUM_UP(ISPC,IREL,SOIL_IDX) + RDWORK(IREL)
            END DO
!
          CASE('OS_TP')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and TERRESTRIAL PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_TP'
            CALL OS_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('OS_TA')
! ***       Equations for ORGANIC NONRADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling OS_TA'
            CALL OS_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('NR_TP')
! ***       Equations for NONORGANIC RADIOACTIVE analytes and TERRESTRIAL PLANTS
!
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_TP : Case '//WTYPE
            CALL NR_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_UP(ISPC,IREL,SOIL_IDX) = RADSUM_UP(ISPC,IREL,SOIL_IDX) + RDWORK(IREL)
            END DO
!
          CASE('NR_TA')
! ***       Equations for NONORGANIC RADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NR_TA'
            CALL NR_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RADSUM_UP(ISPC,IREL,SOIL_IDX) = RADSUM_UP(ISPC,IREL,SOIL_IDX) + RDWORK(IREL)
            END DO
!
          CASE('NS_TP')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and TERRESTRIAL PLANTS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_TP'
            CALL NS_TP( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE('NS_TA')
! ***       Equations for NONORGANIC NONRADIOACTIVE analytes and TERRESTRIAL ANIMALS
            IF( BGLOOP ) WRITE(IRPT,'(A)') 'Calling NS_TA'
            CALL NS_TA( ITIM, ILOC, ISPC, IANA, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE DEFAULT
! ***       Other types
!
        END SELECT
!
      END DO SPECIES_LOOP
!
      RETURN
      END SUBROUTINE UPLAND_SPECIES

      SUBROUTINE USTAT_ECEM( X, N, UWORK, XMIN, XV05, XV10, XV25, XMED, &
        XV75, XV90, XV95, XMAX, XAVG, XSTD, LT1, TH1, PR1, LT2, TH2, PR2, IERR )
!!***************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine will compute some summary statistics for the N values in the
!!    vector X.  In addition, it will optionally calculate the probability of exceeding
!!    two user specified thresholds
!!
!!  Change History:
!!
!!    Paul W. Eslinger :  2 Mar 1998 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 1999 : Version 1.1
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 11 May 2005 : Fix logic for calculating thresholds
!!                                     with sample size 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!  Auxiliary Routines Required:
!!
!!    SORT : A subroutine to do a quicksort on a REAL vector
!!
!!***************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL, INTENT(IN), DIMENSION(*) :: X ! Vector of input values
      INTEGER, INTENT(IN) :: N            ! Number of values in the vector X
      REAL, DIMENSION(*) :: UWORK         ! A work vector with the same dimension as X
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
      LOGICAL, INTENT(IN) :: LT1 ! Flag for computing threshold 1
      REAL, INTENT(IN) :: TH1    ! Value for threshold 1
      REAL :: PR1                ! Probability of exceeding threshold 1
!
      LOGICAL, INTENT(IN) :: LT2 ! Flag for computing threshold 2
      REAL, INTENT(IN) :: TH2    ! Value for threshold 2
      REAL :: PR2                ! Probability of exceeding threshold 2
!
      INTEGER :: IERR            ! Output integer error flag
!
! *** Local variables
      REAL(KIND=8) :: VSUM ! Temporary sum variable
      INTEGER :: I     ! Temporary loop control
      INTEGER :: ICNT  ! Temporary counting variable
      INTEGER :: KFLAG ! Temporary sort type indicator
      INTEGER :: IP05  ! Temporary index variable
      INTEGER :: IP10  ! Temporary index variable
      INTEGER :: IP25  ! Temporary index variable
      INTEGER :: IP75  ! Temporary index variable
      INTEGER :: IP90  ! Temporary index variable
      INTEGER :: IP95  ! Temporary index variable
!
!---- First executable code --------------------------------------------
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
      IERR = 0
      IF( N .LT. 1 ) THEN
        IERR = 1
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
      IF( IERR .NE. 0 ) RETURN
!
! *** Set up indices for limits
!
      IP05 = 0.05*N
      IF( IP05 .LT. 1 ) IP05 = 1
      IP10 = 0.10*N
      IF( IP10 .LT. 1 ) IP10 = 1
      IP25 = 0.25*N
      IF( IP25 .LT. 1 ) IP25 = 1
      IP75 = 0.75*N
      IF( IP75 .LT. 1 ) IP75 = 1
      IP90 = 0.90*N
      IF( IP90 .LT. 1 ) IP90 = 1
      IP95 = 0.95*N
      IF( IP95 .LT. 1 ) IP95 = 1
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
      IF( N .GT. 1 ) THEN
        VSUM  = 0.0D0
        DO I = 1, N
          VSUM  = VSUM  + (UWORK(I)-XAVG)**2
        END DO
        XSTD = SQRT( VSUM/DBLE(N-1) )
      END IF
!
! *** Calculate the exceedance probabilities if desired
!
      IF( LT1 ) THEN
        ICNT = 0
        DO I = 1, N
          IF( UWORK(I) .GT. TH1 ) ICNT = ICNT + 1
        END DO
        PR1 = FLOAT(ICNT) / FLOAT(N)
      ELSE
        PR1 = 0.0
      END IF
!
      IF( LT2 ) THEN
        ICNT = 0
        DO I = 1, N
          IF( UWORK(I) .GT. TH2 ) ICNT = ICNT + 1
        END DO
        PR2 = FLOAT(ICNT) / FLOAT(N)
      ELSE
        PR2 = 0.0
      END IF
!
! *** Normal exit
!
      RETURN
      END SUBROUTINE USTAT_ECEM

      SUBROUTINE WRITE_BUGS( ILOC, LOCID, ISPC, SPECIEID, IANA, ANALYTEID, RESULT_TYPE, UNITS, NREAL, WORK )
!!**********************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes out detailed information to the report file
!!    for purposes of debugging calculations
!!
!!  Call List Variables:
!!
!!    ILOC        : Location index
!!    LOCID       : Location ID
!!    ISPC        : Species index
!!    SPECEID     : Species ID
!!    IANA        : Analyte index
!!    ANALYTEID   : Analyte ID
!!    RESULT_TYPE : Character result type
!!    NREAL       : Number of realizations
!!    CWORK       : Output vector of results
!!
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Dec 1997 : Version 1.0
!!    Paul W. Eslinger : 19 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 20 Jan 2004 : Change output format
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1140) Add output units
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************************************************
!
! *** Global variables
      USE Files_Mod, ONLY: IRPT
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ILOC     ! Location index
      CHARACTER(LEN=*) :: LOCID       ! Location ID - derived from variable ESD_LOC(*)%ID
!
      INTEGER, INTENT(IN) :: ISPC     ! Species index
      CHARACTER(LEN=*) :: SPECIEID    ! Species ID - derived from variable ESD_SPC(*)%ID
!
      INTEGER, INTENT(IN) :: IANA     ! Analyte index
      CHARACTER(LEN=*) :: ANALYTEID   ! Analyte ID - derived from variable ESD_ANA(*)%ID
!
      CHARACTER(LEN=*) :: RESULT_TYPE ! Result type ID (6 characters)
      CHARACTER(LEN=*) :: UNITS       ! Units label
!
      INTEGER, INTENT(IN) :: NREAL    ! Number of realizations
      REAL, DIMENSION(*) :: WORK      ! Vector of results
!
! *** Local variables
      INTEGER :: IREL ! Realization looping index
!
!---- Executable code --------------------------------------------
!
      WRITE(IRPT,1000)  ILOC, LOCID, ISPC, SPECIEID, IANA, ANALYTEID, RESULT_TYPE, UNITS
 1000 FORMAT(/'Computed values'/ &
        '---------------------   -----------------'/ &
        I4,' : Location index : ',A/ &
        I4,' : Species index  : ',A/ &
        I4,' : Analyte index  : ',A/ &
        4X,'   Result type    : ',A/ &
        4X,'   Result Units   : ',A)
!
      WRITE(IRPT,1010)
 1010 FORMAT(/'Index Result'/'----- ------------')
!
      DO IREL = 1, NREAL
        WRITE(IRPT,1020) IREL, WORK(IREL)
 1020   FORMAT(I5,1X,1P,E12.5)
      END DO
!
      RETURN
      END SUBROUTINE WRITE_BUGS

      SUBROUTINE WRITE_DETAILS( TIME, LOC_ID, SOI_ID, ANA_ID, SPC_ID, S_TYPE, UNITS, RISK )
!!*****************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes detailed risk values to the details file
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Feb 2002 : Add units output
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Aug 2004 : Change output format
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger : 13 Nov 2013 : Change output format for more realizations
!!
!!*****************************************************************************************
!
! *** Global variables
      USE Files_Mod, ONLY: IDET
      USE Control_Mod, ONLY: NREAL
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: TIME ! Time (celndar year) for this result
      CHARACTER(LEN=*), INTENT(IN) :: LOC_ID ! Location ID
      CHARACTER(LEN=*), INTENT(IN) :: SOI_ID ! Soil type indicator
      CHARACTER(LEN=*), INTENT(IN) :: ANA_ID ! Analyte ID
      CHARACTER(LEN=*), INTENT(IN) :: SPC_ID ! Species ID
      CHARACTER(LEN=*), INTENT(IN) :: S_TYPE ! Solution type
      CHARACTER(LEN=*), INTENT(IN) :: UNITS  ! Data units
      REAL, INTENT(IN), DIMENSION(*) :: RISK ! Vector of risk values
!
! *** Local variables
      INTEGER :: I ! Looping index
!
!---- First executable code --------------------------------------------
!
      WRITE(IDET,1000) TIME, TRIM(LOC_ID), TRIM(SOI_ID), TRIM(ANA_ID), &
        TRIM(SPC_ID), S_TYPE, TRIM(UNITS), (RISK(I),I=1,NREAL)
 1000 FORMAT(I0,',"',A,'","',A,'","',A,'","',A,'","',A,'","',A,'"',1P,2500(:,',',E12.5))
!
      RETURN
      END SUBROUTINE WRITE_DETAILS

      SUBROUTINE WRITE_STATS( TIME, LOCID, SOILID, SPECIEID, ANALYTEID, RESULT_TYPE, UNITS, &
        XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD )
!!*****************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes out summary statistical information to the summary statistics file.
!!
!!  Call List Variables:
!!
!!    ISUM        : File unit number for the summary statistics file
!!    TIME        : Time (calenday year)
!!    LOCID       : Character location ID - derived from variable ESD_LOC(*)%ID
!!    SOILID      : Character soil ID - type of soil used at the location
!!    SPECEID     : Character species ID - derived from variable ESD_SPC(*)%ID
!!    ANALYTEID   : Character analyte ID - derived from variable ESD_ANA(*)%ID
!!    RESULT_TYPE : Character analyte ID - derived from variable ESD_ANA(*)%ID
!!                  The following results from USTAT
!!    XMIN        : Minimum value
!!    XV05        : Value at the  5% level
!!    XV10        : Value at the 10% level
!!    XV25        : Value at the 25% level
!!    XMED        : Value at the 50% level (median)
!!    XV75        : Value at the 75% level
!!    XV90        : Value at the 90% level
!!    XV95        : Value at the 95% level
!!    XMAX        : Maximum value
!!    XAVG        : Average value
!!    XSTD        : Standard deviation of the values
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Dec 1997 : Version 1.0
!!    Paul W. Eslinger : 17 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Feb 2002 : Add units to the output
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!*****************************************************************************************************
!
! *** Global variables
      USE Files_Mod, ONLY: ISUM
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: TIME
      CHARACTER(LEN=*) :: LOCID, SPECIEID, SOILID, ANALYTEID, RESULT_TYPE, UNITS
      REAL :: XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD
!
!---- Executable code --------------------------------------------
!
      WRITE(ISUM,1010) TIME, TRIM(LOCID), TRIM(SOILID), TRIM(ANALYTEID), TRIM(SPECIEID), &
        TRIM(RESULT_TYPE), TRIM(UNITS), &
        XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD
 1010 FORMAT(I0,',"',A,'","',A,'","',A,'","',A,'","',A,'","',A,'"',1P,11(:,',',E12.5))
!
      RETURN
      END SUBROUTINE WRITE_STATS

      SUBROUTINE WRITE_THRESH( TIME, LOCID, SPECIEID, ANALYTEID, RESULT_TYPE, RESULT_UNITS, TH1, PR1, TH2, PR2  )
!!***************************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes out threshold information to the threshold file.
!!
!!  Call List Variables:
!!
!!    TIME         : Time (calendar year)
!!    LOCID        : Character location ID - derived from variable ESD_LOC(*)%ID
!!    SPECEID      : Character species ID - derived from variable ESD_SPC(*)%ID
!!    ANALYTEID    : Character analyte ID - derived from variable ESD_ANA(*)%ID
!!    RESULT_TYPE  : Character analyte ID - derived from variable ESD_ANA(*)%ID
!!    RESULT_UNITS : Units for the retult type
!!
!!                   The following results typically are computed by USTAT_ECEM
!!    TH1          : Threshold 1 value
!!    TH2          : Threshold 2 value
!!    PR1          : Exceedance probability for threshold 1
!!    PR2          : Exceedance probability for threshold 2
!!
!!  History:
!!
!!    Paul W. Eslinger : 17 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 10 Nov 2006 : (SCR-1149) Add units for result type
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!***************************************************************************************************************
!
! *** Global variables
      USE Files_Mod, ONLY: ITHS
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: LOCID, SPECIEID, ANALYTEID, RESULT_TYPE, RESULT_UNITS
      INTEGER, INTENT(IN) :: TIME
      REAL, INTENT(IN) :: TH1, PR1, TH2, PR2
!
!---- Executable code --------------------------------------------
!
      WRITE(ITHS,1010) TIME, LOCID, SPECIEID, ANALYTEID, RESULT_TYPE, RESULT_UNITS, TH1, PR1, TH2, PR2
 1010 FORMAT(I0,',"',A,'","',A,'","',A,'","',A,'","',A,'"',2(',',1P,E12.5,',',0P,F6.3))
!
      RETURN
      END SUBROUTINE WRITE_THRESH
