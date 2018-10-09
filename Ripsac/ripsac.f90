!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
MODULE Control_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History:
!
!    Paul W. Eslinger : 29 Apr 2000 : Version 1.0
!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!    Carmen Arimescu  : 11 Nov 2002 : SAC Rev. 1
!
      CHARACTER(LEN=200) :: PTITLE ! Title for this program run
      INTEGER :: NREAL ! Number of realizations
      LOGICAL :: DEBUG ! Flag for RIPSAC debug writes to the report file
!
      LOGICAL :: REPORT ! Flag whether the report file is open
      LOGICAL :: EXECUT ! Flag whether problem is to be executed (.false.=error check only)
!
      INTEGER :: RIP_NUM_ANA ! Number of analytes in this scenario
!
      LOGICAL :: NEED_SEEP ! Flag whether seep calculations will be performed
      LOGICAL :: NEED_SORP ! Flag whether soil calculations will be performed
!
      LOGICAL, ALLOCATABLE :: REL_USE(:) ! Realization use logical vector
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Dilute_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to Dilute Data (modified from ECDA)
!
!  History:
!
!    Paul W. Eslinger : 29 Apr 2000 : Version 1.0
!    Paul W. Eslinger : 11 Nov 2002 : SAC Rev. 1
!
      CHARACTER(LEN=  8) :: DF_TYPE   ! The character string DILUTE
      CHARACTER(LEN=200) :: DF_PTITLE ! Problem title line from generating program
      CHARACTER(LEN= 10) :: DF_PRGNAM ! Program name of generating program
      CHARACTER(LEN=  8) :: DF_PRGVER ! Program version number of generating program
      CHARACTER(LEN= 12) :: DF_PRGDAT ! Program date of generating program
      CHARACTER(LEN= 16) :: DF_USRNAM ! User name from generating program
      CHARACTER(LEN= 14) :: DF_CRUNID ! Run identification number from generating program
      CHARACTER(LEN= 24) :: DF_ID     ! Dilution factor IDs for the calculations
      CHARACTER(LEN=20), ALLOCATABLE :: DF_UNITS(:) ! Dilution factor UNITS by impact location
      INTEGER :: DF_NUM               ! Number of dilution factor stochastic variables
      INTEGER :: DF_NITR              ! Number of DILUTE realizations
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE ESD_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information from the ESD file that will be
!    used in the RIPSAC code, in addition, it contains some information
!    defined in this code that depends on location, time, or analyte.
!
!  History:
!
!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!    Paul W. Eslinger : 11 Nov 2002 : SAC Rev. 1
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
      TYPE (ESD_TIM_TYPE), ALLOCATABLE :: ESD_TIM(:) ! The ESD time variable
!
!     Type definition for ESD location data
!
      TYPE ESD_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        LOGICAL :: SEEP           ! Flag for seep concentrations
        LOGICAL :: SORP           ! Flag for soil concentrations
        INTEGER :: SECOND         ! Index for the second choice of concentrations
      END TYPE ESD_LOC_TYPE
      TYPE (ESD_LOC_TYPE), ALLOCATABLE :: ESD_LOC(:) ! The ESD location variable
!
!     Type definition for ESD analyte data
!
      TYPE ESD_ANA_TYPE ! Type definition for ESD analyte data
        CHARACTER(LEN= 6) :: ID   ! Identification number for an analyte
        CHARACTER(LEN=72) :: NAME ! Descriptive name for an analyte
        CHARACTER(LEN=2)  :: TYPE ! Analyte type
        LOGICAL :: COMP           ! Flag whether this analyte is to be used
      END TYPE ESD_ANA_TYPE
      TYPE (ESD_ANA_TYPE), ALLOCATABLE :: ESD_ANA(:) ! The ESD analyte variable
!
!     Distribution coefficient map by location, and analyte (length of VLABEL)
      CHARACTER(LEN=24), ALLOCATABLE :: KD_MAP(:,:)
!     Distribution coefficient by location, analyte, and realization
      REAL, ALLOCATABLE :: KD(:,:,:)
!
!     Dilution factor map by location (length of VLABEL)
      CHARACTER(LEN=24), ALLOCATABLE :: DF_MAP(:)
!     Dilution factor by location and realization
      REAL, ALLOCATABLE :: DF(:,:)
!
      REAL, ALLOCATABLE :: CVEC_GWAT(:) ! Temporary groundwater concentration work vector
      REAL, ALLOCATABLE :: CVEC_SWAT(:) ! Temporary surface water concentration work vector
      REAL, ALLOCATABLE :: CVEC_SEEP(:) ! Temporary seep water concentration work vector
      REAL, ALLOCATABLE :: CVEC_SORP(:) ! Temporary soil work concentration vector
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Files_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains file names and unit numbers for the
!    input and output files.
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Carmen Arimescu  : 15 Mar 2000
!    Carmen Arimescu  : 11 Nov 2002 : SAC Rev. 1
!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
!
!  Code Block:
!
      INTEGER, PARAMETER :: MAXFN=200 ! Set equal to LENQQQ in RDBLKD.INS??
      CHARACTER(LEN=MAXFN) :: FNTMP  ! Temporary file name
!
      INTEGER :: IRPT        ! Unit number for the report file
      CHARACTER(LEN=MAXFN) :: FNRPT  ! Name of the report file
!
      INTEGER :: IKEY        ! Unit number for the input keyword file
      CHARACTER(LEN=MAXFN) :: FNKEY  ! Name of the input keyword file
      CHARACTER(LEN=MAXFN) :: PATH_KEY ! Path of the input keyword file name
!
      INTEGER :: UN_ECDAMAP  ! Unit number for the ECDA map file
!
      CHARACTER(LEN=MAXFN) :: FN_MAP ! File name for record map
!
      INTEGER :: IESD ! Unit number for the ESD keyword file
      CHARACTER(LEN=MAXFN) :: FN_ESD ! Name of the input ESD keyword file
!
      INTEGER :: IKDS ! Unit number for the KdSoil data file
      CHARACTER(LEN=MAXFN) :: FN_KDSOIL ! File name for KdSoil data
!
      INTEGER :: IDIL ! Unit number for the Dilute data file
      CHARACTER(LEN=MAXFN) :: FN_DILUTE ! File name for Dilute data
!
!      INTEGER :: IDON ! Unit number for the "Done" file
!      CHARACTER(LEN=MAXFN) :: FN_DONE ! File name for the "Done" file
!
!      INTEGER :: IRUN ! Unit number for the "Run" file
!      CHARACTER(LEN=MAXFN) :: FN_RUN ! File name for the "Run" file
!
      INTEGER :: ICON ! Unit number for the concentration files (only one open at a time)
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FN_CON(:) ! File name for concentrations
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Iden_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Kdsoil_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information related to the KD (soil-water
!    distribution coefficient) data file (modified from ECDA)
!
!  History:
!
!    Paul W. Eslinger : 28 Apr 2000 : Version 1.0
!    Paul W. Eslinger : 11 Nov 2002 : SAC Rev. 1
!
      CHARACTER(LEN=  8) :: KD_TYPE   ! The character string KDSOIL
      CHARACTER(LEN=200) :: KD_PTITLE ! Program modification date from KD generator
      CHARACTER(LEN= 10) :: KD_PRGNAM ! Program name from KD generator
      CHARACTER(LEN=  8) :: KD_PRGVER ! Program version number from KD generator
      CHARACTER(LEN= 12) :: KD_PRGDAT ! Program date from KD generator
      CHARACTER(LEN= 14) :: KD_CRUNID ! Run identification number from KD generator
      CHARACTER(LEN= 16) :: KD_USRNAM ! User name from KD generator
      CHARACTER(LEN= 24) :: KD_ID     ! KD stochastic ID
      CHARACTER(LEN=20), ALLOCATABLE :: KD_UNITS(:) ! KD factor UNITS by location 
      INTEGER :: KD_NUM               ! Number of KD stochastic variables
      INTEGER :: KD_NITR              ! Number of KD stochastic iterations
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

      PROGRAM RipSac
!!***********************************************************************************************************
!!
!!                             RipSac - Stochastic Riparian Zone Analysis
!!                        Toolkit for Integrated Impacts Assessments (TIIA)
!!
!!                Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!***********************************************************************************************************
!!
!!  RipSac is the top level routine for the riparian zone environment of the TIAA, Version 1.  This code
!!  calculates stochastic concentrations in seep water and soil based on concentrations in groundwater
!!  and surface water.
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!***********************************************************************************************************
!!
!!  Module History:
!!    Paul W. Eslinger : 15 Jun 2000 : Version 1.0
!!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!!    Paul W. Eslinger : 14 Aug 2001 : Error message on negative data
!!    Carmen Arimescu  : 11 Nov 2002 : SAC Rev. 1
!!    Carmen Arimescu  : 10 Jan 2003 : SAC Rev. 1 - Rdblk routines
!!    Paul W. Eslinger :  6 Jun 2007 : Update for TIIA logic and copyright information
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
!!
!!***********************************************************************************************************
!!
!!  General Notes:
!!    1. This code applies to all locations.  It also handles multiple contaminants.
!!    2. This code provides stochastic results.  It also provides deterministic results in that a run of
!!       one realization is allowed.
!!    3. This program was written in Fortran 95 (free source form).
!!    4. This program is a batch-type program.  The sole input to start a program run is the name of a
!!       simulation control keyword file.
!!    5. Even though RipSac runs in a stand-alone mode, previous programs are used to set up input files
!!       and define the concentrations of analytes in the groundwater and surface water.
!!       groundwater concentrations.
!!
!!***********************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Iden_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod
      USE Esd_Mod
      USE Ecda_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'RipSac' ! Name of this routine
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
! *** Open the RipSac keyword file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the keyword file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
!
! *** Make sure there is no "done" file
!      INQUIRE(FILE=FN_DONE,EXIST=THERE)
!      IF( THERE ) THEN
!        OPEN(IDON,FILE=FN_DONE)
!        CLOSE(IDON,STATUS='DELETE')
!      END IF
!
! *** Open the run signal file
!      OPEN(IRUN,FILE=FN_RUN)
!
! *** Define the report file
!     Special case exit if can't open the report file
      CALL TELLTIME( 'Extracting the report file name', 'SCREEN', .TRUE., IRPT )
      CALL READ_KEYS_REPORT( IERR )
      IF( IERR .NE. 0 ) THEN
!        CLOSE(IRUN,STATUS='DELETE')
!        OPEN(IDON,FILE=FN_DONE)
!        WRITE(IDON,*) IERR, 'Error termination'
!        CLOSE(IDON)
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP 'Abnormal Run Termination Due to Errors'
      END IF
!
! *** Write the initial portion of the banner page
      CALL BANNER_1( )
!
! *** Read the first pass of the RipSac control keywords
      CALL TELLTIME( 'Reading RIPSAC Keywords - First Pass', 'SCREEN', .FALSE., IRPT )
      CALL KEY_RIPSAC_1( IERR )
      IF( IERR .NE. 0 ) THEN
        IF( .NOT. REPORT ) THEN
          WRITE(*,*) 'Error opening the report file'
          WRITE(*,*) 'Stop in '//CALLER
        END IF
        GO TO 9999
      END IF
!
! *** Open the environmental settings definition keyword file
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
      CALL TELLTIME( 'Allocating ESD Memory', 'SCREEN', .FALSE., IRPT )
      CALL ESD_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Initialize environmental settings data and the "use" information
      CALL ESD_INIT(  )
!
! *** Read the second pass of environmental keywords
!     Save time, analyte, location information and concentration file names
      CALL TELLTIME( 'Reading ESD Keywords - Second Pass', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_2( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Read the rest of the RipSac control keywords
      CALL TELLTIME( 'Reading RIPSAC Keywords - Second Pass', 'SCREEN', .FALSE., IRPT )
      CALL KEY_RIPSAC_2( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Finish the banner page
      CALL BANNER_2( )
!
! *** Perform error checking on keywords and the problem definition
      CALL CHECK_RIPSAC( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Echo the problem definition to the report file
      CALL ECHO( )
!
!-------------------------------------------------------------------------------------
!     Start execution of the problem
!-------------------------------------------------------------------------------------
!
      IF( .NOT. EXECUT ) THEN
        MESSAG(1) = 'Execution not requested'
        MESSAG(2) = 'Use the EXECUTE card'
        CALL PRTERR( 0, CALLER, 2 )
        GO TO 9999
      END IF
!
! *** Open and read the file containing the KDSOIL stochastic data if needed
      IF( NEED_SEEP .OR. NEED_SORP ) THEN
        CALL OPEN_KDSOIL( IERR )
        IF( IERR .NE. 0 ) GO TO 9999
      END IF
!
! *** Open and read the file containing the DILUTE stochastic data if needed
      IF( NEED_SEEP ) THEN
        CALL OPEN_DILUTE( IERR )
        IF( IERR .NE. 0 ) GO TO 9999
      END IF
!
! *** ==> Process impacts
!
! *** Get the record number map for the concentration data
!     The same map applies for all analytes
      CALL TELLTIME( 'Processing impacts', 'SCREEN', .FALSE., IRPT )
      CALL ECDA_MAPREAD( FN_MAP, UN_ECDAMAP, IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Echo the header lines from the map file to the reporit file
      IF( DEBUG ) CALL ECDA_MAPECHO( IRPT )
!
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE ! Only process the requested analytes
        CALL TELLTIME( 'Processing impacts for ' // ESD_ANA(IANA)%ID, 'SCREEN', .FALSE., IRPT )
        CALL PROCESS_IMPACTS( IANA, IERR )
        IF( IERR .NE. 0 ) GO TO 9999
      END DO
!
 9999 CONTINUE
!
! *** Write the "Done" file
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
      CALL DATE_AND_TIME( SDATE, STIME )
!
      IF( IERR .NE. 0 ) THEN
        IF( REPORT ) THEN
          MESSAG(1) = 'Abnormal Run Termination Due to Errors'
          CALL PRTERR( IERR, CALLER, 1)
          WRITE(IRPT,'(9X,A)') 'Run Completed on '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//&
            ' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
          CALL TELLTIME( 'Abnormal Run Termination Due to Errors', 'SCREEN', .TRUE., IRPT )
          WRITE(*,*) 'Abnormal Run Termination Due to Errors'
          STOP
        ELSE
          WRITE(*,*) 'Error encountered in a lower-level routine.'
          WRITE(*,*) 'Execution halted because of the above errors.'
          WRITE(*,*) 'Program stop in ' // TRIM(CALLER)
          WRITE(*,*) 'Abnormal Run Termination Due to Errors'
          STOP
        END IF
      ELSE
        MESSAG(1) = 'Normal Termination'
        CALL PRTERR( IERR, CALLER, 1)
        WRITE(IRPT,'(9X,A)') 'Run Completed on '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//&
          ' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
        CALL TELLTIME( 'Normal Termination', 'SCREEN', .TRUE., IRPT )
        WRITE(*,*) 'Normal Termination'
        STOP
      END IF
!
      END PROGRAM

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.
!!
!!  History:
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Revise for TIIA and copyright
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
      WRITE(IRPT,1010) 'RRRRR    IIIIIII  PPPPP      SSS     AAAAA     CCC  '
      WRITE(IRPT,1010) 'R    RR     I     P    PP  S    SS  A     A   C   CC'
      WRITE(IRPT,1010) 'R    RR     I     P    PP   S    S  A     A  C      '
      WRITE(IRPT,1010) 'RRRRR       I     PPPPP      SS     AAAAAAA  C      '
      WRITE(IRPT,1010) 'R    R      I     P        S   S    A     A  C      '
      WRITE(IRPT,1010) 'R     R     I     P        SS   S   A     A   C   CC'
      WRITE(IRPT,1010) 'R     R  IIIIIII  P          SSS    A     A    CCC  '
 1010 FORMAT(13X,A)
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
!!    This subroutine prints banner information to the report file.
!!
!!  History:
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global valriables
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

      SUBROUTINE CHECK_RIPSAC( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the RIPSAC keyword
!!    cards looking for problem definition problems.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!!    Carmen Arimescu  : 11 Nov 2002 : Take off the Impacts
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
      CHARACTER(LEN=12) :: CALLER = 'CHECK_RIPSAC' ! Name of this subroutine
!
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ILOC ! Location looping index
      INTEGER :: ICNT ! Realization counting variable
      INTEGER :: IREL ! Realization looping variable
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
        MESSAG(2) = 'Use the USER keyword in the RIPSAC keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The problem title is missing'
        MESSAG(2) = 'Use the TITLE keyword in the RIPSAC keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** File name errors
!
      IF( (NEED_SEEP.OR.NEED_SORP) .AND. FN_KDSOIL .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The input KD data file name is missing'
        MESSAG(2) = 'Use the KDSOIL modifier on the ESD FILE Keyword'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      IF( NEED_SEEP .AND. FN_DILUTE .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The input dilution data file name is missing'
        MESSAG(2) = 'Use the DILUTE modifier on the ESD FILE Keyword'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check for all concentration files
!
      DO IANA = 1, ESD_NUM_ANA
!        WRITE(*,*) ESD_ANA(IANA)%ID, ESD_ANA(IANA)%COMP
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        IF( FN_CON(IANA) .EQ. ' ' ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'The input ECDA concentration file name is missing'
          MESSAG(2) = 'Analyte ID = '//TRIM(ESD_ANA(IANA)%ID)
          MESSAG(3) = 'Check the FILE keyword in the ESD keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END DO
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
! *** Analyte-related errors
!
      IF( RIP_NUM_ANA .EQ. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one analyte must be requested'
        MESSAG(2) = 'Modify the ANALYTE keyword in the RIPSAC keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** KDSOIL and DILUTE mappings

!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.(ESD_LOC(ILOC)%SEEP .OR. ESD_LOC(ILOC)%SORP) ) CYCLE
!
        IF( DF_MAP(ILOC) .EQ. ' ' ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'DF_MAP data missing'
          MESSAG(2) = 'Location = ' // ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Modify the DILUTE keyword or the LOCATION keyword'
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
            MESSAG(4) = 'Modify the KDSOIL keyword or the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 4 )
          END IF
        END DO
      END DO
!
! *** Check on the number of activated realizations
!
      ICNT = 0
      DO IREL = 1, NREAL
        IF( REL_USE(IREL) ) ICNT = ICNT + 1
      END DO
      IF( ICNT .EQ. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one realization is required in RIPSAC'
        MESSAG(2) = 'Modify the REALIZATION keyword in the RIPSAC keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Ending message if errors encountered
!
      IF( IERR .GT. 0 ) THEN
        MESSAG(1) = 'Ending consistency scan of inputs with III errors.'
        WRITE(MESSAG(1)(40:42),'(I3)') IERR
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
!!    Paul W. Eslinger :  6 Jun 2007 : New source
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
      SUBROUTINE ECHO(  )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the problem definition
!!    to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 23 May 2000 : Version 1.0
!!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
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
      INTEGER :: ILOC ! Location looping index
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ITIM ! Time looping index
      INTEGER :: ICNT ! Realization counting variable
      INTEGER :: IREL ! Realization looping variable
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
 1020 FORMAT(I0,1X,A)
      ICNT = 0
      DO IREL = 1, NREAL
        IF( REL_USE(IREL) ) ICNT = ICNT + 1
      END DO
      WRITE(IRPT,1020) ICNT, 'Realizations utilized in this run'
!
      WRITE(IRPT,1025)
 1025 FORMAT(/'A map of realization activations (true/false) is:')
      NLINE = 20
      NTIME = NREAL / NLINE
      NOVER = NREAL - NLINE*NTIME
      I2 = 0
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
! *** Files names used in the analysis
!
      WRITE(IRPT,1030) 'File Name for RIPSAC Input Keyword Data', TRIM(FNKEY)
      WRITE(IRPT,1030) 'File Name for ESD Input Keyword Data', TRIM(FN_ESD)
      IF( NEED_SEEP  ) WRITE(IRPT,1030) 'File Name for Dilution Data', TRIM(FN_DILUTE)
      IF( NEED_SEEP .OR. NEED_SORP ) WRITE(IRPT,1030) 'File Name for KDSOIL Data', TRIM(FN_KDSOIL)
 1030 FORMAT(/A/'File: ',A)
!
! *** Concentration map and data files for each analyte
!
      WRITE(IRPT,1045) TRIM(FN_MAP)
 1045 FORMAT(/'File Name for Concentration Index Map File'/'File: ',A)
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
 1060     FORMAT(3X,I2,' : ',A,' : ',A)
        ELSE
          WRITE(IRPT,1060) IANA, ESD_ANA(IANA)%ID, '===> Not used'
        END IF
      END DO
      WRITE(IRPT,1070) RIP_NUM_ANA
 1070 FORMAT('A total of ',I2,' analytes have been requested.')
!
! *** Output the set of time definitions
!
      WRITE(IRPT,*) ' '
      WRITE(IRPT,1300) ESD_NUM_TIM
 1300 FORMAT('Number of times is ',I0)
      WRITE(IRPT,'(A)') 'Index    Year'
      DO ITIM = 1, ESD_NUM_TIM
        WRITE(IRPT,1302) ITIM, ESD_TIM(ITIM)%TIME
 1302   FORMAT(I6,1X,I0)
      END DO
!
! *** Output the set of location definitions
!     Skip output indices where no solutions are requested
!
      WRITE(IRPT,*) ' '
      WRITE(IRPT,1320) ESD_NUM_LOC
 1320 FORMAT('Number of locations is ',I0)
      WRITE(IRPT,'(A)') 'Index   Location ID'
      DO ILOC = 1, ESD_NUM_LOC
        WRITE(IRPT,1304) ILOC, ESD_LOC(ILOC)%ID
 1304   FORMAT(I6,1X,A)
      END DO
!
! *** KDSOIL and DILUTE mappings
!
      WRITE(IRPT,1180)
 1180 FORMAT(/'Soil-Water KD and Dilution Factor Mapping'/&
        '  Location      Analyte      KDSOIL ID String          DILUTE ID String'/&
        '  ------------  -----------  --------------------      --------------------')
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.(ESD_LOC(ILOC)%SEEP .OR. ESD_LOC(ILOC)%SORP) ) CYCLE
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
          WRITE(IRPT,1190) ILOC, ESD_LOC(ILOC)%ID, IANA, ESD_ANA(IANA)%ID, KD_MAP(ILOC,IANA), &
            DF_MAP(ILOC)
 1190     FORMAT(2X,I4,': ',A,2X,I3,': ',A,2X,A,2X,A)
        END DO
      END DO
!
! *** End of problem definition
!
      WRITE(IRPT,1200)
 1200 FORMAT(/24('='),' End of the Problem Definition ',25('='))
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
!!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!!    Carmen Arimescu  : 11 Nov 2002 : Remove different impact types
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no match
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'ESD_INIT' ! Name of this subroutine
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
        ESD_LOC(I)%SEEP = .FALSE.
        ESD_LOC(I)%SORP = .FALSE.
        ESD_LOC(I)%SECOND = 0
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
      ICON = 15
      DO I = 1, ESD_NUM_ANA
        ESD_ANA(I)%ID   = ' '
        ESD_ANA(I)%NAME = ' '
        ESD_ANA(I)%COMP = .FALSE.
        FN_CON(I)       = ' '
     END DO
!
! *** Map and data for Soil Kd's and dilution factors
!
      DO ILOC = 1, ESD_NUM_LOC
        DF_MAP(ILOC) = ' '
        DO IREL = 1, NREAL
          DF(ILOC,IREL) = -1.0
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
!!    Paul W. Eslinger : 25 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!!    Carmen Arimescu  : 11 Nov 2002 : Impacts, Media
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE ESD_Mod
      USE Control_Mod
      USE Kdsoil_Mod
      USE Dilute_Mod
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
! *** Allocate for the number of locations
!
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( ESD_LOC(ESD_NUM_LOC), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 4
          MESSAG(1) = 'Error allocating memory for ESD_LOC'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( DF_MAP(ESD_NUM_LOC), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 5
          MESSAG(1) = 'Error allocating memory for DF_MAP'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
! *** Allocation depending on locations and analytes
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( KD_MAP(ESD_NUM_LOC,ESD_NUM_ANA), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 6
          MESSAG(1) = 'Error allocating memory for KD_MAP'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
! *** Allocate on the number of times
!
      IF( ESD_NUM_TIM .GT. 0 ) THEN
        ALLOCATE( ESD_TIM(ESD_NUM_TIM), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 7
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
        IERR = 8
        MESSAG(1) = 'At least 1 ESD realization required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( KD(ESD_NUM_LOC,ESD_NUM_ANA,ESD_NREAL), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 9
          MESSAG(1) = 'Error allocating memory for KD'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( DF(ESD_NUM_LOC,ESD_NREAL), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 10
          MESSAG(1) = 'Error allocating memory for DF'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!      
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( DF_UNITS(ESD_NUM_LOC), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 11
          MESSAG(1) = 'Error allocating memory for DF_UNITS'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( KD_UNITS(ESD_NUM_LOC), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 12
          MESSAG(1) = 'Error allocating memory for KD_UNITS'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
! *** Allocate work space
!
      ALLOCATE( CVEC_GWAT(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error allocating memory for CVEC_GWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CVEC_SWAT(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 14
        MESSAG(1) = 'Error allocating memory for CVEC_SWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CVEC_SEEP(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating memory for CVEC_SEEP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CVEC_SORP(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error allocating memory for CVEC_SORP'
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
        IERR = 17
        MESSAG(1) = 'Error allocating memory for REL_USE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE GET_INDEX( ITIM, ILOC, IMED, IDX, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine gets the record number in an ECDA concentration
!!    file associated with:
!!      time index, ITIM
!!      location index, ILOC
!!      media index, IMED
!!    The index is output in the variable IDX
!!
!!  History:
!!
!!    Paul W. Eslinger : 17 Apr 2000 : Version 1.0
!!    Carmen Arimescu  : 11 Nov 2002
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
      INTEGER :: IDX  ! Output record number index
      INTEGER :: IERR ! Error number variable
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'GET_INDEX' ! Name of this subroutie
      CHARACTER(LEN=6) :: LOC_ID, LOC_N2 ! ID's for primary and second location
      INTEGER :: N2                      ! Number of the second location
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      LOC_ID = ESD_LOC(ILOC)%ID
      N2 = ESD_LOC(ILOC)%SECOND
      IF( N2 .GT. 0 ) LOC_N2 = ESD_LOC(N2)%ID
!
! *** Get the primary location index
!
      CALL ECDA_RECNO_INDEX( ITIM, ILOC, IMED, IDX, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine for primary index'
        MESSAG(2) = 'Location ID is ' // LOC_ID
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
      IF( N2 .GT. 0 ) THEN
        CALL ECDA_RECNO_INDEX( ITIM, N2, IMED, IDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine for secondary index'
          MESSAG(2) = 'Location ID is '// LOC_N2
          MESSAG(3) = 'Time index is '
          WRITE(MESSAG(3)(16:),*) ITIM
          MESSAG(4) = 'Media index is '
          WRITE(MESSAG(4)(17:),*) IMED
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
!        WRITE(*,*) n2, LOC_N2, idx, ' media = ',IMED
        IF( IDX .GT. 0 ) RETURN
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
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Last version update
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
      PRGNAM = 'RipSac'
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
      SUBROUTINE INIT(  )
!!****************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes global variables
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Jun 2000 : Version 1.0
!!    Carmen Arimescu  : 11 Nov 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
!!
!!****************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE KdSoil_Mod
      USE Dilute_Mod
      USE Esd_Mod
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code ---------------------------------------------------
!
! *** Keyword file
      IKEY  = 7
      FNKEY = ' '
      PATH_KEY = ' '
!
! *** Report file
      IRPT  = 8
      FNRPT = ' '
      IRPT_ERR = IRPT
      REPORT = .FALSE.
!
! *** ECDA map file
      UN_ECDAMAP  = 9
!
! *** Define the "Done" file name without the path
!     The path for this file will be added in routine OPEN_ESD
!      IDON = 10
!      IRUN = 11
!      FN_DONE = TRIM(PRGNAM)//'.Done'
!      FN_RUN  = TRIM(PRGNAM)//'.Run'
!
! *** Error message file
      IRPT_ERR = IRPT
!
! *** KDSOIL variables
      IKDS = 12
      FN_KDSOIL = ' '
!
! *** DILUTE variables
      IDIL = 13
      FN_DILUTE = ' '
!
! *** ESD keyword input file
      IESD  = 14
      FN_ESD = ' '
!
! *** Problem title
      PTITLE = ' '
!
! *** Debug and execute flags
      DEBUG  = .FALSE.
      EXECUT = .FALSE.
!
! *** Initialize some file names
      FN_MAP = ' '
!
! *** Initialize counters
      ESD_NUM_LOC = 0
      ESD_NUM_TIM = 0
!
! *** Initialize compute flage
!
      NEED_SEEP = .FALSE.
      NEED_SORP = .FALSE.
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
!!    Paul W. Eslinger : 25 Apr 2000 : Version 2.0
!!    Carmen Arimescu  : 11 Nov 2002
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
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_1' ! Name of this routine
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
          NREAL = ESD_NREAL
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
!!    Paul W. Eslinger : 15 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  1 May 2001 : Ignore concentration data files
!!                                     for undefined analytes.
!!    Carmen Arimescu  : 11 Nov 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments and error return
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
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_2' ! Name of this routine
      INTEGER :: IDX, ITMP, IDXA  ! Temporary index variables
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
!
!     Temporary character strings (match length of quote string from RDBLK)
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_ANA, TMP_NAME
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
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
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IESD )
          RETURN
!
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('C_ECDA') ) THEN
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
            IF( CEXIST('ANALYTE ') ) THEN
              CALL NXTQOT( IDX, TMP_ANA )
              CALL MATCH_ANA( TMP_ANA, IDXA )
              IF( IDXA .GT. 0 ) THEN
                FN_CON(IDXA) = TMP_ID
              END IF
            ELSE
              IERR = 9
              MESSAG(1) = 'ANALYTE modifier not found on FILE card for type C_ECDA'
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
                IERR = 10
                MESSAG(1) = 'NAME modifier missing quote string on FILE (I_ECDA) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              ELSE
                FN_MAP = TMP_ID
              END IF
            ELSE
              IERR = 11
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
                IERR = 12
                MESSAG(1) = 'NAME modifier missing quote string on FILE (KDSOIL) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              ELSE
                FN_KDSOIL = TMP_ID
              END IF
            ELSE
              IERR = 13
              MESSAG(1) = 'NAME modifier not found on FILE card for type KDSOIL'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('DILUTE  ') ) THEN
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 14
                MESSAG(1) = 'NAME modifier missing quote string on FILE (DILUTE) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              ELSE
                FN_DILUTE = TMP_ID
              END IF
            ELSE
              IERR = 15
              MESSAG(1) = 'NAME modifier not found on FILE card for type DILUTE'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
!
          ESD_NUM_LOC = ESD_NUM_LOC + 1
          IF( CEXIST('SORP    ') ) THEN
            ESD_LOC(ESD_NUM_LOC)%SORP = .TRUE.
            NEED_SEEP = .TRUE.
            NEED_SORP = .TRUE.
          ENDIF
          IF( CEXIST('SEEP') ) THEN
            ESD_LOC(ESD_NUM_LOC)%SEEP = .TRUE.
            NEED_SEEP = .TRUE.
          ENDIF
          IF( CEXIST('ID') ) THEN
!            WRITE(*,*) 'Found ID'
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%ID = TMP_ID
            ELSE
              IERR = 16
              MESSAG(1) = 'Location ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 17
            MESSAG(1) = 'ID modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%NAME = TMP_NAME
            ELSE
              IERR = 18
              MESSAG(1) = 'Location NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 19
            MESSAG(1) = 'NAME modifier not entered on the LOCATION keyword'
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
            IERR = 20
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
      SUBROUTINE KEY_RIPSAC_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the RIPSAC keyword control
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
!!    Paul W. Eslinger :  9 May 2000 : Version 1.0
!!    Carmen Arimescu  : 11 Nov 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Move REPORT to separate routine
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
      CHARACTER(LEN=12) :: CALLER = 'KEY_RIPSAC_1' ! Name of this subroutine
      INTEGER :: IDX   ! Temporary index variable
      CHARACTER*(LENCRD) :: TITLE
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
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
          RIP_NUM_ANA = RIP_NUM_ANA + NQUOTE
!
        CASE( 'DEBUG' ) ! ===> DEBUG keyword
          DEBUG = .TRUE.
!
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
!
        CASE( 'EXECUTE' ) ! ===> EXECUTE keyword
          EXECUT = .TRUE.
!
        CASE( 'FILE' ) ! ===> FILE keyword
          IF( CEXIST('ESD') ) THEN
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
        CASE( 'TITLE' ) ! ===> TITLE Keyword
          PTITLE = QUOTE(1)
!
        CASE( 'USER' ) ! ===> USER Keyword
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
      SUBROUTINE KEY_RIPSAC_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the RIPSAC keyword control
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
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!!    Carmen Arimescu  : 11 Nov 2002 : SAC Rev. 1
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
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'KEY_RIPSAC_2' ! Name of this subroutine
!
      INTEGER :: IDX   ! Temporary index variable
      INTEGER :: IDX2  ! Temporary index variable
      INTEGER :: IREL  ! Temporary realization index variable
      INTEGER :: IANA, IDXA  ! Temporary index for analytes
      INTEGER :: IDXL, ILOC  ! Temporary index for locations
!
      CHARACTER*(LENCRD) :: TITLE  ! Keyword data line read by RDBLK routines
      CHARACTER(LEN=6) :: SHORT_ID ! Quote string at ID length
!
!     Temporary variableS for quote strings (length matched with RDBLK quotes)
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_AN, TMP_ID_K, LOC_ID_2
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
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
!
          IF( NQUOTE .LT. 1 ) THEN
            IERR = 1
            MESSAG(1) = 'At least one ID is required on the ANALTYE keyword'
            MESSAG(2) = 'Modify the ANALYTE keyword in the RIPSAC keyword file'
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
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IKEY )
          RETURN
!
        CASE( 'KDSOIL' ) ! ===> KDSOIL keyword
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
          IF( CEXIST('KDSOIL') ) THEN
            CALL NXTQOT( IDX, TMP_ID_K )
            IF( IDX .LE. 0 ) THEN
              IERR = 5
              MESSAG(1) = 'KDSOIL modifier on the KDSOIL keyword is missing the ID string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'Every KDSOIL keyword must also have the KDSOIL modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('ANALYTE') ) THEN
            CALL NXTQOT( IDX, TMP_AN )
            CALL MATCH_ANA( TMP_AN, IDXA )
            IF( IDXA .LE. 0 ) GO TO 10
          ELSE
            IERR = 7
            MESSAG(1) = 'Every KDSOIL keyword must have the ANALYTE modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Check for a LOCATION or TERSE modifier
          IF( .NOT.(CEXIST('LOCATION') .OR. CEXIST('TERSE')) ) THEN
            IERR = 8
            MESSAG(1) = 'The KDSOIL keyword must have a LOCATION or TERSE modifier.'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Check for only one of LOCATION or TERSE
          IF( CEXIST('LOCATION') .AND. CEXIST('TERSE') ) THEN
            IERR = 9
            MESSAG(1) = 'The KDSOIL keyword must have a LOCATION or TERSE modifier.'
            MESSAG(2) = 'But, only one of the LOCATION or TERSE modifiers is alowed.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('LOCATION') ) THEN ! Assign the KDSOIL value to one or more locations
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 10
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
                IERR = 11
                MESSAG(1) = 'Location requested on KDSOIL keyword is not in the master list'
                MESSAG(2) = 'Location ID is '//TMP_ID
                MESSAG(3) = 'Analyte ID is '//TMP_AN
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
              KD_MAP(IDXL,IDXA) = TRIM(TMP_ID_K)
            END DO
          END IF
!
          IF( CEXIST('TERSE') ) THEN ! Assign the same KDSOIL value to all locations
            DO ILOC = 1, ESD_NUM_LOC
              KD_MAP(ILOC,IDXA) = TRIM(TMP_ID_K)
            END DO
          END IF
!
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
!
          IF( CEXIST('PRIMARY') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              SHORT_ID = TMP_ID
              CALL MATCH_LOC( SHORT_ID, IDXL )
              IF( IDXL .LE. 0 ) THEN
                IERR = 12
                MESSAG(1) = 'Primary location is not in the master list'
                MESSAG(2) = 'Location ID: ' // TRIM(TMP_ID)
                MESSAG(3) = 'Problem in the RIPSAC keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            ELSE
              IERR = 13
              MESSAG(1) = 'location PRIMARY modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
            MESSAG(1) = 'PRIMARY modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( CEXIST('SECOND') ) THEN
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 15
              MESSAG(1) = 'The SECOND modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(TMP_ID)
              MESSAG(4) = 'Problem in the RIPSAC keyword file'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            ELSE
              SHORT_ID = LOC_ID_2
              CALL MATCH_LOC( SHORT_ID, IDX2 )
              IF( IDX2 .LE. 0 ) THEN
                IERR = 16
                MESSAG(1) = 'Secondary location is not in the master list'
                MESSAG(2) = 'Location and secondary IDs are: '//TRIM(TMP_ID)//' and '//TRIM(LOC_ID_2)
                MESSAG(3) = 'Problem in the ECEM keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
              ESD_LOC(IDXL)%SECOND = IDX2
            END IF
          ELSE
            IERR = 17
            MESSAG(1) = 'SECOND modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( CEXIST('DF      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              DF_MAP(IDXL) = TMP_ID
            ELSE
              IERR = 18
              MESSAG(1) = 'Location DF modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 19
            MESSAG(1) = 'LOCATION keyword missing dilution factor modifier DF'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
!         Check for single option
          IF( CEXIST('ALL').AND.CEXIST('LIST') .OR. CEXIST('ALL').AND.CEXIST('RANGE') .OR. &
            CEXIST('LIST').AND.CEXIST('RANGE') ) THEN
            IERR = 20
            MESSAG(1) = 'Invalid options on the REALIZATION keyword'
            MESSAG(2) = 'Only one of ALL, LIST, or RANGE allowed'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!         ALL option
          IF( CEXIST('ALL') ) THEN
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
              IERR = 21
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
!!    Paul W. Eslinger : 10 Mar 2000 : Version 1.0
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
!!    IERR  : Output - Integer   - Error number
!!
!!  History:
!!
!!    Paul W. Eslinger : 15 Mar 2000 : Version 1.0
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
      SUBROUTINE OPEN_DILUTE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the input file which contain the dilution
!!    data and reads the data.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Apr 2000 : Version 1.0
!!    Carmen Arimescu  : 11 Nov 2002
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
      USE Dilute_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'OPEN_DILUTE' ! Name of this subroutine
      INTEGER :: ILOC ! Local location index
      INTEGER :: IREL ! Local realization index
      INTEGER :: I    ! Local looping variable
      INTEGER :: J    ! Local looping variable
!
      LOGICAL :: THERE ! Temporary logical variable
      INTEGER :: IERF  ! Temporary system error code
!
      CHARACTER(LEN=20) :: TMP_UNITS ! Temporary units string for input
      REAL, ALLOCATABLE :: DFWORK(:) ! Temporary dilution factor work vector
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the requested file exists
!
      IF( FN_DILUTE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The seep water dilution file name is blank'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, DILUTE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FN_DILUTE,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested seep water dilution data file was not found'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, DILUTE modifier'
        MESSAG(3) = 'File: '//TRIM(FN_DILUTE)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IDIL,FILE=FN_DILUTE,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the seep water dilution data file'
        MESSAG(2) = 'File name entered on the ESD FILE keyword, DILUTE modifier'
        MESSAG(3) = 'File: '//TRIM(FN_DILUTE)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read all of the header data
!
      READ(IDIL,*,ERR=998,END=998) DF_TYPE   ! Type of data file
      READ(IDIL,*,ERR=998,END=998) DF_PTITLE ! Program modification date from dilution generator
      READ(IDIL,*,ERR=998,END=998) DF_PRGNAM ! Program name from dilution generator
      READ(IDIL,*,ERR=998,END=998) DF_PRGVER ! Program version number from dilution generator
      READ(IDIL,*,ERR=998,END=998) DF_PRGDAT ! Program date from dilution generator
      READ(IDIL,*,ERR=998,END=998) DF_CRUNID ! Run identification number from dilution generator
      READ(IDIL,*,ERR=998,END=998) DF_USRNAM ! User name from dilution generator
      READ(IDIL,*,ERR=998,END=998) DF_NUM    ! Number of dilution stochastic variables
      READ(IDIL,*,ERR=998,END=998) DF_NITR   ! Number of dilution stochastic iterations
!      
      IF( DEBUG ) THEN
        WRITE(IRPT,1000) 'DF_TYPE   = ', TRIM(DF_TYPE)
        WRITE(IRPT,1000) 'DF_PTITLE = ', TRIM(DF_PTITLE)
        WRITE(IRPT,1000) 'DF_PRGNAM = ', TRIM(DF_PRGNAM)
        WRITE(IRPT,1000) 'DF_PRGVER = ', TRIM(DF_PRGVER)
        WRITE(IRPT,1000) 'DF_PRGDAT = ', TRIM(DF_PRGDAT)
        WRITE(IRPT,1000) 'DF_CRUNID = ', TRIM(DF_CRUNID)
        WRITE(IRPT,1000) 'DF_USRNAM = ', TRIM(DF_USRNAM)
        WRITE(IRPT,1010) 'DF_NUM    = ', DF_NUM
        WRITE(IRPT,1010) 'DF_NITR   = ', DF_NITR  
 1000   FORMAT(3X,A,A)          
 1010   FORMAT(3X,A,I0)          
      END IF
!
! *** Do some error checking
!
      IF( DF_TYPE .NE. 'DILUTE' ) THEN
        IERR = 4
        MESSAG(1) = 'Wrong data type in the seep water dilution data file'
        MESSAG(2) = 'First entry in the file was not the string DILUTE'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( DF_NITR .LT. NREAL ) THEN
        IERR = 5
        MESSAG(1) = 'Not enough realizations in the seep water dilution data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!
! *** Read the dilution factor data and see if it matches with locations
!     we are using in this run
!
      ALLOCATE( DFWORK(DF_NITR) )
!
      DO I = 1, DF_NUM
!
! ***   Read all of the data
!
        READ(IDIL,*,ERR=999) DF_NUM, DF_ID, TMP_UNITS, (DFWORK(J),J=1,DF_NITR) 
!
! ***   See if the data just read are needed
!
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT. ESD_LOC(ILOC)%SEEP ) CYCLE
!          write(irpt,1111) iloc, DF_NUM, trim(DF_ID), trim(TMP_UNITS), trim(DF_MAP(ILOC))
! 1111     format(' Dilute: ',I0,' : ',I0,' : ',A,' : ',A,' Checking against ',A)          
          IF( DF_MAP(ILOC) .EQ. DF_ID ) THEN
!            write(irpt,1112) trim(DF_ID), TMP_UNITS
! 1112       format(' Match was found at ',A,' for units = ',A)            
            DF_UNITS(ILOC) = TMP_UNITS
            DO IREL = 1, NREAL
              DF(ILOC,IREL) = DFWORK(IREL)
            END DO
          END IF
        END DO
      END DO
!
! *** Check that all required dilution factors are entered (when seep media are being computed)
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%SEEP ) CYCLE
        DO IREL = 1, NREAL
          IF( DF(ILOC,IREL) .LT. 0.0 ) THEN
            IERR = 6
            MESSAG(1) = 'Invalid (negative) dilution factor value encountered'
            MESSAG(2) = 'Location ID = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Option: Data not matched on a DILUTE card'
            MESSAG(4) = 'Option: Invalid distribution on an ESD DILUTE keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END DO
      END DO
!
! *** Normal exit
!
      DEALLOCATE( DFWORK )
      CLOSE( IDIL )
      RETURN
!
! *** Error reading the header data
!
  998 CONTINUE
      IERR = 7
      MESSAG(1) = 'Error reading header data from the DILUTE data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
! *** Error reading the header data
!
  999 CONTINUE
      IERR = 8
      MESSAG(1) = 'Error reading data from the DILUTE data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
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
!!    Paul W. Eslinger : 25 Apr 2000 : Version 2.0
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
      INTEGER :: IERR ! Error numner (0=no errors)
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'OPEN_ESD' ! Name of this subroutine
      INTEGER :: IERF  ! Status variable for open statement
      LOGICAL :: THERE ! Logical value for inquire statement
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
        MESSAG(3) = 'Problem exists in the RIPSAC keyword file'
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
        MESSAG(4) = 'Problem exists in the RIPSAC keyword file'
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
!!    Carmen Arimescu  : 11 Nov 2002
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
      INTEGER :: IERR ! Error number (0=no errors)
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
      CHARACTER(LEN=20) :: TMP_UNITS ! Temporary units string for input
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
! ***   Read all of the data
!
        READ(IKDS,*,ERR=999) KD_NUM, KD_ID, TMP_UNITS, (KDWORK(J),J=1,KD_NITR) 
!
! ***   See if the data just read are needed
!
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT. (ESD_LOC(ILOC)%SEEP.OR.ESD_LOC(ILOC)%SORP) ) CYCLE
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
! *** Check that all required KD's are entered (when seep or soil media are being computed)
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. (ESD_LOC(ILOC)%SEEP.OR.ESD_LOC(ILOC)%SORP) ) CYCLE
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
          DO IREL = 1, NREAL
            IF( KD(ILOC,IANA,IREL) .LT. 0.0 ) THEN
              IERR = 6
              MESSAG(1) = 'Invalid (negative) KD value encountered, Realization = xxx'
              WRITE(MESSAG(1)(56:58),'(I3)') IREL
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
!!      2. Open the file for use by subroutine reading subroutines
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate to TIIA, add copyright information
!!    Paul W. Eslinger :  9 Jul 2012 : Revise to a common callable routine
!!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
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
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      LOGICAL :: THERE         ! Existence variable
      INTEGER :: IERF          ! Status variable for open statement
      INTEGER :: PLEN_KEY      ! Number of characters in the variable PATH_KEY
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
! *** Get the path from the RIPSAC keyword file for use in the "Done" file
!      CALL GET_PATH( FNKEY, PATH_KEY, PLEN_KEY, IERR )
!      IF( IERR .NE. 0  ) THEN
!        IERR = 3
!        WRITE(*,1040) TRIM(FNKEY)
! 1040   FORMAT(' Error extracting path from the RIPSAC keyword file name'/' File: ',A)
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
      SUBROUTINE PROCESS_IMPACTS( IANA, IERR )
!!******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine processes the seep and soil concentrations for all
!!    locations and times.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  1 May 2001 : Add realization subset logic
!!    Paul W. Eslinger : 14 Aug 2001 : Error message for negative concentratins
!!    Carmen Arimescu  : 11 Nov 2002
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments and add IMPLICIT NONE
!!
!!******************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE Esd_Mod
      USE Control_Mod
      USE ECDA_Mod
      USE Dilute_Mod
      USE Kdsoil_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IANA ! Analyte index
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'PROCESS_IMPACTS' ! Name of this subroutine
!
      INTEGER :: IDXGWAT   ! Groundwater concentration data record number
      INTEGER :: IDXSEEP   ! Seep water concentration data record number
      INTEGER :: IDXSWAT   ! Surface water concentration data record number
      INTEGER :: IDXSORP   ! Soil concentration data record number
      INTEGER :: ILOC, ITIM, IREL ! Location, time and realization indices
!
      INTEGER :: TIME            ! Calendar year from file
      CHARACTER(LEN=6) :: LOC_ID ! Location ID from file
      CHARACTER(LEN=4) :: MED_ID ! Media ID from file
!
      LOGICAL :: THERE ! Local logical variable
!
      CHARACTER(LEN=10) :: SDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: STIME ! System time in the form HHMMSS.SSS
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      MESSAG(1) = 'Processing data for analyte "' // TRIM(ESD_ANA(IANA)%ID) // '"'
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
! *** Loop over times and locations for this analyte
!
      DO ILOC = 1, ESD_NUM_LOC
!
        IF( .NOT.(ESD_LOC(ILOC)%SEEP .OR. ESD_LOC(ILOC)%SORP) ) CYCLE
!        WRITE(*,*) '  Location = ',ESD_LOC(ILOC)%ID
!
        DO ITIM = 1, ESD_NUM_TIM
!
          IF( DEBUG ) THEN
            WRITE(IRPT,2000) ILOC, ESD_LOC(ILOC)%ID, ITIM, ESD_TIM(ITIM)%TIME
 2000       FORMAT(/'Location Index ', I0,' and ID "',A,'" : Time Index ',I0,' and year ',I0)
          END IF
!
! ***     Get surface water concentrations
!
          CALL GET_INDEX( ITIM, ILOC, ISWAT, IDXSWAT, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 3
            MESSAG(1) = 'Invalid ECDA index for surface water'
            MESSAG(2) = 'Check whether surface water was calculated'
            MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( IDXSWAT .LE. 0 ) THEN
            IERR = 4
            MESSAG(1) = 'Surface water concentration data not computed'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          CALL ECDA_READ( IDXSWAT, TIME, LOC_ID, MED_ID, CVEC_SWAT, NREAL, ICON, IERR )
!          WRITE(*,*) IDXSWAT, TIME, LOC_ID, ' ', MED_ID, CVEC_SWAT
          IF( IERR .NE. 0 ) THEN
            IERR = 5
            MESSAG(1) = 'Error reading surface water concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          IF( DEBUG ) THEN
            WRITE(IRPT,'(A)') 'Surface water: Index and realization'
            DO IREL = 1, NREAL
              IF( .NOT.REL_USE(IREL) ) CYCLE
              WRITE(IRPT,2040) IREL, CVEC_SWAT(IREL)
 2040         FORMAT(2X,I3,1X,1P,E13.6)
            END DO
          END IF
!
          DO IREL = 1, NREAL
            IF( .NOT.REL_USE(IREL) ) CYCLE
            IF( CVEC_SWAT(IREL) .LT. 0.0 ) THEN
              IERR = 6
              MESSAG(1) = 'Negative surface water concentration encountered'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID // '  Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC_SWAT(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
! ***     Get ground water concentrations
!
          CALL GET_INDEX( ITIM, ILOC, IGWAT, IDXGWAT, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 7
            MESSAG(1) = 'Invalid ECDA index for ground water'
            MESSAG(2) = 'Check whether ground water was calculated'
            MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( IDXGWAT .LE. 0 ) THEN
            IERR = 8
            MESSAG(1) = 'Ground water concentration data not computed'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          CALL ECDA_READ( IDXGWAT, TIME, LOC_ID, MED_ID, CVEC_GWAT, NREAL, ICON, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 9
            MESSAG(1) = 'Error reading ground water concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          IF( DEBUG ) THEN
            WRITE(IRPT,'(A)') 'Ground water: Index and realization'
            DO IREL = 1, NREAL
              IF( .NOT.REL_USE(IREL) ) CYCLE
              WRITE(IRPT,2040) IREL, CVEC_GWAT(IREL)
            END DO
          END IF
!
          DO IREL = 1, NREAL
            IF( .NOT.REL_USE(IREL) ) CYCLE
            IF( CVEC_GWAT(IREL) .LT. 0.0 ) THEN
              IERR = 10
              MESSAG(1) = 'Negative ground water concentration encountered'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID // '  Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Year        = '
              WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
              MESSAG(4) = 'Realization = '
              WRITE(MESSAG(4)(15:),'(I0)') IREL
              MESSAG(5) = 'Value       = '
              WRITE(MESSAG(5)(15:),'(1P,E12.5)') CVEC_GWAT(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
!          IF( DEBUG ) THEN
!            WRITE(IRPT,2030) IDXGW, ESD_LOC(ILOC)%ID, LOC_ID, 'GW', MED_ID
! 2030       FORMAT(/' Check the groundwater reading routine for index ',I6/ &
!              5X,' Asked for location ',A,'  Obtained location ',A/ &
!              5X,' Asked for media ',A,'  Obtained media ',A)
!          END IF
!
! ***     Get seep water concentrations
!
          CALL GET_INDEX( ITIM, ILOC, ISEEP, IDXSEEP, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 11
            MESSAG(1) = 'Invalid ECDA index for seep water'
            MESSAG(2) = 'Check whether seep water is to be calculated'
            MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( IDXSEEP .LE. 0 ) THEN
            IERR = 12
            MESSAG(1) = 'Seep water concentration data not computed'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          CALL ECDA_READ( IDXSEEP, TIME, LOC_ID, MED_ID, CVEC_SEEP, NREAL, ICON, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 13
            MESSAG(1) = 'Error reading seep water concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          IF( DEBUG ) THEN
            WRITE(IRPT,'(A)') 'Seep water: Index and realization'
            DO IREL = 1, NREAL
              IF( .NOT.REL_USE(IREL) ) CYCLE
              WRITE(IRPT,2040) IREL, CVEC_SEEP(IREL)
            END DO
          END IF
!
! ***     Get riparian soil concentrations
!
          CALL GET_INDEX( ITIM, ILOC, ISORP, IDXSORP, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 14
            MESSAG(1) = 'Invalid ECDA index for riparian soil'
            MESSAG(2) = 'Check whether riparian soil is to be calculated'
            MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( IDXSORP .LE. 0 ) THEN
            IERR = 15
            MESSAG(1) = 'Riparian soil concentration data not computed'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          CALL ECDA_READ( IDXSORP, TIME, LOC_ID, MED_ID, CVEC_SORP, NREAL, ICON, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 16
            MESSAG(1) = 'Error reading Riparian soil concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            MESSAG(4) = 'Time = '
            WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          IF( DEBUG ) THEN
            WRITE(IRPT,'(A)') 'Riparian Soil: Index and realization'
            DO IREL = 1, NREAL
              IF( .NOT.REL_USE(IREL) ) CYCLE
              WRITE(IRPT,2040) IREL, CVEC_SORP(IREL)
            END DO
          END IF
!
! ***     Compute the seep and soil concentrations
!
          DO IREL = 1, NREAL
            IF( .NOT.REL_USE(IREL) ) CYCLE
            CVEC_SEEP(IREL) = DF(ILOC,IREL)*CVEC_GWAT(IREL) + (1.0-DF(ILOC,IREL))*CVEC_SWAT(IREL)
            CVEC_SORP(IREL) = KD(ILOC,IANA,IREL)*CVEC_SEEP(IREL)
          END DO
!
! ***     Output the seep water concentration if requested
!
          IF( ESD_LOC(ILOC)%SEEP ) THEN
            CALL GET_INDEX( ITIM, ILOC, ISEEP, IDXSEEP, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 17
              MESSAG(1) = 'Invalid ECDA index for seep water'
              MESSAG(2) = 'Check whether seep water was to be calculated'
              MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
            IF( IDXSEEP .LE. 0 ) THEN
              IERR = 18
              MESSAG(1) = 'Seep water concentration data computed but not desired'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
            CALL ECDA_WRITE( IDXSEEP, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SEEP', CVEC_SEEP, NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 19
              MESSAG(1) = 'An error occurred writing the seep water data to the ECDA file.'
              MESSAG(2) = 'File name is ' // TRIM(FN_CON(IANA))
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(5) = 'Time = '
              WRITE(MESSAG(5)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
          END IF
!
! ***     Output the soil concentration if requested
!
          IF( ESD_LOC(ILOC)%SORP ) THEN
            CALL GET_INDEX( ITIM, ILOC, ISORP, IDXSORP, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 20
              MESSAG(1) = 'Invalid ECDA index for soil'
              MESSAG(2) = 'Check whether soil was to be calculated'
              MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
            IF( IDXSORP .LE. 0 ) THEN
              IERR = 21
              MESSAG(1) = 'Soil concentration data computed but not desired'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
            CALL ECDA_WRITE( IDXSORP, ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'SORP', CVEC_SORP, NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 22
              MESSAG(1) = 'An error occurred writing the soil data to the ECDA file.'
              MESSAG(2) = 'File name is ' // TRIM(FN_CON(IANA))
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(5) = 'Time = '
              WRITE(MESSAG(5)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
          END IF
!
          IF( DEBUG ) THEN
!
          IF( ESD_LOC(ILOC)%SEEP ) THEN
            WRITE(IRPT,2010) IDXGWAT, IDXSEEP, IDXSORP, IDXSWAT        
 2010       FORMAT(/ &
              'Goundwater Index    : ',I0/ &
              'Seep water Index    : ',I0/ &
              'Soil Index          : ',I0/ &
              'Surface Water Index : ',I0/ &
              'Rel Ground Water Surface Wate Seep Water    Soil         Dilute Fact  Dilute Units  KD Value    KD UNITS   ' / &  
              '--- ------------ ------------ ------------ ------------ ------------ ------------ ------------ ------------')
!
            DO IREL = 1, NREAL
              IF( .NOT.REL_USE(IREL) ) CYCLE
              WRITE(IRPT,2020) IREL, CVEC_GWAT(IREL), CVEC_SWAT(IREL), CVEC_SEEP(IREL), CVEC_SORP(IREL), &
                DF(ILOC,IREL), DF_UNITS(ILOC), KD(ILOC,IANA,IREL), KD_UNITS(ILOC)
 2020         FORMAT(I3,1P,5(1X,E12.5),1X,A12,1X,E12.5,1X,A12)
            END DO
          ELSE
            WRITE(IRPT,2030) IDXGWAT, IDXSEEP, IDXSORP, IDXSWAT
 2030       FORMAT(/ &
              'Goundwater Index    : ',I0/ &
              'Seep water Index    : ',I0/ &
              'Soil Index          : ',I0/ &
              'Surface Water Index : ',I0/ &
              'Rel Ground Water Surface Wate Seep Water    Soil        KD Value    KD UNITS   ' / &  
              '--- ------------ ------------ ------------ ------------ ------------ ------------')
!
            DO IREL = 1, NREAL
              IF( .NOT.REL_USE(IREL) ) CYCLE
              WRITE(IRPT,2050) IREL, CVEC_GWAT(IREL), CVEC_SWAT(IREL), CVEC_SORP(IREL), &
                KD(ILOC,IANA,IREL), TRIM(KD_UNITS(ILOC))
 2050         FORMAT(I3,1P,4(1X,E12.5),1X,A)
            END DO
          END IF
!
          END IF
!
        END DO
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
! *** Call list variables
      CHARACTER(LEN=*) :: MESSAGE ! Character string to write
      CHARACTER(LEN=*) :: PLACE   ! Place to output the mesage
      LOGICAL :: LDATE            ! Only print date if LDATE=.TRUE.
      INTEGER :: IRPT             ! Unit number for output file
!
! *** User defined functions
      LOGICAL, EXTERNAL :: STRCOMP
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

