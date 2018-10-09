!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
MODULE FCDA_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Purpose:
!
!   This module contains variables related to reading and writing FOOD Concentration Data (FCDA) files.
!
! History:
!
!    Paul W. Eslinger : 11 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 23 Feb 2006 : Change name of upland index variables
!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
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
!
      CHARACTER(LEN=200) :: FCDA_PTITLE ! Problem title line from generating program
      CHARACTER(LEN= 10) :: FCDA_PRGNAM ! Program name of generating program
      CHARACTER(LEN=  8) :: FCDA_PRGVER ! Program version number of generating program
      CHARACTER(LEN= 12) :: FCDA_PRGDAT ! Program date of generating program
      CHARACTER(LEN= 16) :: FCDA_USRNAM ! User name from generating program
      CHARACTER(LEN= 14) :: FCDA_CRUNID ! Run identification number from generating program
!
!     Indices for food calculations by ESD location and aquatic species
      INTEGER, ALLOCATABLE :: FCDA_MAP_AQ(:)
!
!     Indices for food calculations by ESD location and riparian species
      INTEGER, ALLOCATABLE :: FCDA_MAP_RP(:)
!
!     Indices for food calculations by ESD location and soil type for upland species
      INTEGER, ALLOCATABLE :: FCDA_MAP_UP(:,:)
!
      INTEGER, PARAMETER :: IDXF_SODR = 1 ! Index for upland food map for dry-land soil species
      INTEGER, PARAMETER :: IDXF_SOGW = 2 ! Index for upland food map for groundwater irrigated soil species
      INTEGER, PARAMETER :: IDXF_SOSW = 3 ! Index for upland food map for surfacewater irrigated soil species
!
!===> Type definition for FCDA (and ECEM) time data
      TYPE FCDA_TIM_TYPE
        INTEGER :: TIME ! Times where data are stored
        LOGICAL :: COMP ! Flag whether time is to be used in Human
      END TYPE FCDA_TIM_TYPE
      TYPE (FCDA_TIM_TYPE), ALLOCATABLE :: FCDA_TIM(:) ! The FCDA location variable
!
!===> Type definition for FCDA (and ECEM) location data
      TYPE FCDA_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        LOGICAL :: COMP           ! Flag whether this location is to be used in Human
      END TYPE FCDA_LOC_TYPE
      TYPE (FCDA_LOC_TYPE), ALLOCATABLE :: FCDA_LOC(:) ! The ESD location variable
!
!===> Type definition for FCDA (and ECEM) species data
      TYPE FCDA_SPC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Species ID
        CHARACTER(LEN=48) :: NAME ! Species name
        LOGICAL :: COMP           ! Flag whether a species results are computed as a food
      END TYPE FCDA_SPC_TYPE
      TYPE(FCDA_SPC_TYPE), ALLOCATABLE :: FCDA_SPC(:) ! Variable structure for species information
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE FCDA_Mod
!
      SUBROUTINE FCDA_MAPECHO( IRPT, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes the initial set of header data from
!!    the index map for FCDA data files to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!***********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IRPT ! Unit number for the open report file
      INTEGER :: IERR ! Error number
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'FCDA_MAPECHO'
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
      WRITE(IRPT,3000,ERR=9000) 'Header information from the map file'
      WRITE(IRPT,3010,ERR=9000) 'FCDA_PTITLE   = ', TRIM(FCDA_PTITLE)
      WRITE(IRPT,3010,ERR=9000) 'FCDA_PRGNAM   = ', TRIM(FCDA_PRGNAM)
      WRITE(IRPT,3010,ERR=9000) 'FCDA_PRGVER   = ', TRIM(FCDA_PRGVER)
      WRITE(IRPT,3010,ERR=9000) 'FCDA_PRGDAT   = ', TRIM(FCDA_PRGDAT)
      WRITE(IRPT,3010,ERR=9000) 'FCDA_USRNAM   = ', TRIM(FCDA_USRNAM)
      WRITE(IRPT,3010,ERR=9000) 'FCDA_CRUNID   = ', TRIM(FCDA_CRUNID)
      WRITE(IRPT,3020,ERR=9000) 'FCDA_BLOCK_AQ = ', FCDA_BLOCK_AQ
      WRITE(IRPT,3020,ERR=9000) 'FCDA_BLOCK_RP = ', FCDA_BLOCK_RP
      WRITE(IRPT,3020,ERR=9000) 'FCDA_BLOCK_UP = ', FCDA_BLOCK_UP
      WRITE(IRPT,3020,ERR=9000) 'FCDA_NHEAD    = ', FCDA_NHEAD
      WRITE(IRPT,3020,ERR=9000) 'FCDA_RECLEN   = ', FCDA_RECLEN
      WRITE(IRPT,3020,ERR=9000) 'FCDA_NREAL    = ', FCDA_NREAL
 3000 FORMAT(/A)
 3010 FORMAT(3X,A,'"',A,'"')
 3020 FORMAT(3X,A,I0)
      RETURN
!
 9000 CONTINUE
!
      IERR = 1
      MESSAG(1) = 'Error writing to the report file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
      END SUBROUTINE FCDA_MAPECHO
!
      SUBROUTINE FCDA_MAPREAD( FNMAP, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, IERR )
!!**********************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads an ASCII FCDA map file.
!!
!!  Limitations:
!!
!!    This subroutine can only be used in the context of code that has read and stored time,
!!    location, and species information from the ESD keyword file.
!!
!!  Auxiliary Routines:
!!
!!    GET_UNIT_NUMBER - Gets a unit number to use when reading the file.  The unit is closed
!!                      at the end of the reading activities.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  1 Jun 2007 : Change FCDA routines to get ESD parameters
!!                                     through the call list rather than a module
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************************************
!
! *** Global variables
      USE FCDA_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FNMAP ! Name of the map file
      INTEGER :: ESD_NUM_TIM                ! Number of times in the ESD keyword file
      INTEGER :: ESD_NUM_LOC                ! Number of locations in the ESD keyword file
      INTEGER :: ESD_NUM_SPC                ! Number of species in the ESD keyword file
      INTEGER :: ESD_NUM_SOI                ! Number of soil types in the ESD keyword file
      INTEGER, INTENT(INOUT) :: IERR        ! Error Number
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'FCDA_MAPREAD' ! Name of this routine
!
      INTEGER :: IMAP  ! Unit number for the record number map file
      INTEGER :: IOS   ! I/O Status from open, write, or close of file
      INTEGER :: IERA  ! Error status variable from the allocate action
      INTEGER :: J, K  ! Index variables for writing
      LOGICAL :: THERE ! Temporary logical variable
!
      INTEGER :: NUM_TIM, NUM_LOC, NUM_SPC ! Time,location, and species counters
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
!
! *** Read number of header lines
      READ(IMAP,*,ERR=7000) FCDA_NHEAD
!
!--------------------------------------------------------------------------
!     Time information
!--------------------------------------------------------------------------
!
      READ(IMAP,*,ERR=7000) NUM_TIM
      IF( NUM_TIM .NE. ESD_NUM_TIM ) THEN
        IERR = 4
        MESSAG(1) = 'Number of times in the map file does not match with ESD keyword information'
        MESSAG(2) = 'Map number = '
        WRITE(MESSAG(2)(14:),*) NUM_TIM
        MESSAG(3) = 'ESD number = '
        WRITE(MESSAG(3)(14:),*) ESD_NUM_TIM
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
!     Allocate the space to store data
      IF( ESD_NUM_TIM .GT. 0 ) THEN
        ALLOCATE( FCDA_TIM(ESD_NUM_TIM), STAT= ISTAT )
        IF(ISTAT .NE. 0 ) THEN
          IERR = 5
          MESSAG(1) = 'Error allocating FCDA_TIM(ESD_NUM_TIM)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        IERR = 6
        MESSAG(1) = 'Could not allocate FCDA_TIM(ESD_NUM_TIM)'
        MESSAG(2) = 'ESD_NUM_TIM was not greater than zero'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Read the data
      DO J = 1, ESD_NUM_TIM
        READ(IMAP,*,ERR=7000) FCDA_TIM(J)%TIME, FCDA_TIM(J)%COMP
      END DO
!
!--------------------------------------------------------------------------
!     Location information
!--------------------------------------------------------------------------
!
      READ(IMAP,*,ERR=7000) NUM_LOC
      IF( NUM_LOC .NE. ESD_NUM_LOC ) THEN
        IERR = 8
        MESSAG(1) = 'Number of locations in the map file does not match with ESD keyword information'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Allocate the space to store data
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( FCDA_LOC(ESD_NUM_LOC), STAT= ISTAT )
        IF(ISTAT .NE. 0 ) THEN
          IERR = 9
          MESSAG(1) = 'Error allocating FCDA_LOC(ESD_NUM_LOC)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        IERR = 10
        MESSAG(1) = 'Could not allocate FCDA_LOC(ESD_NUM_LOC)'
        MESSAG(2) = 'ESD_NUM_LOC was not greater than zero'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Read the data
      DO J = 1, ESD_NUM_LOC
        READ(IMAP,*,ERR=7000) FCDA_LOC(J)%ID, FCDA_LOC(J)%COMP, FCDA_LOC(J)%NAME
      END DO
!
!--------------------------------------------------------------------------
!     Species information
!--------------------------------------------------------------------------
!
      READ(IMAP,*,ERR=7000) NUM_SPC
      IF( NUM_SPC .NE. ESD_NUM_SPC ) THEN
        IERR = 12
        MESSAG(1) = 'Number of species in the map file does not match with ESD keyword information'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Allocate the space to store data
      IF( ESD_NUM_SPC .GT. 0 ) THEN
        ALLOCATE( FCDA_SPC(ESD_NUM_SPC), STAT= ISTAT )
        IF(ISTAT .NE. 0 ) THEN
          IERR = 13
          MESSAG(1) = 'Error allocating FCDA_SPC(ESD_NUM_SPC)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        IERR = 14
        MESSAG(1) = 'Could not allocate FCDA_SPC(ESD_NUM_SPC)'
        MESSAG(2) = 'ESD_NUM_SPC was not greater than zero'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Read the data
      DO J = 1, ESD_NUM_SPC
        READ(IMAP,*,ERR=7000) FCDA_SPC(J)%ID, FCDA_SPC(J)%COMP, FCDA_SPC(J)%NAME
      END DO
!
!--------------------------------------------------------------------------
!     Map information
!--------------------------------------------------------------------------
!
!     Allocate space for aquatic species map
      ALLOCATE( FCDA_MAP_AQ(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 7
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
      ALLOCATE( FCDA_MAP_RP(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
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
      ALLOCATE( FCDA_MAP_UP(ESD_NUM_LOC,ESD_NUM_SOI), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
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
        IERR = 16
        MESSAG(1) = 'Data mismatch in the map file'
        MESSAG(2) = 'Expecting the string "Map: Aquatic Species"'
        MESSAG(3) = 'Obtained the string: "' // TRIM(CTMP) // '"'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
      DO K = 1, ESD_NUM_LOC
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_AQ(K)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 17
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
        IERR = 18
        MESSAG(1) = 'Data mismatch in the map file'
        MESSAG(2) = 'Expecting the string "Map: Riparian Species"'
        MESSAG(3) = 'Obtained the string: "' // TRIM(CTMP) // '"'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
      DO K = 1, ESD_NUM_LOC
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_RP(K)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 19
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
        IERR = 20
        MESSAG(1) = 'Data mismatch in the map file'
        MESSAG(2) = 'Expecting the string "Map: Upland Species"'
        MESSAG(3) = 'Obtained the string: "' // TRIM(CTMP) // '"'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
      DO K = 1, ESD_NUM_LOC
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_UP(K,1)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 21
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'Locations in the map record set - upland'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(K)%ID // '"'
          MESSAG(4) = 'MAP:  "' // TMP_ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_UP(K,2)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 22
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'locations in the map record set - upland'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(K)%ID // '"'
          MESSAG(4) = 'MAP:  "' // TMP_ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_UP(K,3)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 23
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
      IERR = 24
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
      IERR = 25
      MESSAG(1) = 'Error reading record number data from the FCDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', IMAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FNMAP = ' // TRIM(FNMAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
      END SUBROUTINE FCDA_MAPREAD
!
      SUBROUTINE FCDA_OPEN( FN_FCDA, UN_FCDA, FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
!!*******************************************************************************************
!!
!!  Purpose:
!!
!!   This routine reads a single data record from an open FCDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!*******************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FN_FCDA   ! File name for the FCDA file
      INTEGER, INTENT(IN) :: UN_FCDA            ! Unit number for the FCDA file
      CHARACTER(LEN=*) :: FCDA_ANA_ID           ! Analyte ID:  First header line
      CHARACTER(LEN=*) :: FCDA_SPC_ID           ! Species ID:  Second header line
      CHARACTER(LEN=*) :: FCDA_SPC_HAB          ! Species habitat: Third header line
      INTEGER :: IERR                           ! Error Number
!
! *** Local variables
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
      END SUBROUTINE FCDA_OPEN
!
      SUBROUTINE FCDA_READ( NREC, TIME, LOC_ID, CONC, NREAL, ICON, IERR )
!!************************************************************************
!!
!!  Purpose:
!!
!!    This routine reads a single data record from an open FCDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!************************************************************************
!
! *** Global variables
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: NREC  ! Record number
      INTEGER :: TIME              ! Calendar year for concentration data
      CHARACTER(LEN=*) :: LOC_ID   ! Location ID
      REAL, DIMENSION(*) :: CONC   ! Concentration vector of length NREAL
      INTEGER, INTENT(IN) :: NREAL ! Number of realizations to read
      INTEGER, INTENT(IN) :: ICON  ! Fortran unit number to use for input
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'FCDA_READ' ! Name of this subroutine
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
      READ( ICON, IOSTAT=IOS, REC=NREC ) TIME, LOC_ID, (CONC(IREL),IREL=1,NREAL)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error reading from the FCDA concentration file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2),'(A,I3,A,I5)') 'UNIT = ',ICON, ' IOSTAT = ',IOS
        CALL PRTERR(IERR, CALLER, 2)
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE FCDA_READ
!
      SUBROUTINE FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
!!***************************************************************************************************
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
!!    Paul W. Eslinger :  6 Jun 2007 : Change FCDA routines to get ESD parameters
!!                                     through the call list rather than a module
!!
!!***************************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: ISPC ! Species index
      INTEGER :: ESD_NUM_TIM      ! Number of times in the ESD keyword file
      INTEGER :: ESD_NUM_LOC      ! Number of locations in the ESD keyword file
      INTEGER :: ESD_NUM_SPC      ! Number of species in the ESD keyword file
      INTEGER :: NREC             ! Record number (output)
      INTEGER :: IERR             ! Error number indicator (output)
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'FCDA_RECNO_AQ' ! Name of this subroutine
      INTEGER :: NTIM ! Local time block counter
      INTEGER :: JTIM ! Time looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the time index
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ESD_NUM_TIM ) THEN
        IERR = 1
        MESSAG(1) = 'Time index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ITIM
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_TIM
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
      IF( ILOC.LT.1 .OR. ILOC.GT.ESD_NUM_LOC ) THEN
        IERR = 2
        MESSAG(1) = 'Location index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ILOC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_LOC
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
      IF( ISPC.LT.1 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
        IERR = 3
        MESSAG(1) = 'Species index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ISPC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_SPC
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
      END SUBROUTINE FCDA_RECNO_AQ
!
      SUBROUTINE FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
!!***************************************************************************************************
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
!!    Paul W. Eslinger :  6 Jun 2007 : Change FCDA routines to get ESD parameters
!!                                     through the call list rather than a module
!!
!!***************************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: ISPC ! Species index
      INTEGER :: ESD_NUM_TIM      ! Number of times in the ESD keyword file
      INTEGER :: ESD_NUM_LOC      ! Number of locations in the ESD keyword file
      INTEGER :: ESD_NUM_SPC      ! Number of species in the ESD keyword file
      INTEGER :: NREC             ! Record number (output)
      INTEGER :: IERR             ! Error number indicator (output)
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'FCDA_RECNO_RP' ! Name of this subroutine
      INTEGER :: NTIM ! Local time block counter
      INTEGER :: JTIM ! Time looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the time index
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ESD_NUM_TIM ) THEN
        IERR = 1
        MESSAG(1) = 'Time index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ITIM
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_TIM
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
      IF( ILOC.LT.1 .OR. ILOC.GT.ESD_NUM_LOC ) THEN
        IERR = 2
        MESSAG(1) = 'Location index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ILOC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_LOC
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
      IF( ISPC.LT.1 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
        IERR = 3
        MESSAG(1) = 'Species index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ISPC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_SPC
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
      END SUBROUTINE FCDA_RECNO_RP
!
      SUBROUTINE FCDA_RECNO_UP( ITIM, ILOC, ISPC, ISOI, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
!!**********************************************************************************************************************
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
!!    Paul W. Eslinger :  6 Jun 2007 : Change FCDA routines to get ESD parameters
!!                                     through the call list rather than a module
!!
!!**********************************************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: ISOI ! Soil type index
      INTEGER, INTENT(IN) :: ISPC ! Species index
      INTEGER :: ESD_NUM_TIM      ! Number of times in the ESD keyword file
      INTEGER :: ESD_NUM_LOC      ! Number of locations in the ESD keyword file
      INTEGER :: ESD_NUM_SPC      ! Number of species in the ESD keyword file
      INTEGER :: ESD_NUM_SOI      ! Number of upland soil types in the ESD keyword file
      INTEGER :: NREC             ! Record number (output)
      INTEGER :: IERR             ! Error number indicator (output)
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'FCDA_RECNO_UP' ! Name of this subroutine
      INTEGER :: NTIM ! Local time block counter
      INTEGER :: JTIM ! Time looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the time index
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ESD_NUM_TIM ) THEN
        IERR = 1
        MESSAG(1) = 'Time index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ITIM
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_TIM
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
      IF( ILOC.LT.1 .OR. ILOC.GT.ESD_NUM_LOC ) THEN
        IERR = 2
        MESSAG(1) = 'Location index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ILOC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_LOC
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
      IF( ISPC.LT.1 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
        IERR = 3
        MESSAG(1) = 'Species index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ISPC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_SPC
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
      END SUBROUTINE FCDA_RECNO_UP
!
      SUBROUTINE FCDA_WRITE( NREC, TIME, LOC_ID, CONC, NREAL, ICON, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine writes a single data record to an open FCDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Mar 2003 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: NREC ! Record number
      INTEGER, INTENT(IN) :: TIME ! Calendar year for concentration data
      CHARACTER(LEN=*), INTENT(IN) :: LOC_ID ! Location ID
      REAL, DIMENSION(*), INTENT(IN) :: CONC ! Concentration vector of length NREAL
      INTEGER, INTENT(IN) :: NREAL ! Number of realizations to write
      INTEGER, INTENT(IN) :: ICON  ! Fortran unit number to use for output
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'FCDA_WRITE' ! Name of this subroutine
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
      WRITE( ICON, IOSTAT=IOS, REC=NREC ) TIME, LOC_ID, (CONC(IREL),IREL=1,NREAL)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error writing to FCDA concentration file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2),'(A,I0,A,I5)') 'UNIT = ',ICON, ', IOSTAT = ',IOS
        CALL PRTERR(IERR, CALLER, 2)
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE FCDA_WRITE

