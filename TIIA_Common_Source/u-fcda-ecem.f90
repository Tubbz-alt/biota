!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
      SUBROUTINE FCDA_CREATE( FN_FCDA, UN_FCDA, FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
!!********************************************************************************************
!!
!!  Purpose:
!!
!!    This routine creates a FCDA (food) data file and writes the three header
!!    lines to the file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!********************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FN_FCDA      ! File name for the FCDA file
      INTEGER, INTENT(IN) :: UN_FCDA               ! Unit number for the FCDA file
      CHARACTER(LEN=*), INTENT(IN) :: FCDA_ANA_ID  ! Analyte ID: First header line
      CHARACTER(LEN=*), INTENT(IN) :: FCDA_SPC_ID  ! Species ID: Second header line
      CHARACTER(LEN=*), INTENT(IN) :: FCDA_SPC_HAB ! Species habitat: Third header line
      INTEGER :: IERR                              ! Error Number
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'FCDA_CREATE' ! Name of this routine
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Open the file
!
      OPEN(UNIT=UN_FCDA,ERR=1000,FILE=FN_FCDA,STATUS='REPLACE',ACCESS='DIRECT',RECL=FCDA_RECLEN)
!
! *** Write the three header lines to the file
!
      WRITE(UNIT=UN_FCDA, REC=1, ERR=2000) FCDA_ANA_ID
      WRITE(UNIT=UN_FCDA, REC=2, ERR=2000) FCDA_SPC_ID
      WRITE(UNIT=UN_FCDA, REC=3, ERR=2000) FCDA_SPC_HAB
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
! *** An error occurred writing the header to the file
!
 2000 CONTINUE
      IERR = 2
      MESSAG(1) = 'An error occurred writing header information to the FCDA file'
      MESSAG(2) = 'File name is ' // TRIM(FN_FCDA)
      CALL PRTERR( IERR, CALLER, 2 )
      RETURN
!
      END SUBROUTINE FCDA_CREATE
!
      SUBROUTINE FCDA_MAPGEN( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates the data for the index map for FCDA
!!    data files.  Three record maps are generated.
!!      Aquatic:  For aquatic species and aquatic locations
!!      Riparian: For riparian species and riparian locations
!!      Upland:   For upland species and upland locations
!!
!!  Note:
!!
!!    Because ECEM uses a food web approach, and the same food web is
!!    used at all locations of the proper type (aquatic species at
!!    aquatic locations, for example) the record map depends only on
!!    location type and species habitat.  Thus, for example, the same
!!    record map applies for all aquatic species.
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
      USE ESD_Mod
      USE Files_Mod
      USE Iden_Mod
      USE Control_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'FCDA_MAPGEN' ! Name of this subroutine
!
      INTEGER :: IERA          ! Error indicator from the file system
      INTEGER :: ILOC, ISPC, J ! Looping indices
      INTEGER :: NREC          ! Record number
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Identification information
      FCDA_PTITLE = PTITLE
      FCDA_PRGNAM = PRGNAM
      FCDA_PRGVER = PRGVER
      FCDA_PRGDAT = PRGDAT
      FCDA_USRNAM = USRNAM
      FCDA_CRUNID = CRUNID
!
! *** Number of header lines in the binary file
      FCDA_NHEAD = 3
!
! *** Number of realizations in the data file
      FCDA_NREAL  = NREAL
!
! *** Record length of records in the data file
      FCDA_RECLEN = 6 + 4 + 4*NREAL ! Location ID, year, food values by realization
!
!----------------------------------------------------------------------------------
!     Process data for FCDA times
!----------------------------------------------------------------------------------
!
!     Allocate the space to store time data
      IF( ESD_NUM_TIM .GT. 0 ) THEN
        ALLOCATE( FCDA_TIM(ESD_NUM_TIM), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'Error allocating FCDA_TIM(ESD_NUM_TIM)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        IERR = 2
        MESSAG(1) = 'Could not allocate FCDA_TIM(ESD_NUM_TIM)'
        MESSAG(2) = 'ESD_NUM_TIM was not greater than zero'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Store the data
      DO J = 1, ESD_NUM_TIM
        FCDA_TIM(J)%TIME = ESD_TIM(J)%TIME
        FCDA_TIM(J)%COMP = ESD_TIM(J)%COMP
      END DO
!
!----------------------------------------------------------------------------------
!     Process data for FCDA locations
!----------------------------------------------------------------------------------
!
!     Allocate the space to store location data
      IF( ESD_NUM_LOC .GT. 0 ) THEN
        ALLOCATE( FCDA_LOC(ESD_NUM_LOC), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 3
          MESSAG(1) = 'Error allocating FCDA_LOC(ESD_NUM_LOC)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        IERR = 4
        MESSAG(1) = 'Could not allocate FCDA_LOC(ESD_NUM_LOC)'
        MESSAG(2) = 'ESD_NUM_LOC was not greater than zero'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Store the data
      DO J = 1, ESD_NUM_LOC
        FCDA_LOC(J)%ID   = ESD_LOC(J)%ID
        FCDA_LOC(J)%COMP = ESD_LOC(J)%COMP
        FCDA_LOC(J)%NAME = ESD_LOC(J)%NAME
      END DO
!
!----------------------------------------------------------------------------------
!     Process data for FCDA species
!----------------------------------------------------------------------------------
!
!     Allocate the space to store species data
      IF( ESD_NUM_SPC .GT. 0 ) THEN
        ALLOCATE( FCDA_SPC(ESD_NUM_SPC), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 5
          MESSAG(1) = 'Error allocating FCDA_SPC(ESD_NUM_SPC)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        IERR = 6
        MESSAG(1) = 'Could not allocate FCDA_SPC(ESD_NUM_SPC)'
        MESSAG(2) = 'ESD_NUM_SPC was not greater than zero'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Store the data
      DO J = 1, ESD_NUM_SPC
        FCDA_SPC(J)%ID   = ESD_SPC(J)%ID
        FCDA_SPC(J)%COMP = ESD_SPC(J)%COMP .AND. ESD_SPC(J)%OUTPUT
        FCDA_SPC(J)%NAME = ESD_SPC(J)%NAME
      END DO
!
!----------------------------------------------------------------------------------
!     Allocate space for the food maps
!----------------------------------------------------------------------------------
!
!     Aquatic species
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
!     Riparian species
      ALLOCATE( FCDA_MAP_RP(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for FCDA_MAP_RP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!     Set the entire vector to -1
      FCDA_MAP_RP = -1
!
!     Upland species
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
!----------------------------------------------------------------------------------
!     Generate the indices for all computed locations by species type and habitat
!     and overwrite the default -1 values in the map
!----------------------------------------------------------------------------------
!
! *** Do the computations for aquatic species and aquatic location
!     All aquatic species have the same indices (only one is needed to set the map)
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT.( ESD_SPC(ISPC)%COMP .AND. ESD_SPC(ISPC)%OUTPUT .AND. ESD_SPC(ISPC)%HABITAT.EQ.'AQUATIC' ) ) CYCLE
        NREC = FCDA_NHEAD
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT. (ESD_LOC(ILOC)%COMP .AND. ESD_LOC(ILOC)%TYPE.EQ.'AQUATIC') ) CYCLE
          NREC = NREC + 1
          FCDA_MAP_AQ(ILOC) = NREC
        END DO
        EXIT
      END DO
!     The number of records in a time block
      FCDA_BLOCK_AQ = NREC - FCDA_NHEAD
!
! *** Do the computations for riparian species and riparian locations
!     All terrestrial riparian species have the same indices (only one is needed to set the map)
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT.( ESD_SPC(ISPC)%COMP .AND. ESD_SPC(ISPC)%OUTPUT .AND. ESD_SPC(ISPC)%HABITAT.EQ.'RIPARIAN' ) ) CYCLE
        NREC = FCDA_NHEAD
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT. (ESD_LOC(ILOC)%COMP .AND. ESD_LOC(ILOC)%TYPE.EQ.'RIPARIAN') ) CYCLE
          NREC = NREC + 1
          FCDA_MAP_RP(ILOC) = NREC
        END DO
        EXIT
      END DO
!     The number of records in a time block
      FCDA_BLOCK_RP = NREC - FCDA_NHEAD
!
! *** Do the computations for upland species (account for soil types) and upland locations
!     All terrestrial upland species have the same indices (only one is needed to set the map)
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT.( ESD_SPC(ISPC)%COMP .AND. ESD_SPC(ISPC)%OUTPUT .AND. ESD_SPC(ISPC)%HABITAT.EQ.'UPLAND' ) ) CYCLE
        NREC = FCDA_NHEAD
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT. (ESD_LOC(ILOC)%COMP .AND. ESD_LOC(ILOC)%TYPE.EQ.'UPLAND') ) CYCLE
          IF( ESD_LOC(ILOC)%SODR ) THEN
            NREC = NREC + 1
            FCDA_MAP_UP(ILOC,IDXF_SODR) = NREC
          END IF
          IF( ESD_LOC(ILOC)%SOGW ) THEN
            NREC = NREC + 1
            FCDA_MAP_UP(ILOC,IDXF_SOGW) = NREC
          END IF
          IF( ESD_LOC(ILOC)%SOSW ) THEN
            NREC = NREC + 1
            FCDA_MAP_UP(ILOC,IDXF_SOSW) = NREC
          END IF
        END DO
        EXIT
      END DO
! *** The number of records in a time block
      FCDA_BLOCK_UP = NREC - FCDA_NHEAD
!
      RETURN
      END SUBROUTINE FCDA_MAPGEN
!
      SUBROUTINE FCDA_MAPWRITE( FNMAP, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine writes the ASCII FCDA map file.  The data for the
!!    map must have already been determined by a call to FCDA_MAPGEN.
!!
!!  Auxiliary Routines:
!!
!!    GET_UNIT_NUMBER - Gets a unit number to use when writing the
!!                      file.  The unit is closed at the end of the
!!                      writing activities.
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE FCDA_Mod
      USE ESD_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      CHARACTER(LEN=*) :: FNMAP   ! Name of the map file
      INTEGER :: IERR ! Error Number
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'FCDA_MAPWRITE' ! Name of this routine
      INTEGER :: IMAP ! Unit number for the record number map file
!
      INTEGER :: IOS  ! I/O Status from open, write, or close of file
      INTEGER :: J, K ! Index variables for writing
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
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
! *** Open the FCDA map file as a sequential ASCII file
      OPEN(IMAP,FILE=FNMAP,IOSTAT=IOS,STATUS='UNKNOWN')
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening FCDA map file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2),*) 'UNIT = ', IMAP, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNMAP = ' // TRIM(FNMAP)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Write character header lines
      WRITE(IMAP,ERR=7000,FMT=1000) TRIM(FCDA_PTITLE)
      WRITE(IMAP,ERR=7000,FMT=1000) TRIM(FCDA_PRGNAM)
      WRITE(IMAP,ERR=7000,FMT=1000) TRIM(FCDA_PRGVER)
      WRITE(IMAP,ERR=7000,FMT=1000) TRIM(FCDA_PRGDAT)
      WRITE(IMAP,ERR=7000,FMT=1000) TRIM(FCDA_USRNAM)
      WRITE(IMAP,ERR=7000,FMT=1000) TRIM(FCDA_CRUNID)
 1000 FORMAT ('"', A, '"')
!
! *** Write block size for the file
      WRITE(IMAP,ERR=7000,FMT=2000) FCDA_BLOCK_AQ, ',"Records in a time block - aquatic species"'
      WRITE(IMAP,ERR=7000,FMT=2000) FCDA_BLOCK_RP, ',"Records in a time block - riparian species"'
      WRITE(IMAP,ERR=7000,FMT=2000) FCDA_BLOCK_UP, ',"Records in a time block - upland species"'
 2000 FORMAT(I0,A)
!
! *** Write record length for the file
      WRITE(IMAP,ERR=7000,FMT=2000) FCDA_RECLEN, ',"Record length in the FCDA file"'
!
! *** Write number of realizations
      WRITE(IMAP,ERR=7000,FMT=2000) FCDA_NREAL, ',"Number of realizations"'
!
! *** Write number of header lines
      WRITE(IMAP,ERR=7000,FMT=2000) FCDA_NHEAD, ',"Number of header lines in binary files"'
!
! *** Write out time information
      WRITE(IMAP,ERR=7000,FMT=2000) ESD_NUM_TIM, ',"Number of times"'
      DO J = 1, ESD_NUM_TIM
        WRITE(IMAP,ERR=7000,FMT=2030) ESD_TIM(J)%TIME, ESD_TIM(J)%COMP
      END DO
 2030 FORMAT(I0,',',L1)
!
! *** Write out location information
      WRITE(IMAP,ERR=7000,FMT=2000) ESD_NUM_LOC, ',"Number of locations"'
      DO J = 1, ESD_NUM_LOC
        WRITE(IMAP,ERR=7000,FMT=2010) TRIM(ESD_LOC(J)%ID), ESD_LOC(J)%COMP, TRIM(ESD_LOC(J)%NAME)
      END DO
 2010 FORMAT('"',A,'",',L1,:,',"',A,'"')
!
! *** Write out food (species) information
      WRITE(IMAP,ERR=7000,FMT=2000) ESD_NUM_SPC, ',"Number of foods"'
      DO J = 1, ESD_NUM_SPC
        WRITE(IMAP,ERR=7000,FMT=2010) TRIM(ESD_SPC(J)%ID), ESD_SPC(J)%OUTPUT, TRIM(ESD_SPC(J)%NAME)
      END DO
!
! *** Write out the map index data for aquatic species
      WRITE(IMAP,ERR=8000,FMT=1000) 'Map: Aquatic Species'
      DO K = 1, ESD_NUM_LOC
        WRITE(IMAP,ERR=8000,FMT=2020) TRIM(ESD_LOC(K)%ID), FCDA_MAP_AQ(K)
 2020   FORMAT ('"',A,'",',I0)
      END DO
!
! *** Write out the map index data for riparian species
      WRITE(IMAP,ERR=8000,FMT=1000) 'Map: Riparian Species'
      DO K = 1, ESD_NUM_LOC
        WRITE(IMAP,ERR=8000,FMT=2020) TRIM(ESD_LOC(K)%ID), FCDA_MAP_RP(K)
      END DO
!
! *** Write out the map index data for upland locations
      WRITE(IMAP,ERR=8000,FMT=1000) 'Map: Upland Species'
      DO K = 1, ESD_NUM_LOC
        WRITE(IMAP,ERR=8000,FMT=2020) TRIM(ESD_LOC(K)%ID), FCDA_MAP_UP(K,1)
        WRITE(IMAP,ERR=8000,FMT=2020) TRIM(ESD_LOC(K)%ID), FCDA_MAP_UP(K,2)
        WRITE(IMAP,ERR=8000,FMT=2020) TRIM(ESD_LOC(K)%ID), FCDA_MAP_UP(K,3)
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
      CLOSE(IMAP)
      IERR = 3
      MESSAG(1) = 'Error writing header lines to the FCDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', IMAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FNMAP = ' // TRIM(FNMAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error branch: The map data were not written successfully
!
 8000 CONTINUE
      CLOSE(IMAP)
      IERR = 4
      MESSAG(1) = 'Error writing record number lines to the FCDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', IMAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FNMAP = ' // TRIM(FNMAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
      END SUBROUTINE FCDA_MAPWRITE
!
      SUBROUTINE FCDA_SACVIEW( FNSAC, FNKEY, FNMAP, PTITLE, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    header data for SACVIEW and writes data to the file.
!!
!!  Calling Requirements:
!!
!!    This subroutine must be preceeded by a call to FCDA_MAPGEN.
!!
!!  History:
!!
!!    Paul W. Eslinger : 20 Mar 2003 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Errors_Mod
      USE FCDA_Mod
      USE ESD_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FNSAC  ! File name for the FCDA file
      CHARACTER(LEN=*), INTENT(IN) :: FNKEY  ! Name of the input ECEM keyword file
      CHARACTER(LEN=*), INTENT(IN) :: FNMAP  ! File name for the FOOD record map data
      CHARACTER(LEN=*), INTENT(IN) :: PTITLE ! Problem title for this run of the cose
      INTEGER :: IERR                        ! Error numer (0=no errors)
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'FCDA_SACVIEW' ! Name of this subroutine
!
      INTEGER :: IERA ! Error indicator from the file system
      INTEGER :: IANA ! Local analyte index
      INTEGER :: ILOC ! Local location index
      INTEGER :: ISPC ! Local species index
      INTEGER :: ITIM ! Local time index
      INTEGER :: ICNT ! Local counter
!
      INTEGER :: ISAC ! Unit number for the FCDA file
!
!---- First executable code -------------------------------------------------
!
      IERR = 0
!
! *** Check for file name definition
!
      IF( FNSAC .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'File name not defined for the SACVIEW food header file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Get the unit number to use
!
      ISAC = GET_UNIT_NUMBER( )
      IF( ISAC .LT. 7 ) THEN
        IERR = 2
        MESSAG(1) = 'Could not obtain a free unit number for the SACVIEW food header file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the header file
!
      OPEN(ISAC,FILE=FNSAC,STATUS='UNKNOWN',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the SACVIEW header file'
        MESSAG(2) = 'File: "'//TRIM(FNSAC)//'"'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!---- Top level identification items ----------------------------------------
!
      WRITE(ISAC,1000,ERR=9999) 'type: ',     'FCDA'
      WRITE(ISAC,1000,ERR=9999) 'title: ',    TRIM(PTITLE)
      WRITE(ISAC,1000,ERR=9999) 'user: ',     TRIM(USRNAM)
      WRITE(ISAC,1000,ERR=9999) 'name: ',     TRIM(PRGNAM)
      WRITE(ISAC,1000,ERR=9999) 'version: ',  TRIM(PRGVER)
      WRITE(ISAC,1000,ERR=9999) 'date: ',     TRIM(PRGDAT)
      WRITE(ISAC,1000,ERR=9999) 'id: ',       TRIM(CRUNID)
      WRITE(ISAC,1000,ERR=9999) 'ecemfile: ', TRIM(FNKEY)
 1000 FORMAT(A,'"',A,'"')
!
! *** Write out number of realizations
!
      WRITE(ISAC,1010,ERR=9999) 'realizations: ', FCDA_NREAL
 1010 FORMAT(A,I0)
!
! *** Write out food information
!
      ICNT = 0
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. (ESD_SPC(ISPC)%COMP.AND.ESD_SPC(ISPC)%OUTPUT) ) CYCLE
        ICNT = ICNT + 1
      END DO
      WRITE(ISAC,1010,ERR=9999) 'foods: ', ICNT
      DO ISPC = 1, ESD_NUM_SPC
        IF( .NOT. (ESD_SPC(ISPC)%COMP.AND.ESD_SPC(ISPC)%OUTPUT) ) CYCLE
        WRITE(ISAC,1030,ERR=9999) TRIM(ESD_SPC(ISPC)%ID), ESD_SPC(ISPC)%TYPE, TRIM(ESD_SPC(ISPC)%NAME)
      END DO
!
! *** Time information
!
      ICNT = 0
      DO ITIM = 1, ESD_NUM_TIM
        IF( .NOT.ESD_TIM(ITIM)%COMP ) CYCLE
        ICNT = ICNT + 1
      END DO
      WRITE(ISAC,1010,ERR=9999) 'times: ', ICNT
      DO ITIM = 1, ESD_NUM_TIM
        IF( .NOT.ESD_TIM(ITIM)%COMP ) CYCLE
        WRITE(ISAC,1040,ERR=9999) ESD_TIM(ITIM)%TIME
 1040   FORMAT(I0)
      END DO
!
! *** Location information
!
      ICNT = 0
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
        ICNT = ICNT + 1
      END DO
      WRITE(ISAC,1010,ERR=9999) 'locations: ', ICNT
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
        WRITE(ISAC,1020,ERR=9999) TRIM(ESD_LOC(ILOC)%ID), TRIM(ESD_LOC(ILOC)%NAME)
 1020   FORMAT('"',A,'","',A,'"')
      END DO
!
! *** Analyte information
!
      ICNT = 0
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        ICNT = ICNT + 1
      END DO
      WRITE(ISAC,1010,ERR=9999) 'analytes: ', ICNT
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        WRITE(ISAC,1030,ERR=9999) TRIM(ESD_ANA(IANA)%ID), TRIM(ESD_ANA(IANA)%TYPE), TRIM(ESD_ANA(IANA)%NAME)
 1030   FORMAT('"',A,'","',A,'","',A,'"')
      END DO
!
! *** Record map file name
!
      WRITE(ISAC,1000,ERR=9999) 'recordmap: ', TRIM(FNMAP)
!
! *** Normal exit
!
      CLOSE( ISAC )
      RETURN
!
! *** Abnormal exit
!
 9999 CONTINUE
!
      IERR = 4
      MESSAG(1) = 'Error writing to the SACVIEW header file'
      CALL PRTERR( IERR, CALLER, 1 )
      CLOSE( ISAC )
      RETURN
!
      END SUBROUTINE FCDA_SACVIEW

