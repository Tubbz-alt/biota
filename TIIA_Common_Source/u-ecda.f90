!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
MODULE ECDA_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
!   Paul W. Eslinger :  9 Oct 2002 : Rev. 1 Version (together with Terri Miley)
!   Paul W. Eslinger :  6 Jun 2007 : Udate comments
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!===> Common data descriptions for the ECDA map and concentration files and the ECDA header file
!
!     Number of media for which the calculations are to be performed
      INTEGER, PARAMETER :: ECDA_NMED = 11
!
!     Number of header records in an ECDA file
      INTEGER, PARAMETER :: ECDA_NHEAD=ECDA_NMED+1
!
!     Flags by media controlling filling the ECDA concentration files (ECDA Code)
!       1 = Fixed value for all locations and times
!       2 = Random value for all locations and times
      INTEGER, DIMENSION(ECDA_NMED) :: FILL
!
!     Numerical values by media for filling the ECDA concentration files
      REAL(KIND=8), DIMENSION(ECDA_NMED) :: FILL_VALUE
!
!     Media indices - This order must match with IDs and descriptions defined below
      INTEGER, PARAMETER :: IGWAT =  1 ! Index to groundwater media (CFEST)
      INTEGER, PARAMETER :: ISEEP =  2 ! Index to seep water media (RIPSAC)
      INTEGER, PARAMETER :: ISWAT =  3 ! Index to surface water media (MASS2)
      INTEGER, PARAMETER :: IPWAT =  4 ! Index to pore water media index (MASS2)
      INTEGER, PARAMETER :: ISEDI =  5 ! Index to river bottom sediment media (MASS2)
      INTEGER, PARAMETER :: ISORP =  6 ! Index to riparian zone soil media (RIPSAC)
      INTEGER, PARAMETER :: ISODR =  7 ! Index to non-irrigated upland soil media (SOIL)
      INTEGER, PARAMETER :: ISOGW =  8 ! Index to groundwater irrigated upland soil media (SOIL)
      INTEGER, PARAMETER :: ISOSW =  9 ! Index to surface water irrigated upland soil media (SOIL)
      INTEGER, PARAMETER :: IAIRC = 10 ! Index to air concentration media (RATCHET)
      INTEGER, PARAMETER :: IAIRD = 11 ! Index to air deposition media (RATCHET)
!
!     Media IDs - Order must match the media indices defined above
      CHARACTER(LEN=4), DIMENSION(ECDA_NMED), PARAMETER :: ECDA_ID_MED &
        = (/'GWAT','SEEP','SWAT','PWAT','SEDI','SORP','SODR','SOGW','SOSW','AIRC','AIRD'/)
!
!     Media descriptions - Order must match the media indices defined above
      CHARACTER(LEN=50), DIMENSION(ECDA_NMED), PARAMETER :: ECDA_DESC_MED &
        = (/'Groundwater concentrations                        ',&
            'Seep water (riparian zone) concentrations         ',&
            'Surface water (river) concentrations              ',&
            'Pore water (river bottom) concentrations          ',&
            'Sediment (river bottom) concentrations            ',&
            'Soil (riparian zone) concentrations               ',&
            'Non-irrigated upland soil concentrations          ',&
            'Groundwater irrigated upland soil concentrations  ',&
            'Surface water irrigated upland soil concentrations',&
            'Air concentrations                                ',&
            'Air deposition rates                              '/)
!
!===> Header information in the binary concentration file
!
!     Analyte ID:  First header line
      CHARACTER(LEN=6) :: ECDA_ANALYTE
!
!     Media units: Header lines 2 through ECDA_NMED+1
      CHARACTER(LEN=16), DIMENSION(ECDA_NMED) :: ECDA_UNITS
!
!===> Information in the ASCII record map file
!
      INTEGER :: ECDA_BLOCK  ! Number of records with data in a time block
      INTEGER :: ECDA_RECLEN ! Record length of records in the data file
      INTEGER :: ECDA_NREAL  ! Number of realizations in the data file
      INTEGER :: ECDA_NTIMES ! Number of times at which the calculations are to be performed
      INTEGER :: ECDA_NLOCS  ! Number of locations at which the calculations are to be performed
!
      CHARACTER(LEN=200) :: ECDA_PTITLE ! Problem title line from generating program
      CHARACTER(LEN= 10) :: ECDA_PRGNAM ! Program name of generating program
      CHARACTER(LEN=  8) :: ECDA_PRGVER ! Program version number of generating program
      CHARACTER(LEN= 12) :: ECDA_PRGDAT ! Program date of generating program
      CHARACTER(LEN= 16) :: ECDA_USRNAM ! User name from generating program
      CHARACTER(LEN= 14) :: ECDA_CRUNID ! Run identification number from generating program
!
      INTEGER, ALLOCATABLE :: ECDA_TIMES(:)            ! (ECDA_NTIMES) Times for the calculations
      CHARACTER(LEN= 6), ALLOCATABLE :: ECDA_ID_LOC(:) ! (ECDA_NLOCS) Location IDs for the calculations
      CHARACTER(LEN=72), ALLOCATABLE :: ECDA_NM_LOC(:) ! (ECDA_NLOCS) Location names for the calculations
      INTEGER, ALLOCATABLE :: ECDA_LOC_MED(:,:)        ! (ECDA_NLOCS,ECDA_NMED) Record numbers for all
!                                                        location and media for the first time step
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE ECDA_Mod
!
      SUBROUTINE ECDA_CREATE( FN_ECDA, UN_ECDA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine creates an ECDA file and writes the header lines.
!!
!!  History:
!!
!!    Kelly Lessor     : 20 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 26 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE ECDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FN_ECDA ! File name for the ECDA file
      INTEGER, INTENT(IN) :: UN_ECDA          ! Unit number for the ECDA file
      INTEGER :: IERR                         ! Error Number
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'ECDA_CREATE' ! Name of this routine
      INTEGER :: IOS ! I/O Status
      INTEGER :: J   ! Index variable for writing
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
!     Open the ECDA file
!
      OPEN(UNIT=UN_ECDA, IOSTAT=IOS, ERR=1000, FILE=FN_ECDA, STATUS='REPLACE', ACCESS='DIRECT', RECL=ECDA_RECLEN)
!
!     Write the header lines to the ECDA file
!
      WRITE(UNIT=UN_ECDA, IOSTAT=IOS, REC=1, ERR=2000) ECDA_ANALYTE
      DO J = 1, ECDA_NMED
        WRITE(UNIT=UN_ECDA, IOSTAT=IOS, REC=(J+1), ERR=2000) ECDA_UNITS(J)
      END DO
      RETURN
!
!     An error occurred opening the ECDA file
!
 1000 CONTINUE
      IF( IOS .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'An error occurred opening the ECDA file'
        MESSAG(2) = 'File name is ' // TRIM(FN_ECDA)
        WRITE(MESSAG(3), FMT='(A,I0)') 'IOSTAT = ', IOS
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
!     An error occurred writing the header to the ECDA file
!
 2000 CONTINUE
      IF ( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'An error occurred writing header information to the ECDA file'
        MESSAG(2) = 'File name is ' // TRIM(FN_ECDA)
        WRITE(MESSAG(3), FMT='(A,I0)') 'IOSTAT = ', IOS
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ECDA_MAPOUT( FN_MAP, UN_MAP, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine writes the ASCII ECDA map file.  The data for the
!!    map must have already been determined by a call to ECDA_MAPGEN.
!!
!!  Operating system:
!!
!!    This subroutine is coded for a PC using a FORTRAN 95 compiler.
!!
!!  History:
!!
!!    Kelly Lessor     : 17 May 2000 : Version 1.0
!!    Paul W. Eslinger : 26 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE ECDA_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FN_MAP ! File name for the map file
      INTEGER, INTENT(IN) :: UN_MAP          ! Unit number for the map file
      INTEGER :: IERR         ! Error Number
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'ECDA_MAPOUT' ! Name of this routine
!
      INTEGER :: IOS   ! I/O Status from open, write, or close of file
      INTEGER :: J, K  ! Index variables for writing
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Open the ECDA map file as a sequential ASCII file
!
      OPEN(UNIT=UN_MAP,FILE=FN_MAP,IOSTAT=IOS,STATUS='UNKNOWN')
      IF( IOS .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error opening ECDA map file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2),*) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
        MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Write character header lines
!
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_PTITLE)
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_PRGNAM)
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_PRGVER)
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_PRGDAT)
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_USRNAM)
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_CRUNID)
 1000 FORMAT ('"', A, '"')
!
! *** Write block size for the file
!
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=2000) ECDA_BLOCK, ',"Records in a time block"'
 2000 FORMAT(I0,A)
!
! *** Write record length for the file
!
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=2000) ECDA_RECLEN, ',"Record length in the ECDA file"'
!
! *** Write number of realizations
!
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=2000) ECDA_NREAL, ',"Number of realizations"'
!
! *** Write out time header information
!
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=2000) ECDA_NTIMES, ',"Number of times"'
      DO J = 1, ECDA_NTIMES
        WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=2000) ECDA_TIMES(J)
      END DO
!
! *** Write out location header information
!
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=2000) ECDA_NLOCS, ',"Number of locations"'
      DO J = 1, ECDA_NLOCS
        WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_ID_LOC(J))
      END DO
!
! *** Write out media header information
!
      WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=2000) ECDA_NMED, ',"Number of media"'
      DO J = 1, ECDA_NMED
        WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=7000,FMT=1000) TRIM(ECDA_ID_MED(J))
      END DO
!
! *** Write out the map index data
!
      DO K = 1, ECDA_NLOCS
        WRITE(UNIT=UN_MAP,IOSTAT=IOS,ERR=8000,FMT=3000) TRIM(ECDA_ID_LOC(K)), (ECDA_LOC_MED(K,J),J=1,ECDA_NMED)
 3000   FORMAT ('"',A,'"',24(',',I0,:))
      END DO
!
! *** Close the ECDA map file and return (normal exit point)
!
      CLOSE(UNIT=UN_MAP)
      RETURN
!
! *** Error branch: The header lines were not written successfully
!
 7000 CONTINUE
      IERR = 2
      MESSAG(1) = 'Error writing header lines to the ECDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error branch: The map data were not written successfully
!
 8000 CONTINUE
      IERR = 3
      MESSAG(1) = 'Error writing record number lines to the ECDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error branch: The ECDA map file was not closed successfully
!
 9000 CONTINUE
      IERR = 4
      MESSAG(1) = 'Error closing ECDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE ECDA_MAPECHO( IRPT )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine write the header information from the ECDA map
!!    file to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  1 Feb 2001 : Version 1.0
!!    Paul W. Eslinger : 18 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger : 31 May 2007 : Add IRPT to call list
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE ECDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IRPT ! Unit number for the output file
!
!---- Executable code ---------------------------------------------------
!
      WRITE(IRPT,3000) 'Header information from the map file'
      WRITE(IRPT,3010) 'ECDA_PTITLE = ', TRIM(ECDA_PTITLE)
      WRITE(IRPT,3010) 'ECDA_PRGNAM = ', TRIM(ECDA_PRGNAM)
      WRITE(IRPT,3010) 'ECDA_PRGVER = ', TRIM(ECDA_PRGVER)
      WRITE(IRPT,3010) 'ECDA_PRGDAT = ', TRIM(ECDA_PRGDAT)
      WRITE(IRPT,3010) 'ECDA_USRNAM = ', TRIM(ECDA_USRNAM)
      WRITE(IRPT,3010) 'ECDA_CRUNID = ', TRIM(ECDA_CRUNID)
      WRITE(IRPT,3020) 'ECDA_BLOCK  = ', ECDA_BLOCK
      WRITE(IRPT,3020) 'ECDA_RECLEN = ', ECDA_RECLEN
      WRITE(IRPT,3020) 'ECDA_NREAL  = ', ECDA_NREAL
 3000 FORMAT(/A)
 3010 FORMAT(3X,A,A)
 3020 FORMAT(3X,A,I0)
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
!!  Operating system:
!!
!!    This subroutine is coded for a PC using a FORTRAN 95 compiler.
!!
!!  History:
!!
!!    Kelly Lessor     : 17 May 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  5 Jun 2007 : Add deallocate logic for to
!!                                     support multiple calls
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE ECDA_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FN_MAP ! File name for the map file
      INTEGER, INTENT(IN)    :: UN_MAP       ! Unit number for the map file
      INTEGER, INTENT(INOUT) :: IERR         ! Error number
!
! *** Local variables
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
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_NREAL
!
! *** Read the time information
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_NTIMES
      IF( ECDA_NTIMES .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) ='ECDA_NTIMES must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      IF( ALLOCATED(ECDA_TIMES) ) DEALLOCATE( ECDA_TIMES )
!
      ALLOCATE( ECDA_TIMES(ECDA_NTIMES), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) ='Error allocating ECDA_TIMES(ECDA_NTIMES)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      DO J = 1, ECDA_NTIMES
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_TIMES(J)
      END DO
!
! *** Read the location information
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_NLOCS
      IF( ECDA_NLOCS .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) ='ECDA_NLOCS must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      IF( ALLOCATED(ECDA_ID_LOC) ) DEALLOCATE( ECDA_ID_LOC )
!
       ALLOCATE( ECDA_ID_LOC(ECDA_NLOCS), STAT= IOS )
       IF( IOS .NE. 0 ) THEN
         IERR = 5
         MESSAG(1) ='Error allocating ECDA_ID_LOC(ECDA_NLOCS)'
         CALL PRTERR( IERR, CALLER, 1 )
         RETURN
       END IF
!
         DO J = 1, ECDA_NLOCS
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_ID_LOC(J)
      END DO
!
! *** Read and discard the media information
!     These will be used by SACView but are hard-coded in this program
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) NUM_TMP
      IF( NUM_TMP .LT. 1 ) THEN
        IERR = 6
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
      IF( ALLOCATED(ECDA_LOC_MED) ) DEALLOCATE( ECDA_LOC_MED )
!
      ALLOCATE( ECDA_LOC_MED(ECDA_NLOCS,ECDA_NMED), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) ='Error allocating ECDA_LOC_MED(ECDA_NLOCS,ECDA_NMED)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      DO K = 1, ECDA_NLOCS
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=8000, FMT=*) TMP_ID, (ECDA_LOC_MED(K,J),J=1,ECDA_NMED)
        IF( TMP_ID .NE. ECDA_ID_LOC(K) ) THEN
          IERR = 8
          MESSAG(1) = 'Mismatch on header location ID and record index ID tag'
          MESSAG(2) = 'Location ID from header = ' // TRIM(ECDA_ID_LOC(K))
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
      IERR = 9
      MESSAG(1) = 'Error reading header lines from the ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error Branch: The map data were not read successfully
!
 8000 CONTINUE
      IERR = 10
      MESSAG(1) = 'Error reading record number lines from the ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error Branch: The ECDA map file was not closed successfully
!
 9000 CONTINUE
      IERR = 11
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
!!  Operating system:
!!
!!    This subroutine is coded for a PC using a FORTRAN 95 compiler.
!!
!!  History:
!!
!!    Kelly Lessor     : 27 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  5 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE ECDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FN_ECDA ! File name for the ECDA file
      INTEGER, INTENT(IN)  :: UN_ECDA ! Unit number for the ECDA file
      INTEGER :: IERR    ! Error Number
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'ECDA_OPEN' ! Name of this routine
      INTEGER :: IOS ! I/O Status
      INTEGER :: J   ! Index variable for writing
!
!---- Executable code ---------------------------------------------------
!
! *** Open the ECDA file
!
      OPEN( UNIT=UN_ECDA, IOSTAT=IOS, ERR=1000, FILE=FN_ECDA, STATUS='OLD', ACCESS='DIRECT', RECL=ECDA_RECLEN)
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
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine reads a single data record from an open ECDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  2 Oct 2002 : SAC Rev. 1
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
      INTEGER, INTENT(IN) :: NREC  ! Record number
      INTEGER :: TIME              ! Calendar year for concentration data
      CHARACTER(LEN=*) :: LOC_ID   ! Location ID
      CHARACTER(LEN=*) :: MED_ID   ! Media ID
      REAL, DIMENSION(*) :: CONC   ! Concentration vector of length NREAL
      INTEGER, INTENT(IN) :: NREAL ! Number of realizations to write
      INTEGER, INTENT(IN) :: ICON  ! Fortran unit number to use for output
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'ECDA_READ'
!
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
      END SUBROUTINE ECDA_READ
!
      SUBROUTINE ECDA_RECNO( TIME, LOC_ID, MED_ID, NREC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine returns a record number in the data file given the
!!    time, location ID, and media ID.
!!
!!  History:
!!
!!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Ecda_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: TIME ! Calendar year for concentration data
      CHARACTER(LEN=*), INTENT(IN) :: LOC_ID   ! Location ID
      CHARACTER(LEN=*), INTENT(IN) :: MED_ID   ! Media ID
      INTEGER :: NREC ! Record number
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'ECDA_RECNO'
      INTEGER :: I    ! Looping index
      INTEGER :: ITIM ! Time index
      INTEGER :: ILOC ! Location index
      INTEGER :: IMED ! Media index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Find the time index
!
      ITIM = 0
      DO I = 1, ECDA_NTIMES
        IF( TIME .EQ. ECDA_TIMES(I) ) THEN
          ITIM = I
          EXIT
        END IF
      END DO
      IF( ITIM .EQ. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'No match for the time value'
        MESSAG(2) = 'Requested time was '
        WRITE(MESSAG(2)(21:),*) TIME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Find the location index
!
      ILOC = 0
      DO I = 1, ECDA_NLOCS
        IF( LOC_ID .EQ. ECDA_ID_LOC(I) ) THEN
          ILOC = I
          EXIT
        END IF
      END DO
      IF( ILOC .EQ. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'No match for the location ID'
        MESSAG(2) = 'Requested location ID was ' // LOC_ID
        RETURN
      END IF
!
! *** Find the media index
!
      IMED = 0
      DO I = 1, ECDA_NMED
        IF( MED_ID .EQ. ECDA_ID_MED(I) ) THEN
          IMED = I
          EXIT
        END IF
      END DO
      IF( IMED .EQ. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'No match for the media ID'
        MESSAG(2) = 'Requested media ID was ' // MED_ID
        RETURN
      END IF
!
! *** Compute the concentration index
!
      NREC = ECDA_LOC_MED(ILOC,IMED)
      IF( NREC .LT. 0 ) RETURN
!
      NREC = (ITIM-1)*ECDA_BLOCK + NREC
!
      RETURN
      END SUBROUTINE ECDA_RECNO
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
!!    Paul W. Eslinger :  2 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Ecda_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: IMED ! Media index
      INTEGER :: NREC             ! Record number
      INTEGER :: IERR             ! Error number indicator
!
! *** Local variables
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
      END SUBROUTINE ECDA_RECNO_INDEX
!
      SUBROUTINE ECDA_WRITE( NREC, TIME, LOC_ID, MED_ID, CONC, NREAL, ICON, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine writes a single data record to an open ECDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
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
      CHARACTER(LEN=*), INTENT(IN) :: MED_ID ! Media ID
      REAL, DIMENSION(*), INTENT(IN) :: CONC ! Concentration vector of length NREAL
      INTEGER, INTENT(IN) :: NREAL ! Number of realizations to write
      INTEGER, INTENT(IN) :: ICON  ! Fortran unit number to use for output
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'ECDA_WRITE'
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
      WRITE( UNIT=ICON, IOSTAT=IOS, REC=NREC ) TIME, LOC_ID, MED_ID, (CONC(IREL),IREL=1,NREAL)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error writing to ECDA concentration file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2),'(A,I3,A,I5)') 'UNIT = ',ICON, ' IOSTAT = ',IOS
        CALL PRTERR(IERR, CALLER, 2)
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

