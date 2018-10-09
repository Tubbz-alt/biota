!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
      PROGRAM ECDA_ASCII
!!**************************************************************************************************
!!
!!                          ECDA_ASCII - ECDA Format Conversion Program
!!              Battelle, Pacific Northwest National Laboratory, Richland, Washington
!!
!!**************************************************************************************************
!!
!!  ECDA is the top level routine for environmental concentration data file
!!  handling of the Toolkit for Integrated Impact Assessments (TIIA).
!!
!!  This code reads an ECDA file in binary format and writes it to another
!!  file in ASCII format.
!!
!!  History:
!!    Jim C. Brown & Paul W. Eslinger : 19 May 2000 : Version 1.0
!!    Paul W. Eslinger : 28 Feb 2003 : SAC, Rev. 1 version
!!    Paul W. Eslinger : 25 Sep 2006 : Make single location version
!!    Paul W. Eslinger : 31 May 2007 : Modify to TIAA version
!!    Paul W. Eslinger : 9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!  Language:
!!    This program was written in Fortran 95 (free source form).
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!**************************************************************************************************
!
      IMPLICIT NONE
!
! *** External functions
      LOGICAL, EXTERNAL :: STRCOMP
!
! *** Local variables
!
!     Program identification
      CHARACTER(LEN=10) :: PRGNAM = 'ECDA_ASCII'  ! Name of this program
      CHARACTER(LEN= 9) :: PRGVER = '4.00.001'    ! Version of this program
      CHARACTER(LEN=10) :: PRGDAT = '07/09/2012' ! Date of this program
!
!     File unit numbers
      INTEGER :: IBIN ! Unit number for input binary file
      INTEGER :: IASC ! Unit number for output ASCII file
!
!     File names
      CHARACTER(LEN=256) :: FN_ASC ! File name for the output ASCII file
      CHARACTER(LEN=256) :: FN_BIN ! File name for the input binary file
!
!     Record information
      INTEGER :: OUT_REC       ! Number of records output
      INTEGER :: RECORD_LENGTH ! Record length of the binary file
!
!     Header information from the ECDA file
      INTEGER :: IHEAD            ! Header line index
      INTEGER :: NUM_HEAD = 12    ! Number of header lines
      CHARACTER(LEN=16) :: HEADER ! Single header line
!
!     User selections
      INTEGER :: NUM_REC ! Number of records to process
      INTEGER :: NREAL   ! Number of realizations in the file
      CHARACTER(LEN=4) :: MED_USE ! Media ID to use in output
      CHARACTER(LEN=6) :: LOC_USE ! Location ID to use in output
      CHARACTER(LEN=6) :: LOC_TMP ! Location ID to check for ALL locations
!
!     Looping variables
      INTEGER :: IREC ! Looping variable for record numbers
      INTEGER :: IREL ! Looping variable for realizations
!
!     Data from a record in the input file
      INTEGER :: TIME              ! Year
      CHARACTER(LEN=6) :: LOC_ID   ! Location ID
      CHARACTER(LEN=4) :: MED_ID   ! Media ID
      REAL, ALLOCATABLE :: CONC(:) ! Concentration vector (length NREAL)
!
!     Local stuff
      LOGICAL :: THERE ! Logical variable for file exist inquiry
      INTEGER :: IERR  ! Integer error flag
      CHARACTER(LEN=1) :: ANSWER ! Character answer string
!
!---- Executable code ---------------------------------------------------
!
! *** Banner to the screen
      CALL QA_CopyRightSingle( 6, .FALSE. )
      WRITE(*,1010) PRGNAM, PRGVER, PRGDAT
 1010 FORMAT( 1X,'-------------------------------------------------'/&
              1X,1X,A,1X,A,'  Last Modified: ',A/                   &
              1X,'-------------------------------------------------')
!
!     Initialize the number of output records
      OUT_REC = 0
!
! *** Get the file name of the ECDA file
      WRITE(*,*) ' Enter the name of the input binary ECDA file > '
      READ(*,*) FN_BIN
!
! *** Get the output file name
      WRITE(*,*) ' Enter the name of the output ASCII file > '
      READ(*,*) FN_ASC
!
! *** Stop if the file names are the same
      IF( FN_ASC .EQ. FN_BIN ) THEN
        WRITE(*,*) ' The input binary and output ASCII file names are identical'
        WRITE(*,*) ' Start over with a different file names'
        STOP
      END IF
!
      IF( STRCOMP(FN_ASC,FN_BIN,256) ) THEN
        WRITE(*,*) ' The input binary and output ASCII file names are the same except for case differences'
        WRITE(*,*) ' Do you wish to continue with these file names?'
        WRITE(*,*) ' The input file will be destroyed on a Windows operating system.'
        WRITE(*,*) ' Y = Yes, N = No > '
        READ(*,*) ANSWER
        IF( .NOT.STRCOMP(ANSWER,'Y',1) ) STOP
      END IF
!
! *** Check if the requested file exists
      INQUIRE(FILE=FN_BIN,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        WRITE(*,*) ' The requested data file was not found'
        WRITE(*,*) ' File: ' // TRIM(FN_BIN)
        WRITE(*,*) ' Start over with a valid file name'
        STOP
      END IF
!
! *** Get the number of realizations (to determine the record length in the file)
      WRITE(*,*) ' Enter the number of realizations in the file > '
      READ(*,*,ERR=9999) NREAL
      IF( NREAL .LT. 1 ) THEN
        WRITE(*,*) ' The number of realizations must be greater than 0'
        STOP
      END IF
!
! *** Compute the record length of the binary file
      RECORD_LENGTH = 4 + 6 + 4 + 4*NREAL
!
!     Allocate the memory needed to read the data
      ALLOCATE( CONC(NREAL), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) ' System error allocating memory for CONC'
        STOP
      END IF
!
! *** Get the media to translate
      WRITE(*,*) ' Enter the ID for the media to translate (ALL for all media) > '
      READ(*,*) MED_USE
! *** Make the ALL not case sensitive (all media ID's are upper case)
      CALL UPCASE( MED_USE )
!
! *** Get the location to translate
!
      WRITE(*,*) ' Enter the ID for the location to translate (ALL for all locations) > '
      READ(*,*) LOC_USE
!
! *** Make the ALL not case sensitive (not changing other location ID's)
      LOC_TMP = LOC_USE
      CALL UPCASE( LOC_TMP )
      IF( LOC_TMP .EQ. 'ALL' ) LOC_USE = 'ALL'
!
! *** Get the number of data records to process
      WRITE(*,*) ' Enter the number of data records to translate > '
      READ(*,*) NUM_REC
      IF( NUM_REC .LT. 1 ) THEN
        WRITE(*,*) ' The number of records to process must be greater than 0'
        STOP
      END IF
!
! *** Open the binary file
      IBIN = 11
      OPEN(IBIN,FILE=FN_BIN,RECL=RECORD_LENGTH,ACCESS='DIRECT',STATUS='OLD')
!
! *** Open the output ASCII file
      IASC = 12
      OPEN(IASC,FILE=FN_ASC)
!     Output the input file name
      WRITE(IASC,*) 'File: ',TRIM(FN_BIN)
!
! *** Read the header information and report it to the ASCII file
      HEADER = ' '
      DO IHEAD = 1, NUM_HEAD
        READ(IBIN,REC=IHEAD) HEADER
        IF( IHEAD .EQ. 1 ) THEN
          WRITE(IASC,1020) IHEAD, TRIM(HEADER(1:6))
        ELSE
          WRITE(IASC,1020) IHEAD, TRIM(HEADER)
        END IF
 1020   FORMAT(1X,I8,1X,A)
      END DO
!
! *** Loop over the records in the file and translate requested data
      DO IREC = 1, NUM_REC
        READ(IBIN,REC=(NUM_HEAD+IREC),ERR=9999) TIME, LOC_ID, MED_ID, (CONC(IREL),IREL=1,NREAL)
!        WRITE(*,*) TIME, LOC_ID, ' ', MED_ID, (CONC(IREL),IREL=1,NREAL)
        OUT_REC = OUT_REC + 1
        IF( MED_USE .NE. 'ALL' ) THEN
          IF( MED_USE .NE. MED_ID ) CYCLE
        END IF
        IF( LOC_USE .NE. 'ALL' ) THEN
          IF( LOC_USE .NE. LOC_ID ) CYCLE
        END IF
        WRITE(IASC,1000) (NUM_HEAD+IREC), TIME, LOC_ID, MED_ID, (CONC(IREL),IREL=1,NREAL)
 1000   FORMAT(1X,I8,1X,I7,1X,A,1X,A,1P,1000(1X,E13.6))
      END DO
!
! *** Normal termination
!
      WRITE(*,*) ' Number of data records read was ', OUT_REC
      CLOSE(IBIN)
      CLOSE(IASC)
      STOP
!
! *** End of file or error reading the file
!
 9999 CONTINUE
!
      WRITE(*,*) ' Number of data records read was ', OUT_REC
      CLOSE(IBIN)
      CLOSE(IASC)
      STOP
!
      END PROGRAM

