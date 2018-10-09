!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
      PROGRAM FCDA_ASCII
!!**************************************************************************************************
!!
!!                  FCDA_ASCII - FCDA Binary to ASCII Conversion Program
!!          Battelle, Pacific Northwest National Laboratory,  Richland, Washington
!!
!!**************************************************************************************************
!!
!!  The ECEM code computes environmental species impacts and food
!!  product concentrations for the HUMAN code in the Toolkit for
!!  Integrated Impacts Assessments (TIIA).
!!
!!  This code reads an FCDA concentration file in binary format and
!!  writes it to another file in ASCII format.
!!
!!  Module History:
!!    Paul W. Eslinger :  5 Mar 2003 : SAC, Rev. 1 version
!!    Paul W. Eslinger : 21 Sep 2006 : Add location selection option
!!    Paul W. Eslinger :  4 Jun 2007 : Modify to TIAA version
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!  Language and Compiler:
!!    This program is written in Fortran 95 (free source form).
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
      CHARACTER(LEN=10) :: PRGNAM = 'FCDA_ASCII' ! Name of this program
      CHARACTER(LEN= 8) :: PRGVER = '4.00.001'   ! Version of this program
      CHARACTER(LEN=10) :: PRGDAT = '09/09/2012' ! Date of this program
!
!     File unit numbers
      INTEGER :: IASC    ! Unit number for output ASCII file
      INTEGER :: IBIN    ! Unit number for input binary file
!
!     File names
      CHARACTER(LEN=256) :: FN_ASC ! File name for the output ASCII file
      CHARACTER(LEN=256) :: FN_BIN ! File name for the input binary file
!
!     User selections
      INTEGER :: NUM_REC          ! Number of records to process
      CHARACTER(LEN=6) :: LOC_USE ! Location ID to use in output
      CHARACTER(LEN=6) :: LOC_TMP ! Location ID to use in output
!
!     Data sizes
      INTEGER :: NREAL         ! Number of realizations in the input file
      INTEGER :: RECORD_LENGTH ! Record length of the binary file
      INTEGER :: OUT_REC       ! Number of records read
!
!     Looping variables
      INTEGER :: IREC ! Looping variable for record numbers
      INTEGER :: IREL ! Looping variable for realizations
!
!     File header data
      INTEGER :: NUM_HEAD = 3     ! Number of header lines
      CHARACTER(LEN=6) :: HEADER_1  ! Header line #1
      CHARACTER(LEN=6) :: HEADER_2  ! Header line #2
      CHARACTER(LEN=8) :: HEADER_3  ! Header line #3
!
!     Data from a record in the input file
      INTEGER :: TIME              ! Year from the file
      CHARACTER(LEN=6) :: LOC_ID   ! Location ID from the file
      REAL, ALLOCATABLE :: CONC(:) ! Concentration vector (length NREAL)
!
!     Local stuff
      LOGICAL :: THERE ! Logical flag for file existence inquire
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
!
!     Initialize the number of output records
      OUT_REC = 0
!
! *** Get the food file name
      WRITE(*,*) ' Enter the name of the input binary FCDA file > '
      READ(*,*) FN_BIN
!
!     Determine whether the file exists
      INQUIRE( FILE=FN_BIN, EXIST=THERE )
      IF( .NOT.THERE ) THEN
        WRITE(*,*) ' The requested food file does not exist'
        WRITE(*,*) ' File: ' // TRIM(FN_BIN)
        WRITE(*,*) ' Start over with a valid file name'
        STOP
      END IF
!
! *** Get the output file name
      WRITE(*,*) ' Enter the name of the output ASCII file > '
      READ(*,*,ERR=9999) FN_ASC
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
! *** Get the number of realizations (to determine the record length in the file)
      WRITE(*,*) ' Enter the number of realizations in the file > '
      READ(*,*,ERR=9999) NREAL
      IF( NREAL .LT. 1 ) THEN
        WRITE(*,*) ' The number of realizations must be greater than 0'
        STOP
      END IF
!
! *** Set the record length of the binary food file
      RECORD_LENGTH = 6 + 4 + 4*NREAL
!
!     Allocate the memory needed to read the data
      ALLOCATE( CONC(NREAL), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) ' System error allocating memory for CONC'
        STOP
      END IF
!
! *** Get the location(s) to translate (make the ALL not case sensitive)
      WRITE(*,*) ' Enter the location ID desired (ALL for every location in the file)> '
      READ(*,*,ERR=9999) LOC_USE
!
! *** Make the ALL not case sensitive
      LOC_TMP = LOC_USE
      CALL UPCASE( LOC_TMP )
      IF( LOC_TMP .EQ. 'ALL' ) LOC_USE = 'ALL'
!
! *** Get the number of data records to process
      WRITE(*,*) ' Enter the number of data records to translate > '
      READ(*,*,ERR=9999) NUM_REC
      IF( NUM_REC .LT. 1 ) THEN
        WRITE(*,*) ' The number of records to process must be greater than 0'
        STOP
      END IF
!
! *** Open the binary food file
      IBIN = 11
      OPEN(IBIN,FILE=FN_BIN,RECL=RECORD_LENGTH,ACCESS='DIRECT',STATUS='OLD',ERR=9999)
!
! *** Open the output ASCII file
      IASC = 12
      OPEN(IASC,FILE=FN_ASC,ERR=9999)
      WRITE(IASC,1040,ERR=9999) 'File: '//TRIM(FN_BIN)
 1040 FORMAT(A)
!
! *** Read the header information and report it to the ASCII file
      IREC = 1
      READ(UNIT=IBIN,REC=IREC,ERR=9999) HEADER_1
      WRITE(IASC,1020,ERR=9999) IREC, TRIM(HEADER_1)
!
      IREC = IREC + 1
      READ(UNIT=IBIN,REC=IREC) HEADER_2
      WRITE(IASC,1020,ERR=9999) IREC, TRIM(HEADER_2)
!
      IREC = IREC + 1
      READ(UNIT=IBIN,REC=IREC,ERR=9999) HEADER_3
      WRITE(IASC,1020,ERR=9999) IREC, TRIM(HEADER_3)
 1020 FORMAT(1X,I8,1X,A)
!
! *** Loop over the records in the file and translate requested data
      DO IREC = 1, NUM_REC
        READ(UNIT=IBIN,REC=(NUM_HEAD+IREC),ERR=9999) TIME, LOC_ID, (CONC(IREL),IREL=1,NREAL)
!        WRITE(*,*) TIME, LOC_ID, CONC
        OUT_REC = OUT_REC + 1
        IF( LOC_USE.EQ.'ALL' ) THEN
          WRITE(IASC,1030,ERR=9999) (NUM_HEAD+IREC), TIME, LOC_ID, (CONC(IREL),IREL=1,NREAL)
        ELSE
          IF( LOC_USE .EQ. LOC_ID ) WRITE(IASC,1030,ERR=9999) (NUM_HEAD+IREC), TIME, LOC_ID, (CONC(IREL),IREL=1,NREAL)
        END IF
 1030   FORMAT(1X,I8,1X,I7,1X,A,1P,5000(1X,E12.5))
      END DO
!
! *** Normal termination point
      WRITE(*,*) ' Number of data records read was ', OUT_REC
      CLOSE(IBIN)
      CLOSE(IASC)
      STOP
!
! *** Branch for read error
 9999 CONTINUE
      WRITE(*,*) ' Number of data records read was ', OUT_REC
      CLOSE(IBIN)
      CLOSE(IASC)
!
      STOP
      END PROGRAM

