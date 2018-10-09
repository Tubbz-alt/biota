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
!    Paul W. Eslinger :  6 Jun 2002 : Version 2.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!
!  Code Block:
!
      CHARACTER(LEN=200) :: PTITLE  ! Title for this program run
      CHARACTER(LEN=  6) :: ANA_ID  ! Analyte type to extract
      CHARACTER(LEN=  6) :: SOL_ID  ! Solution type to extract
      CHARACTER(LEN=  4) :: SOIL_ID ! Soil type type to extract
!
      CHARACTER(LEN=6), ALLOCATABLE :: LOC_USE(:) ! Locations to process
      INTEGER :: NUM_LOC                          ! Number of locations to process
!
      INTEGER :: NREAL   ! Total number of realizations in the ECEM data file
      INTEGER :: NUM_REL ! Realization number to extract
      INTEGER :: NUM_TAB ! Number of table rows for each extraction
      INTEGER :: NUM_REC ! Number of records data rows for each extraction
      INTEGER :: NUM_VAL ! Number of values for an extraction
      LOGICAL :: MEAN    ! Flag for using the average instead of a single realization
!
      INTEGER, ALLOCATABLE :: TIM_USE(:) ! Times (years) to process
      INTEGER :: NUM_TIM                 ! Number of times to process
!
      CHARACTER(LEN=10) :: SDATE ! Start date of processing
      CHARACTER(LEN=10) :: STIME ! Start time of processing
!
      LOGICAL :: REPORT  ! Flag whether the report file is open
      LOGICAL :: VERBOSE ! Flag for verbose outputs to the screen
!
!     Type definition for specie data
!
      TYPE SPC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Species ID
        CHARACTER(LEN= 2) :: TYPE ! Species type
!                                     QP = aquatic plant,  TP = terrestrial plant
!                                     QA = aquatic animal, TA = terrestrial, animal
        CHARACTER(LEN=48) :: NAME ! Species name
      END TYPE SPC_TYPE
      TYPE(SPC_TYPE), ALLOCATABLE :: SPECIE(:) ! Variable structure for species information
      INTEGER :: NUM_SPC ! Number of species in this run
!
!     Data space
!
      REAL, ALLOCATABLE :: TMP_DATA(:) ! Vector of single realization data (length NREAL)
      CHARACTER(LEN=6), ALLOCATABLE :: MATCH_SPC(:) ! Specie ID's that match the given criteria
      REAL, ALLOCATABLE :: MATCH_VAL(:) ! Specie doses that match the given criteria
      CHARACTER(LEN=6), ALLOCATABLE :: TABLE_SPC(:,:) ! Specie ID's in the table
      REAL, ALLOCATABLE :: TABLE_VAL(:,:) ! Dose values in the table
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Control_Mod

MODULE Files_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This include deck contains file specific variable information.
!
!  History:
!
!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!
!  Code Block:
!
      INTEGER, PARAMETER :: MAXFN=128 ! Length of file names
!
      INTEGER :: IECM               ! Unit number for the ECEM output file
      CHARACTER(LEN=MAXFN) :: FNECM ! Name of the ECEM output file
!
      INTEGER :: IKEY               ! Unit number for the keyword file
      CHARACTER(LEN=MAXFN) :: FNKEY ! Name of the input keyword file
!
      INTEGER :: IRES               ! Unit number for the output results file
      CHARACTER(LEN=MAXFN) :: FNRES ! Name of the output results file
!
      INTEGER :: IRPT               ! Unit number for the report file
      CHARACTER(LEN=MAXFN) :: FNRPT ! Name of the output report file
!
      INTEGER :: ISPE               ! Unit number for the input species list file
      CHARACTER(LEN=MAXFN) :: FNSPE ! Name of the input species list file
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Files_Mod

MODULE Iden_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Iden_Mod

      PROGRAM HighDose
!!***********************************************************************************************************
!!
!!          HighDose - Highest Dose Extraction Utility Tool from ECEM Data
!!               Toolkit for Integrated Impacts Assessments (TIIA)
!!      Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!***********************************************************************************************************
!!
!!  Purpose:
!!    HighDose is a utility code to extract the highest dose by specie type from a
!!    detailed body burden file written by ECEM.  ECEM is the top level routine for
!!    the ecological impacts portion of the suite of TIIA, Version 1, codes.
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!  History:
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  5 Jun 2007 : Update for TIIA logic and copyright information
!!
!!***********************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IERR ! Integer error flag
      INTEGER :: ILOC ! Location loop counter
      INTEGER :: ITIM ! Time loop counter
      CHARACTER(LEN=64) :: CLINE ! Character output line
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Program, run identification, and initialization
!
      CALL IDEN_SET( )
      CALL INIT( )
!
! *** Open the input file
!
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the HUMAN keyword file'
        WRITE(*,*) 'Stop in '//PRGNAM
        STOP
      END IF
!
! *** Define the report file
      CALL TELLTIME( 'Extracting the report file name', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEYS_REPORT( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//PRGNAM
        STOP
      END IF
!
!     Print the first part of the opening banner page
      CALL BANNER_1(  )
!
! *** Read the first pass of the keywords to get the report file
!     open, user definition, and array dimension information collected
!
      CALL TELLTIME( 'Reading keywords', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEY( IERR )
      IF( IERR .EQ. 666 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//PRGNAM
        STOP
      END IF
      IF( IERR .NE. 0 ) GO TO 999
!
!     Finish printing the banner page
      CALL BANNER_2( )
!
!-----------------------------------------------------------------------------
! *** Read the specie information
!-----------------------------------------------------------------------------
!
! *** Open the species file
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening species file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_SPECIES( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Read the species data
!
      CALL TELLTIME( 'Reading species information', 'SCREEN', .FALSE., IRPT )
      CALL READ_SPECIES( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
!-----------------------------------------------------------------------------
! *** Output initial and specie information to the report file and open files
!-----------------------------------------------------------------------------
!
! *** Output information and check for errors
!
      CALL CHECK_ECHO( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Open the data file from ECEM
!
      CALL TELLTIME( 'Opening input ECEM file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_ECEM( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Open the output data file
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening output results file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_RESULTS( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
!-----------------------------------------------------------------------------
! *** Process the requested set of table extractions
!-----------------------------------------------------------------------------
!
! *** Set up memory locations
!
      IF( VERBOSE ) CALL TELLTIME( 'Allocating memory', 'SCREEN', .FALSE., IRPT )
      CALL SET_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Loop over the locations and times to be processed
!
      CALL TELLTIME( 'Processing data by location', 'SCREEN', .FALSE., IRPT )
      DO ILOC = 1, NUM_LOC
        DO ITIM = 1, NUM_TIM
!
          IF( VERBOSE ) THEN
            WRITE(CLINE,'(A,I0)') 'Processing location "'//TRIM(LOC_USE(ILOC))//'" for year ', TIM_USE(ITIM)
            CALL TELLTIME( TRIM(CLINE), 'SCREEN', .FALSE., IRPT )
          END IF
!
! ***     Read the data file for this combination of indicators and save matching data
!          IF( VERBOSE ) CALL TELLTIME( 'Reading ECEM data file', 'SCREEN', .FALSE., IRPT )
          CALL READ_ECEM( ILOC, ITIM, IERR )
          IF( IERR .NE. 0 ) GO TO 999
!
! ***     Construct and output the table entries
!          IF( VERBOSE ) WRITE(*,*) ' Constructing table entries'
          CALL MAKE_TABLE( ILOC, ITIM, IERR )
          IF( IERR .NE. 0 ) GO TO 999
!
        END DO
      END DO
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
      IF( REPORT ) THEN
        MESSAG(1) = 'Error encountered in a lower-level routine.'
        MESSAG(2) = 'Execution halted because of the above errors.'
        CALL PRTERR( IERR, PRGNAM, 2 )
      ELSE
        WRITE(*,*) 'Error encountered in a lower-level routine.'
        WRITE(*,*) 'Execution halted because of the above errors.'
        WRITE(*,*) 'Program stop in ' // TRIM(PRGNAM)
      END IF
      CALL TELLTIME( 'Abnormal Run Termination Due to Errors', 'SCREEN', .FALSE., IRPT )
      STOP
!
 1000 CONTINUE
!
! *** Elapsed time message
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, PRGNAM, 1 )
      CALL TELLTIME( 'Normal Termination', 'SCREEN', .FALSE., IRPT )
      STOP
!
      END PROGRAM

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.
!!
!!  History:
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Update for TIIA logic and copyright information
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
      WRITE(IRPT,1010) 'H     H IIIIIII  GGGGG  H     H DDDDDD   OOOOO   SSSSS  EEEEEEE'
      WRITE(IRPT,1010) 'H     H    I    G     G H     H D     D O     O S     S E      '
      WRITE(IRPT,1010) 'H     H    I    G       H     H D     D O     O  S      E      '
      WRITE(IRPT,1010) 'HHHHHHH    I    G   GGG HHHHHHH D     D O     O   SSS   EEEEE  '
      WRITE(IRPT,1010) 'H     H    I    G     G H     H D     D O     O      S  E      '
      WRITE(IRPT,1010) 'H     H    I    G     G H     H D     D O     O S     S E      '
      WRITE(IRPT,1010) 'H     H IIIIIII  GGGGG  H     H DDDDDD   OOOOO   SSSSS  EEEEEEE'
 1010 FORMAT(8X,A)
!
      WRITE(IRPT,1020) PRGNAM, PRGVER, PRGDAT
 1020 FORMAT(//27X,A,' Version ',A/29X,'Modified on ',A)
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
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Qa_Mod
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

      SUBROUTINE CHECK_ECHO( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks input data and writes an echo of the
!!    problem definition to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 2.0
!!    Paul W. Eslinger :  5 Jun 2007 : Update for TIIA logic
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_ECHO' ! Name of this subroutine
      INTEGER :: ILOC ! Location looping index
      INTEGER :: ITIM ! Time looping index
      INTEGER :: ISPC ! Species looping index
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
! *** Files names used in the analysis
!
      WRITE(IRPT,1040) 'File Name for Input Keyword Data', TRIM(FNKEY)
      WRITE(IRPT,1040) 'File Name for the Report File', TRIM(FNRPT)
      WRITE(IRPT,1040) 'File Name for the Species Data', TRIM(FNSPE)
      WRITE(IRPT,1040) 'File Name for the ECEM Data', TRIM(FNECM)
      WRITE(IRPT,1040) 'File Name for the Results Data', TRIM(FNRES)
 1040 FORMAT(/A/'File: ',A)
!
! *** Species Definitions
!
      IF( NUM_SPC .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'The number of species is zero'
        MESSAG(2) = 'Check the species data file for definitions'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1060)
 1060 FORMAT(/'Species information listed in input order'/ &
              'Index Type      ID      Long Name'/ &
              '----- ----   --------   ',48('-'))
      DO ISPC = 1, NUM_SPC
        WRITE(IRPT,1070) ISPC, SPECIE(ISPC)%TYPE, SPECIE(ISPC)%ID, TRIM(SPECIE(ISPC)%NAME)
 1070   FORMAT(1X,I3,'  "',A2,'" : "',A,'" : "',A,'"')
      END DO
      WRITE(IRPT,1090) NUM_SPC
 1090 FORMAT('A total of ',I0,' species have been requested.')
!
! *** Solution requested
!
      IF( SOL_ID .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'The solution ID is not specified'
        MESSAG(2) = 'Change the SOLUTION keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1100) 'The solution ID requested is "', TRIM(SOL_ID),'"'
 1100 FORMAT(/A,A,A)
!
! *** Analyte requested
!
      IF( ANA_ID .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'The analyte ID is not specified'
        MESSAG(2) = 'Change the ANALYTE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1100) 'The analyte ID requested is "', TRIM(ANA_ID), '"'
!
! *** Soil type requested
!
      WRITE(IRPT,1100) 'The soil type ID requested is "', TRIM(SOIL_ID), '"'
!
! *** Locations desired
!
      IF( NUM_LOC .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'The number of desired locations is zero'
        MESSAG(2) = 'Change the LOCATION keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1110)
 1110 FORMAT(/'Location information'/ &
              'Index    ID   '/ &
              '----- --------')
      DO ILOC = 1, NUM_LOC
        WRITE(IRPT,1120) ILOC, LOC_USE(ILOC)
 1120   FORMAT(1X,I3,'  "',A6,'"')
      END DO
      WRITE(IRPT,1130) NUM_LOC
 1130 FORMAT('A total of ',I0,' locations have been requested.')
!
! *** Times desired
!
      IF( NUM_TIM .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'The number of desired times is zero'
        MESSAG(2) = 'Change the TIME keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1140)
 1140 FORMAT(/'Time information'/ &
              'Index  Year  '/ &
              '----- -----')
      DO ITIM = 1, NUM_TIM
        WRITE(IRPT,1150) ITIM, TIM_USE(ITIM)
 1150   FORMAT(1X,I3,3X,I5)
      END DO
      WRITE(IRPT,1160) NUM_TIM
 1160 FORMAT('A total of ',I0,' times have been requested.')
!
! *** Realization checks
!
      IF( MEAN ) THEN
        WRITE(IRPT,'(/A)') 'The mean of all realizations is requested'
      ELSE
        WRITE(IRPT,1170) NUM_REL
 1170   FORMAT(/'Realization ',I0,' is requested.')
        IF( NREAL .LT. NUM_REL ) THEN
          IERR = 6
          MESSAG(1) = 'The specific realization requested is larger than the'
          MESSAG(2) = 'number of realizations in the keyword file'
          MESSAG(3) = 'Change the REALIZATION or MEMORY keywords'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
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
      SUBROUTINE GET_TYPE( LABEL, THIS_TYPE )
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
!!    LABEL      : Input  - Character - species label
!!    THIS_TYPE  : Output - Character - type for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
      CHARACTER(LEN=*) :: THIS_TYPE
!
! *** Local variables
      INTEGER :: I ! Looping variable
!
! *** Look for a match on the species ID
!
      THIS_TYPE = 'XX'
      DO I = 1, NUM_SPC
        IF( SPECIE(I)%ID .EQ. LABEL ) THEN
          THIS_TYPE = SPECIE(I)%TYPE
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE IDEN_SET( )
!!**********************************************************************
!!
!!  Purpose:
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
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
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
      PRGNAM = 'HighDose'
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
!!    This subroutine initializes the global variables of the ECEM
!!    software package.
!!
!!  History:
!!
!!    Paul W. Eslinger : 31 Jul 2002 : Version 1.0
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------
!
      VERBOSE = .FALSE.
!
! *** Keyword input file
!
      IKEY  = 9
      FNKEY = ' '
!
! *** Report file
!
      IRPT  = 10
      IRPT_ERR = IRPT
      FNRPT = ' '
      REPORT = .FALSE.
!
! *** ECEM output file
!
      IECM  = 11
      FNECM = ' '
!
! *** Output results file
!
      IRES  = 12
      FNRES = ' '
!
! *** Input species file
!
      ISPE  = 13
      FNSPE = ' '
!
! *** Problem title, solution nad analyte type
!
      PTITLE = ' '
      SOL_ID = ' '
      ANA_ID = ' '
      SOIL_ID = 'NONE'
!
! *** Counter type information
!
      NUM_SPC = 0
      NUM_LOC = 0
      NUM_TIM = 0
      NUM_REL = 0
      NUM_TAB = 0
      NUM_REC = 0
!
      MEAN = .FALSE.
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MAKE_TABLE( ILOC, ITIM, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine constructs the output table from the extracted
!!    data and outputs the values to the results file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger : 31 Jul 2002 : Add mean capability
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ILOC ! Location looping index
      INTEGER, INTENT(IN) :: ITIM ! Time looping index
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'MAKE_TABLE' ! Name of this subroutine
      CHARACTER(LEN=6) :: TY, TTY ! Temporary work space
      CHARACTER(LEN=2) :: THIS_TYPE
      INTEGER :: KFLAG
      INTEGER :: NUM_TA, NUM_QA, NUM_TP, NUM_QP
      INTEGER I, IUSE, IDX
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Message to the results file
!
      WRITE(IRES,1000) TRIM(LOC_USE(ILOC)), TIM_USE(ITIM), TRIM(SOL_ID), TRIM(SOIL_ID), TRIM(ANA_ID)
 1000 FORMAT(/'"Results for location ID = ',A,' year = ',I0,' solution = ',A,' soil type = ',A,' and analyte type = ',A,'"')
!
! *** Exit with a short message if no matching data are found
!
      IF( NUM_VAL .EQ. 0 ) THEN
        WRITE(IRES,'(A)') '"No matching data were found"'
        RETURN
      END IF
!
! *** Sort the entire set of values
!
      KFLAG = -2
      CALL SORT_CHAR( MATCH_VAL, MATCH_SPC, TY, TTY, NUM_VAL, KFLAG, IERR )
!
! *** Fill in the table
!
! *** Reset counters for each type of specie
!
      NUM_TA = 0
      NUM_QA = 0
      NUM_TP = 0
      NUM_QP = 0
!
! *** Reset the entire table
!
      DO IUSE = 1, NUM_TAB
        DO I = 1, 4
          TABLE_SPC(IUSE,I) = ' '
          TABLE_VAL(IUSE,I) = 0.0
        END DO
      END DO
!
! *** Loop over the results and put in table
!
      DO IUSE = 1, NUM_VAL
!
! ***   Determine the species type
!
        CALL GET_TYPE( MATCH_SPC(IUSE), THIS_TYPE )
!       WRITE(IRES,*)  MATCH_SPC(IUSE) //' type= '// THIS_TYPE
!
        SELECT CASE( THIS_TYPE )
!
          CASE('TP') !===> Terrestrial animals
            IF( NUM_TP .LT. NUM_TAB ) THEN
              IDX = 1
              NUM_TP = NUM_TP + 1
              TABLE_SPC(NUM_TP,IDX) = MATCH_SPC(IUSE)
              TABLE_VAL(NUM_TP,IDX) = MATCH_VAL(IUSE)
            END IF
!
          CASE('TA') !===> Terrestrial animals
            IF( NUM_TA .LT. NUM_TAB ) THEN
              IDX = 2
              NUM_TA = NUM_TA + 1
              TABLE_SPC(NUM_TA,IDX) = MATCH_SPC(IUSE)
              TABLE_VAL(NUM_TA,IDX) = MATCH_VAL(IUSE)
            END IF
!
          CASE('QP') !===> Aquatic plants
            IF( NUM_QP .LT. NUM_TAB ) THEN
              IDX = 3
              NUM_QP = NUM_QP + 1
              TABLE_SPC(NUM_QP,IDX) = MATCH_SPC(IUSE)
              TABLE_VAL(NUM_QP,IDX) = MATCH_VAL(IUSE)
            END IF
!
          CASE('QA') !===> Aquatic animals
            IF( NUM_QA .LT. NUM_TAB ) THEN
              IDX = 4
              NUM_QA = NUM_QA + 1
              TABLE_SPC(NUM_QA,IDX) = MATCH_SPC(IUSE)
              TABLE_VAL(NUM_QA,IDX) = MATCH_VAL(IUSE)
            END IF
!
          CASE DEFAULT
! ***       Shouldn't get to here
            IERR = 1
            MESSAG(1) = 'Invalid species type: ' // THIS_TYPE
            MESSAG(1) = 'Species ID: ' // MATCH_SPC(IUSE)
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
!
          END SELECT

      END DO
!
! *** Output the header line
!
      WRITE(IRES,1010)
 1010 FORMAT('"Terrestrial Plants"," ","Terrestrial Animals"," ","Aquatic Plants"," ","Aquatic Animals"')
!
      WRITE(IRES,1020)
 1020 FORMAT('"Specie","Dose","Specie","Dose","Specie","Dose","Specie","Dose"')
!
! *** Output the table values
!
      DO IUSE = 1, NUM_TAB
        WRITE(IRES,1030) TABLE_SPC(IUSE,1), TABLE_VAL(IUSE,1), TABLE_SPC(IUSE,2), TABLE_VAL(IUSE,2), &
          TABLE_SPC(IUSE,3), TABLE_VAL(IUSE,3), TABLE_SPC(IUSE,4), TABLE_VAL(IUSE,4)
 1030   FORMAT(1P,4('"',A,'",',E10.3,:,','))
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_ECEM( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the ECEM data file for reading.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA
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
      INTEGER :: IERR ! Error number (0=no errors)
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'OPEN_ECEM' ! Name of this subroutine
      LOGICAL :: THERE ! File existence variable
      INTEGER :: IERF ! Status variable for open statement
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the requested file exists
!
      IF( FNECM .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The ECEM data file name is blank'
        MESSAG(2) = 'Change the file name on the FILE keyword, ECEM modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FNECM,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested ECEM data file was not found'
        MESSAG(2) = 'Change the file name on the FILE keyword, ECEM modifier'
        MESSAG(3) = 'File: '//TRIM(FNECM)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IECM,FILE=FNECM,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the ECEM data file'
        MESSAG(2) = 'File name entered on the FILE keyword, ECEM modifier'
        MESSAG(3) = 'File: '//TRIM(FNECM)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_KEY( IERR )
!!********************************************************************************
!!
!!  Purpose:
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
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA, add copyright information
!!    Paul W. Eslinger : 29 Jun 2012 : Revise to a common callable routine
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
! *** User defined functions
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      LOGICAL :: THERE ! File existence variable
      INTEGER :: IERF  ! Status variable for open statement
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
! *** Attempt to open the file
!
      OPEN(IKEY,FILE=FNKEY,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        WRITE(*,1030) TRIM(FNKEY)
 1030   FORMAT(' System error opening the input keyword file'/ &
               ' File: ',A)
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_RESULTS( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    extracted data values.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger : 31 Jul 2002 : Add mean value option
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Errors_Mod
      USE Iden_Mod
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'OPEN_RESULTS'
      INTEGER :: IERF ! Status variable for open statement
      CHARACTER(LEN=64) :: CLINE
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the file name was entered
!
      IF( FNRES .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The output results data file name is blank'
        MESSAG(2) = 'Change the file name on the FILE keyword, RESULTS modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IRES,FILE=FNRES,STATUS='UNKNOWN',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the results data file'
        MESSAG(2) = 'File name entered on the FILE keyword, RESULTS modifier'
        MESSAG(3) = 'File: '//TRIM(FNRES)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Write a few identification items to the file
!
      WRITE(IRES,1000) 'Code Name:',    TRIM(PRGNAM)
      WRITE(IRES,1000) 'Code Version:', TRIM(PRGVER)
      WRITE(IRES,1000) 'Code Date:',    TRIM(PRGDAT)
      WRITE(IRES,1000) 'Run ID:',       TRIM(CRUNID)
      WRITE(IRES,1000) 'Run Title:',    TRIM(PTITLE)
      WRITE(IRES,1000) 'User Name:',    TRIM(USRNAM)
 1000 FORMAT('"',A,'"',:,',"',A,'"')
!
      WRITE(IRES,*) ' '
      IF( MEAN ) THEN
        WRITE(IRES,1000) 'The mean of all realizations is requested'
      ELSE
        WRITE(CLINE,1010) NUM_REL
 1010   FORMAT('Realization ',I0,' is requested.')
        WRITE(IRES,1000) TRIM(CLINE)
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_SPECIES( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the species file for reading data.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA
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
      INTEGER :: IERR ! Error number (0=no errors)
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'OPEN_SPECIES'
      INTEGER :: IERF ! Status variable for open statement
      LOGICAL :: THERE ! File existence variable
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the file name was entered and the file exists
!
      IF( FNSPE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The species data file name is blank'
        MESSAG(2) = 'Change the file name on the FILE keyword, SPECIES modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FNSPE,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested species data file was not found'
        MESSAG(2) = 'Change the file name on the FILE keyword, SPECIES modifier'
        MESSAG(3) = 'File: '//TRIM(FNSPE)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(ISPE,FILE=FNSPE,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the species data file'
        MESSAG(2) = 'File name entered on the FILE keyword, SPECIES modifier'
        MESSAG(3) = 'File: '//TRIM(FNSPE)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE READ_ECEM( ILOC, ITIM, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading the ECEM data file and extracting
!!    data for a given set of indicators.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger : 31 Jul 2002 : Add mean value option
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
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
! *** Call list variables
      INTEGER, INTENT(IN) :: ILOC ! Location looping index
      INTEGER, INTENT(IN) :: ITIM ! Time looping index
      INTEGER :: IERR             ! Error number flag
      INTEGER :: I
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'READ_ECEM'
      CHARACTER(LEN=1) :: CDUM
!
      INTEGER :: TMP_YEAR
      CHARACTER(LEN= 6) :: TMP_LOC  ! Location ID
      CHARACTER(LEN= 4) :: TMP_SOIL ! Soil type
      CHARACTER(LEN= 6) :: TMP_ANA  ! Analyte ID
      CHARACTER(LEN= 6) :: TMP_SPC  ! Species ID
      CHARACTER(LEN= 6) :: TMP_SOL  ! Solution type
      CHARACTER(LEN=10) :: TMP_UNT  ! Data units
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Rewind the data file
!
      REWIND( IECM )
      NUM_VAL = 0
!
! *** Read the header line from the file
!
      READ(IECM,*) CDUM
!
! *** Read until an end-of-file
!
   10 CONTINUE
!       Sample data lines
!      "Time","Location ID","Soil Type","Analyte ID","Specie","Solution Type","Units","Values by realization"
!      1945,"TH0002","NONE","CCl4","PERPHY","BMTISS","unitless", 1.05656E+02, 7.57601E+04, ...
        READ(IECM,*,END=20) TMP_YEAR, TMP_LOC, TMP_SOIL, TMP_ANA, TMP_SPC, TMP_SOL, TMP_UNT, (TMP_DATA(I),I=1,NREAL)
        IF( TMP_YEAR.EQ.TIM_USE(ITIM) .AND. TMP_LOC.EQ.LOC_USE(ILOC) .AND. &
            TMP_ANA.EQ.ANA_ID .AND. TMP_SOIL.EQ.SOIL_ID .AND. TMP_SOL.EQ.SOL_ID ) THEN
!            WRITE(*,*) 'MATCH '//ANA_ID//' '//SOL_ID
          NUM_VAL = NUM_VAL + 1
          IF( NUM_VAL .GT. NUM_REC ) THEN
            IERR = 1
            MESSAG(1) = 'The number of data exceeded the record count'
            MESSAG(2) = 'Increase the value with RECORDS on the MEMORY keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          MATCH_SPC(NUM_VAL) = TMP_SPC
          IF( MEAN ) THEN
            MATCH_VAL(NUM_VAL) = 0.0
            DO I = 1, NREAL
              MATCH_VAL(NUM_VAL) = MATCH_VAL(NUM_VAL) + TMP_DATA(I)
            END DO
            MATCH_VAL(NUM_VAL) = MATCH_VAL(NUM_VAL) / NREAL
          ELSE
            MATCH_VAL(NUM_VAL) = TMP_DATA(NUM_REL)
          END IF
        END IF
        GO TO 10
!
   20 CONTINUE
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE READ_KEY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the keyword control
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
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Move REPORT to another subroutine
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
!
      CHARACTER(LEN=8) :: CALLER = 'READ_KEY' ! Name of this subroutine
      INTEGER :: IDX   ! Temporary index
      CHARACTER :: TITLE*(LENCRD)
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary file name
      REAL :: RTMP
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
        MESSAG(2) = 'Examine the standard output messages for details'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ===> ANALYTE Keyword
          ANA_ID = QUOTE(1)
!
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IKEY )
          RETURN
!
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('SPECIES') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The SPECIES modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNSPE = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('ECEM') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The ECEM modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNECM = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('RESULTS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The RESULTS modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNRES = TMP_NAME
            END IF
          END IF
!
        CASE( 'LOCATION' ) ! ===> LOCATION Keyword
!         Count quote strings, allocate memory, store data
          NUM_LOC = NQUOTE
          IF( NUM_LOC .GT. 0 ) THEN
            ALLOCATE( LOC_USE(NUM_LOC) )
            DO IDX = 1, NUM_LOC
              LOC_USE(IDX) = QUOTE(IDX)(1:6)
            END DO
          END IF
!
        CASE( 'MEMORY' ) ! ===> MEMORY Keyword
!MEMORY REALIZATION=25 TABLEROW=4 RECORDS=1000 ! NOT CODED
!
          IF( CEXIST('TABLEROW') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 4
              MESSAG(1) = 'Table row count value not found'
              MESSAG(2) = 'MEMORY keyword, TABLEROW modifier'
              MESSAG(3) = 'Problem in the input keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            NUM_TAB = RTMP
          ELSE
            IERR = 5
            MESSAG(1) = 'TABLEROW modifier not found'
            MESSAG(2) = 'Problem with the MEMORY keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('REALIZAT') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 6
              MESSAG(1) = 'Realization indicator value not found'
              MESSAG(2) = 'MEMORY keyword, REALIZATION modifier'
              MESSAG(3) = 'Problem in the input keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            NREAL = RTMP
          ELSE
            IERR = 7
            MESSAG(1) = 'REALIZATION modifier not found'
            MESSAG(2) = 'Problem with the MEMORY keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('RECORDS') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 8
              MESSAG(1) = 'Record count value not found'
              MESSAG(2) = 'MEMORY keyword, RECORDS modifier'
              MESSAG(3) = 'Problem in the input keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            NUM_REC = RTMP
          ELSE
            IERR = 9
            MESSAG(1) = 'RECORDS modifier not found'
            MESSAG(2) = 'Problem with the MEMORY keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
          IF( CEXIST('MEAN') ) THEN
            MEAN = .TRUE.
          ELSE
            NUM_REL = VALUE(1)
          END IF
!
        CASE( 'SOIL' ) ! ===> SOIL Keyword
          SOIL_ID = QUOTE(1)
!
        CASE( 'SOLUTION' ) ! ===> SOLUTION Keyword
          SOL_ID = QUOTE(1)
!
        CASE( 'TIME' ) ! ===> TIME Keyword
!         Count values, allocate memory, store data
          NUM_TIM = NVALUE
          IF( NUM_TIM .GT. 0 ) THEN
            ALLOCATE( TIM_USE(NUM_TIM) )
            DO IDX = 1, NUM_TIM
              TIM_USE(IDX) = VALUE(IDX)
            END DO
          END IF
!
        CASE( 'TITLE' ) ! ===> TITLE Keyword
          PTITLE = QUOTE(1)
!
        CASE( 'USER' ) ! ===> USER Keyword
          USRNAM = QUOTE(1)
!
        CASE( 'VERBOSE' ) ! ===> VERBOSE Keyword
          VERBOSE = .TRUE.
!
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
      SUBROUTINE READ_SPECIES( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the species file and stores the data.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'READ_SPECIES' ! Name of this subroutine
      CHARACTER(LEN=1) :: CDUM
      INTEGER :: IERA ! Error status variable from the allocate action
      INTEGER :: ISPC ! Looping index for species
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Count the number of species
!
      NUM_SPC = 0
   10 CONTINUE
        READ(ISPE,*,END=20) CDUM
        NUM_SPC = NUM_SPC + 1
        GO TO 10
!
   20 CONTINUE
!
! *** Check on the number of species
!
      IF( NUM_SPC .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least 1 specie required to allocate memory'
        MESSAG(2) = 'Check the species data file'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate memory for the species
!
      ALLOCATE( SPECIE(NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating memory for SPECIE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Read and store the species data
!
      REWIND( ISPE )
      DO ISPC = 1, NUM_SPC
        READ(ISPE,*,END=999,ERR=999) SPECIE(ISPC)%TYPE, SPECIE(ISPC)%ID, SPECIE(ISPC)%NAME
      END DO
!
! *** Finished with the file and normal termination
!
      CLOSE( ISPE )
      RETURN
!
! *** Error branch
!
  999 CONTINUE
      IERR = 3
      MESSAG(1) = 'Error reading the species data file'
      MESSAG(2) = 'File: ' // TRIM(FNSPE(1:64))
      CALL PRTERR(IERR, CALLER, 2)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE SET_MEMORY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine allocates memory for variables dependent on the
!!    MEMORY keyword.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 2.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'SET_MEMORY'
      INTEGER :: IERA ! Error status variable from the allocate action
!
!---- First executable code --------------------------------------------
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
      ALLOCATE( TMP_DATA(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating memory for TMP_DATA'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the number of records
!
      IF( NUM_REC .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'At least 1 record required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( MATCH_SPC(NUM_REC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for MATCH_SPC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( MATCH_VAL(NUM_REC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating memory for MATCH_VAL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the number of table rows
!
      IF( NUM_TAB .LT. 1 ) THEN
        IERR = 6
        MESSAG(1) = 'At least 1 table row required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( TABLE_SPC(NUM_TAB,4), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error allocating memory for TABLE_SPC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( TABLE_VAL(NUM_TAB,4), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for TABLE_VAL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE SORT_CHAR( X, Y, TY, TTY, N, KFLAG, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    SSORT sorts array X and optionally makes the same interchanges
!!    in the character vector Y.  The array X may be sorted in
!!    increasing order or decreasing order.  A modified QUICKSORT
!!    algorithm is used.
!!
!!  Reference:
!!
!!    Singleton, R. C.
!!    Algorithm 347
!!    "An Efficient Algorithm for Sorting with Minimal Storage"
!!    Comm. Assoc. Comput. Mach.
!!    Vol. 12, No. 3, 1969, pp. 185-187.
!!
!!  Auxiliary Routines Required:
!!
!!    PRTERR
!!
!!  Variable Descriptions:
!!
!!    X     : array of values to be sorted
!!    Y     : character array to be (optionally) carried along
!!    N     : number of values in array X to be sorted
!!    KFLAG : control parameter
!!            =  2 means sort X in increasing order and carry Y along.
!!            =  1 means sort X in increasing order (ignoring Y)
!!            = -1 means sort X in decreasing order (ignoring Y)
!!            = -2 means sort X in decreasing order and carry Y along.
!!    IERR  : Returned error flag
!!            = 0 Means no errors
!!            = 1 Means error on the number of data values
!!            = 2 Means error on the sort indicator
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
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
      REAL, DIMENSION(*) :: X
      CHARACTER(LEN=*), DIMENSION(*) :: Y
      CHARACTER(LEN=*) :: TY, TTY
      INTEGER, INTENT (IN) :: N, KFLAG
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      INTEGER, DIMENSION(21) :: IL, IU
      CHARACTER(LEN=9) :: CALLER = 'SORT_CHAR' ! Name of this routine
      INTEGER :: NN, I, M, J, K, KK, IJ, L
      REAL :: R, T, TT
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      NN = N
      KK = IABS(KFLAG)
!
! *** Check for proper dimensioning
!
      IF( N .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid number of data.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Check for valid sort order indicator
!
      IF( .NOT.(KK.EQ.1 .OR. KK.EQ.2) ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid sort indicator.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Alter array X to get decreasing order if needed
!
   15 IF (KFLAG.GE.1) GO TO 30
!
      DO 20 I = 1, NN
        X(I) = -X(I)
   20 CONTINUE
!
   30 GO TO (100,200),KK
!
! *** Sort X only
!
  100 CONTINUE
      M = 1
      I = 1
      J = NN
      R = 0.375
  110 IF(I .EQ. J) GO TO 155
  115 IF(R .GT. 0.5898437) GO TO 120
      R = R + 3.90625E-2
      GO TO 125
!
  120 R = R - 0.21875
  125 K = I
!
! *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
!
      IJ = I + IFIX( FLOAT(J-I) * R)
      T = X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 130
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
  130 L = J
!
! *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
!
      IF (X(J) .GE. T) GO TO 140
      X(IJ) = X(J)
      X(J)  = T
      T     = X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 140
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      GO TO 140
  135 TT = X(L)
      X(L) = X(K)
      X(K) = TT
!
! *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
! *** SMALLER THAN T
!
  140 L = L - 1
      IF( X(L) .GT. T ) GO TO 140
!
! *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
! *** GREATER THAN T
!
  145 K=K+1
      IF (X(K) .LT. T) GO TO 145
!
! *** INTERCHANGE THESE ELEMENTS
!
      IF (K .LE. L) GO TO 135
!
! *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
!
      IF( L-I .LE. J-K ) GO TO 150
      IL(M) = I
      IU(M) = L
      I = K
      M = M+1
      GO TO 160
!
  150 IL(M) = K
      IU(M) = J
      J = L
      M = M+1
      GO TO 160
!
! *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
!
  155 M=M-1
      IF( M .EQ. 0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  160 IF( J-I .GE. 1 ) GO TO 125
      IF( I .EQ. 1 ) GO TO 110
      I = I-1
  165 I = I+1
      IF( I .EQ. J ) GO TO 155
      T = X(I+1)
      IF( X(I) .LE. T ) GO TO 165
      K = I
  170 X(K+1) = X(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 170
      X(K+1) = T
      GO TO 165
!
! *** SORT X AND CARRY Y ALONG
!
  200 CONTINUE
      M = 1
      I = 1
      J = NN
      R = 0.375
  210 IF( I .EQ. J ) GO TO 255
  215 IF( R .GT. 0.5898437 ) GO TO 220
      R = R + 3.90625E-2
      GO TO 225
!
  220 R = R - 0.21875
  225 K = I
!
! *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
!
      IJ = I + IFIX( FLOAT(J-I) * R )
      T  = X(IJ)
      TY = Y(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 230
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y(IJ) = Y(I)
      Y(I)  = TY
      TY    = Y(IJ)
  230 L = J
!
! *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
!
      IF (X(J) .GE. T) GO TO 240
      X(IJ) = X(J)
      X(J)  = T
      T     = X(IJ)
      Y(IJ) = Y(J)
      Y(J)  = TY
      TY    = Y(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 240
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y(IJ) = Y(I)
      Y(I)  = TY
      TY    = Y(IJ)
      GO TO 240
!
  235 TT=X(L)
      X(L) = X(K)
      X(K) = TT
      TTY  = Y(L)
      Y(L) = Y(K)
      Y(K) = TTY
!
! *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
! *** SMALLER THAN T
!
  240 L = L-1
      IF( X(L) .GT. T ) GO TO 240
!
! *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
! *** GREATER THAN T
!
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
!
! *** INTERCHANGE THESE ELEMENTS
!
      IF (K .LE. L) GO TO 235
!
! *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
!
      IF( L-I .LE. J-K ) GO TO 250
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 260
!
  250 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 260
!
! *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
!
  255 M = M-1
      IF( M .EQ. 0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I .GE. 1 ) GO TO 225
      IF( I .EQ. 1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I .EQ. J ) GO TO 255
      T  = X(I+1)
      TY = Y(I+1)
      IF( X(I) .LE. T ) GO TO 265
      K = I
  270 X(K+1) = X(K)
      Y(K+1) = Y(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 270
      X(K+1) = T
      Y(K+1) = TY
      GO TO 265
!
! *** CLEAN UP
!
  300 IF( KFLAG .GE. 1 ) RETURN
!
      DO 310 I = 1, NN
        X(I) = -X(I)
  310 CONTINUE
!
      RETURN
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

