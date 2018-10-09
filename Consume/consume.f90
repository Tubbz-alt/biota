!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
MODULE Control_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This include deck contains control information for the entire problem as well as
!    providing allocation of work arrays for a variety of purposes.
!
!  History:
!    Paul W. Eslinger : 24 Apr 2002 : Version 2.0
!
!  Code Block:
!
      CHARACTER(LEN=10) :: SDATE ! Start date of processing
      CHARACTER(LEN=10) :: STIME ! Start time of processing
!
      LOGICAL :: REPORT  ! Flag whether the report file is open
      INTEGER :: NUM_SPC ! Number of species in this run
!
!     Type definition for specie data
!
      TYPE SPC_TYPE
        INTEGER :: ORDER             ! Order in which to compute species
        REAL :: SOILING              ! Soil ingestion rate (kg soil ing/kg dry weight)
        REAL :: SEDING               ! Sediment ingestion rate (kg soil ing/kg dry weight)
        CHARACTER(LEN= 6) :: ID      ! Species ID
        CHARACTER(LEN= 2) :: TYPE    ! Species type
!                                      QP = aquatic plant,  TP = terrestrial plant
!                                      QA = aquatic animal, TA = terrestrial, animal
        CHARACTER(LEN=48) :: NAME    ! Species long name
        CHARACTER(LEN= 8) :: HABITAT ! Species type (aquatic, riparian, or upland)
        LOGICAL :: COMP              ! Flag whether a species results are computed
      END TYPE SPC_TYPE
      TYPE(SPC_TYPE), ALLOCATABLE :: SPECIE(:) ! Variable structure for species information
      INTEGER, ALLOCATABLE :: ISPCTMP(:) ! Temporary integer vector for species
!
      REAL, ALLOCATABLE :: PREDATE(:,:) ! Predation matrix for all species (size based on keywords)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Control_Mod

MODULE Files_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This include deck contains file specific variable information.
!
!  History:
!    Paul W. Eslinger : 24 Apr 2002 : Version 1.0
!
!  Code Block:
!
      INTEGER, PARAMETER :: MAXFN=256 ! Length of file names
!
      INTEGER :: IDAT ! Unit number for the data file
      CHARACTER(LEN=MAXFN) :: FNDAT ! Name of the input data file
!
      INTEGER :: IRPT ! Unit number for the report file
      CHARACTER(LEN=MAXFN) :: FNRPT ! Name of the output report file
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
!    Paul W. Eslinger : 24 Apr 2002 : Version 1.0
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

      PROGRAM CONSUME
!!**************************************************************************************************
!!
!!                                  CONSUME
!!  Build CONSUME keywords for the Ecological Contaminant Exposure Model (ECEM)
!!      given predation information in the form of a full predation matrix.
!!
!!             Battelle Memorial Institute, Richland, Washington
!!          Toolkit for Integrated Impact Assessments (TIIA), Ver. 1
!!
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    CONSUME is a utility code to build and test CONSUME keywords from
!!    predation matrices for ECEM.  ECEM is the top level routine for the
!!    ecological impacts portion of the TIIA, Version 1).
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jul 2003 : Version 1.0
!!    Paul W. Eslinger : 30 Jan 2006 : Upgrade error trapping
!!    Paul W. Eslinger :  7 Aug 2007 : Update to TIIA version
!!    Paul W. Eslinger :  9 Oct 2007 : Upgrade error messages and add
!!                                     user name requirement
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!  Notes:
!!
!!    1. This program writes a single output file called Consume.rpt
!!    2. This program reads a single input file with a user specified name.
!!
!! Reference:
!!
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!**************************************************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
!
! *** Declare local variables
      IMPLICIT NONE
!
      INTEGER :: IERR ! Integer error flag
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Program, run identification, and initialization
      CALL IDEN_SET( )
      CALL INIT( )
!
! *** Banner to the screen
      CALL QA_CopyRightSingle( 6, .FALSE. )
      WRITE(*,1010) PRGNAM, PRGVER, PRGDAT
 1010 FORMAT( 1X,'-------------------------------------------------'/&
              1X,1X,A,1X,A,'  Last Modified: ',A/                   &
              1X,'-------------------------------------------------')
!
! *** Open the report file
      IERR = 1
      FNRPT = 'Consume.rpt'
      OPEN(IRPT,FILE=FNRPT,ERR=999)
      IERR = 0
      REPORT = .TRUE.
!
! *** Note to the user
      WRITE(*,*) 'Results written to the file:  '//TRIM(FNRPT)
!
! *** Write initial information to the report file
      CALL BANNER( )
!
! *** Open the input data file
      CALL OPEN_DAT( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Read the input data
      WRITE(*,*) 'Reading input data from file: '//TRIM(FNDAT)
      CALL READ_DAT( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Output specie information to the report file
      CALL ECHO( )
!
! *** Output consumption keywords to the report file
      CALL CHECK_KEYS( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Output consumption keywords to the report file
      CALL COMP_KEYS( )
!
! *** Check the predation matrix for consumption order and diet fractions
      CALL PRED_CHECK( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Output predation information to the report file
      CALL ECHO2( )
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
        IERR = 999
        MESSAG(1) = 'Error encountered in a lower-level routine.'
        MESSAG(2) = 'Execution halted because of the above errors.'
        CALL PRTERR( IERR, PRGNAM, 2 )
      ELSE
        WRITE(*,*) 'Error encountered in a lower-level routine.'
        WRITE(*,*) 'Execution halted because of the above errors.'
        WRITE(*,*) 'Program stop in ' // TRIM(PRGNAM)
      END IF
      WRITE(*,*) 'Abnormal Run Termination Due to Errors'
      STOP
!
 1000 CONTINUE
!
! *** Final message
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, PRGNAM, 1 )
      WRITE(*,*) TRIM(MESSAG(1))
!
      STOP
      END
!
      SUBROUTINE BANNER( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints a banner page to the report file.
!!
!!  Calling Requirements:
!!
!!    Subroutine IDENC must have been called prior to this routine
!!    to set the identification variables.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jul 2003 : Version 1.0
!!    Paul W. Eslinger :  7 Aug 2007 : Revise for TIIA Version
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE QA_Mod
!
!---- Executable code --------------------------------------------------
!
      IMPLICIT NONE
!
! *** Define the header
!
      WRITE(IRPT,1000)
 1000 FORMAT(/)
!
      WRITE(IRPT,1010) ' CCCCC   OOOOO  N     N   SSS   U     U MM   MM EEEEEEE'
      WRITE(IRPT,1010) 'C     C O     O NN    N S    SS U     U M M M M E      '
      WRITE(IRPT,1010) 'C       O     O N N   N  S    S U     U M  M  M E      '
      WRITE(IRPT,1010) 'C       O     O N  N  N   SS    U     U M  M  M EEEEE  '
      WRITE(IRPT,1010) 'C       O     O N   N N S   S   U     U M     M E      '
      WRITE(IRPT,1010) 'C     C O     O N    NN SS   S  U     U M     M E      '
      WRITE(IRPT,1010) ' CCCCC   OOOOO  N     N   SSS    UUUUU  M     M EEEEEEE'
 1010 FORMAT(12X,A)
!
      WRITE(IRPT,1020) PRGNAM, PRGVER, PRGDAT
 1020 FORMAT(//27X,A,' Version ',A/26X,'Last Modified on ',A)
!
      WRITE(IRPT,1030)
 1030 FORMAT(&
        15X,'Build ECEM Consume Keywords from Predation Matrix'/     &
        15X,'------------------------------------------------'/&
        15X,'    Developed By Battelle Memorial Institute'/&
        15X,'              Richland, Washington'/&
        15X,'------------------------------------------------')
!
! *** Identification information
!
      WRITE(IRPT,1040) CRUNID, USRNAM
 1040 FORMAT(/8X,'Current Run ID = ',A14,'   User Name = ',A16)
!
      WRITE(IRPT,1050) SYSDAT, SYSTIM
 1050 FORMAT(15X,'System Date = ',A10,'   System Time = ',A8)
!
! *** Code status disclaimer (centered on page)
      CALL QA_CopyrightFull( Irpt, .TRUE. )
      CALL QA_Disclaimer( Irpt, .TRUE. )
      CALL QA_Reference( Irpt )
!
! *** Review Block
!
      WRITE(IRPT,1060)
 1060 FORMAT(//'                             Review Signatures')
!
      WRITE(IRPT,1070)
 1070 FORMAT(/'Input Prepared By: ______________________________', &
             '       Date: _______________')
!
      WRITE(IRPT,1080)
 1080 FORMAT(/'Input Reviewed By: ______________________________', &
             '       Date: _______________')
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_KEYS( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks several things in the inputs
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Jan 2006 : Original code
!!    Paul W. Eslinger :  7 Aug 2007 : Clean up comments and unused code
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_KEYS'
      INTEGER :: ISPC       ! Species looping index
      LOGICAL :: SPECIES_OK ! Logical trap on invalid
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      DO ISPC = 1, NUM_SPC
!
! ***   Check on species type
        SPECIES_OK = .FALSE.
        IF( SPECIE(ISPC)%TYPE .EQ. 'TP' ) SPECIES_OK = .TRUE.
        IF( SPECIE(ISPC)%TYPE .EQ. 'TA' ) SPECIES_OK = .TRUE.
        IF( SPECIE(ISPC)%TYPE .EQ. 'QP' ) SPECIES_OK = .TRUE.
        IF( SPECIE(ISPC)%TYPE .EQ. 'QA' ) SPECIES_OK = .TRUE.
        IF( .NOT.SPECIES_OK ) THEN
          IERR = 1
          MESSAG(1) = 'Invalid species type was found: '//SPECIE(ISPC)%TYPE
          MESSAG(2) = 'Valid types are QA, QP, TA, or TP'
          MESSAG(3) = 'Species is "'//TRIM(SPECIE(ISPC)%ID)//'" "'//TRIM(SPECIE(ISPC)%NAME)//'"'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
!       Aquatic animals can eat sediment
        IF( SPECIE(ISPC)%TYPE .EQ. 'QA' ) THEN
          IF( SPECIE(ISPC)%SEDING.LT.0.0 .OR. SPECIE(ISPC)%SEDING.GT.1.0 ) THEN
            IERR = 2
            MESSAG(1) = 'Invalid sediment consumption was found'
            MESSAG(2) = 'Valid range is 0 to 1 (inclusive)'
            MESSAG(3) = 'Species is "'//TRIM(SPECIE(ISPC)%ID)//'" "'//TRIM(SPECIE(ISPC)%NAME)//'"'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
!
!       Terrestrial animals can eat soil
        IF( SPECIE(ISPC)%TYPE .EQ. 'TA' ) THEN
          IF( SPECIE(ISPC)%SOILING.LT.0.0 .OR. SPECIE(ISPC)%SOILING.GT.1.0 ) THEN
            IERR = 3
            MESSAG(1) = 'Invalid soil consumption was found'
            MESSAG(2) = 'Valid range is 0 to 1 (inclusive)'
            MESSAG(3) = 'Species is "'//TRIM(SPECIE(ISPC)%ID)//'" "'//TRIM(SPECIE(ISPC)%NAME)//'"'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
!
      END DO
!
      RETURN
      END
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
!!    Paul W. Eslinger :  7 Aug 2007 : Add for TIIA Version
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
      WRITE(*,*) '  '//TRIM(PRGNAM)//'            : To be prompted for the input data file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' file_name  : To define the input data file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' -HELP      : To get this help message (but not execute anything)'
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE COMP_KEYS(  )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes and writes several things to the report file
!!      1. A listing of the species in the computation order
!!      2. A listing of consumption information
!!      3. A series of CONSUME keywords for use in ECEM
!!      4. An echo of the predation matrix
!!
!!  History:
!!
!!    Paul W. Eslinger :  7 Jul 2003 : Version 2.0
!!    Paul W. Eslinger :  7 Aug 2007 : Revise for TIIA Version
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
! *** Local variables
!
!      INTEGER :: IDX      ! Index variable
      INTEGER :: ISPC     ! Species looping index
      INTEGER :: JSPC     ! Species looping index
      INTEGER :: NUM_PREY ! Number of prey
!
!---- First executable code --------------------------------------------
!
! *** Output consumption information as CONSUME keywords for ECEM
!
      WRITE(IRPT,*) ' '
      WRITE(IRPT,'(A)') '!'
      WRITE(IRPT,'(A)') '! Consumption formatted as CONSUME keywords for ECEM '
      WRITE(IRPT,'(A)') '!   Program: '//TRIM(PRGNAM)
      WRITE(IRPT,'(A)') '!   Version: '//TRIM(PRGVER)
      WRITE(IRPT,'(A)') '!   Revised: '//TRIM(ADJUSTL(PRGDAT))
      WRITE(IRPT,'(A)') '!   User:    '//TRIM(USRNAM)
      WRITE(IRPT,'(A)') '!   Date:    '//TRIM(SYSDAT)
      WRITE(IRPT,'(A)') '!   Time:    '//TRIM(SYSTIM(1:8))
      WRITE(IRPT,'(A)') '!   File:    '//TRIM(FNDAT)
      DO ISPC = 1, NUM_SPC
!
!       No CONSUME keywords for plants
        IF( SPECIE(ISPC)%TYPE(2:2) .EQ. 'P' ) CYCLE
!
!       Count the number of prey so we can determine the form of the CONSUME keyword
        NUM_PREY = 0
        DO JSPC = 1, NUM_SPC
          IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) NUM_PREY = NUM_PREY + 1
        END DO
!
!       Labels for both aquatic and terrestrial animals depend on whether they eat anything
        IF( NUM_PREY .GT. 0 ) THEN
          WRITE(IRPT,1190) TRIM(SPECIE(ISPC)%ID)
 1190     FORMAT('!'/'CONSUME ID="',A,'" PREY')
        ELSE
          WRITE(IRPT,1200) TRIM(SPECIE(ISPC)%ID)
 1200     FORMAT('!'/'CONSUME ID="',A,'"')
        END IF
!
!       Consumption for both aquatic and terrestrial animals
        DO JSPC = 1, NUM_SPC
          IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) WRITE(IRPT,1210) SPECIE(JSPC)%ID, PREDATE(ISPC,JSPC)
 1210     FORMAT(2X,'"',A,'"',1X,F8.5)
        END DO
!
!       Aquatic Animals eat sediment
        IF( SPECIE(ISPC)%TYPE(1:1) .EQ. 'Q' ) WRITE(IRPT,1220) 'SEDING ', SPECIE(ISPC)%SEDING
 1220   FORMAT(2X,A,2X,F8.5)
!
!       Terrestrial Animals eat soil
        IF( SPECIE(ISPC)%TYPE(1:1) .EQ. 'T' ) WRITE(IRPT,1220) 'SOILING', SPECIE(ISPC)%SOILING
!
      END DO
!
! *** Write out the predation matrix
!
      WRITE(IRPT,2010) (SPECIE(ISPC)%ID,ISPC=1,NUM_SPC)
 2010 FORMAT(/'Predation matrix '/' ID',4X,150(2X,A,:))
      DO ISPC = 1, NUM_SPC
        WRITE(IRPT,2020) SPECIE(ISPC)%ID, (PREDATE(ISPC,JSPC),JSPC=1,NUM_SPC)
      END DO
 2020 FORMAT(1X,A,150(1X,F7.4):/,9(7X,150(1X,F7.4):/))
!
      RETURN
      END
!
      SUBROUTINE ECHO(  )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the species definition
!!    to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jul 2003 : Version 1.0
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
! *** Local variables
!
      INTEGER :: ISPC ! Species looping index
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//24('='),' Echo of the Problem Definition ',24('='))
!
! *** Files names used in the analysis
!
      WRITE(IRPT,1040) 'File Name for Input Data', TRIM(FNDAT)
      WRITE(IRPT,1040) 'File Name for the Report File', TRIM(FNRPT)
 1040 FORMAT(/A/'File: ',A)
!
! *** Species Definitions
!
      WRITE(IRPT,1060)
 1060 FORMAT(/'Species information listed in input order'/ &
              'Index Type      ID      Long Name'/ &
              '----- ----   --------   ',48('-'))
      DO ISPC = 1, NUM_SPC
        WRITE(IRPT,1070) ISPC, SPECIE(ISPC)%TYPE, SPECIE(ISPC)%ID, TRIM(SPECIE(ISPC)%NAME)
 1070   FORMAT(1X,I3,'  "',A2,'" : "',A,'" : "',A,'"')
      END DO
!
      WRITE(IRPT,1090) NUM_SPC
 1090 FORMAT('A total of ',I0,' species have been defined.')
!
      RETURN
      END
!
      SUBROUTINE ECHO2(  )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes and writes several things to the report file
!!      1. A listing of the species in the computation order
!!      2. A listing of consumption information
!!      3. A series of CONSUME keywords for use in ECEM
!!      4. An echo of the predation matrix
!!
!!  History:
!!
!!    Paul W. Eslinger :  7 Jul 2003 : Version 2.0
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
! *** Local variables
!
      INTEGER :: IDX      ! Index variable
      INTEGER :: ISPC     ! Species looping index
      INTEGER :: JSPC     ! Species looping index
!      INTEGER :: NUM_PREY ! Number of prey
!
!---- First executable code --------------------------------------------
!
! *** Species Definitions
!
      WRITE(IRPT,1060)
 1060 FORMAT(/'Species information listed in order of computation'/ &
              'Index Type      ID      Long Name'/ &
              '----- ----   --------   ',48('-'))
      DO ISPC = 1, NUM_SPC
        IDX = SPECIE(ISPC)%ORDER
        IF( IDX .LT. 0 ) CYCLE
        WRITE(IRPT,1070) IDX, SPECIE(IDX)%TYPE, SPECIE(IDX)%ID, TRIM(SPECIE(IDX)%NAME)
 1070   FORMAT(1X,I3,'  "',A2,'" : "',A,'" : "',A,'"')
      END DO
      WRITE(IRPT,1090) NUM_SPC
 1090 FORMAT('A total of ',I0,' species have been requested.')
!
! *** Output consumption information in an easy-to-read format
!
      DO ISPC = 1, NUM_SPC
        WRITE(IRPT,1160) ISPC, TRIM(SPECIE(ISPC)%ID), TRIM(SPECIE(ISPC)%NAME)
 1160   FORMAT(/'Consumption information for species ',I0,' : "',A,'" : "',A,'"')
        DO JSPC = 1, NUM_SPC
          IF( PREDATE(ISPC,JSPC) .GT. 0.0 ) THEN
            WRITE(IRPT,1170) PREDATE(ISPC,JSPC), JSPC, SPECIE(JSPC)%ID, TRIM(SPECIE(JSPC)%NAME)
 1170       FORMAT(3X,F7.4,' of species ',I3,' : "',A,'" : "',A,'"')
          END IF
        END DO
        IF( SPECIE(ISPC)%SEDING  .GT. 0.0 ) WRITE(IRPT,1180) SPECIE(ISPC)%SEDING,  ' of "sediment"'
        IF( SPECIE(ISPC)%SOILING .GT. 0.0 ) WRITE(IRPT,1180) SPECIE(ISPC)%SOILING, ' of "soil"'
 1180   FORMAT(3X,F7.4,A)
      END DO
!
      RETURN
      END
!
      SUBROUTINE IDEN_SET( )
!!**************************************************************************************************
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
!!    Paul W. Eslinger : 3 Jul 2003 : Version 1.0
!!    Paul W. Eslinger : 7 Aug 2007 : Update comments
!!    Paul W. Eslinger : 9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
!
      USE Iden_Mod
!
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
      PRGNAM = 'Consume'
      PRGVER = '4.0.001'
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
!!    Paul W. Eslinger :  3 Jul 2003 : Version 1.0
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------
!
! *** Input data file
!
      IDAT  = 9
      FNDAT = ' '
!
! *** Report file
!
      IRPT  = 10
      IRPT_ERR = IRPT
      REPORT = .FALSE.
!
! *** Species information
!
      NUM_SPC = 0
!
      RETURN
      END

      SUBROUTINE OPEN_DAT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine performs two actions for the input file:
!!      1. Obtain the file name and store it in the variable FNDAT
!!         a) First check the command line to see if the file
!!            name was provided or,
!!         b) query the user for the file name if it was not
!!            entered on the command line.
!!      2. Open the file on unit number IDAT
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jul 2003 : Version 1.0
!!    Paul W. Eslinger :  7 Aug 2007 : Revise for TIIA Version
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Iden_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR ! Output error flag
!                        0 = No errors
!                       >0 = Error in locating or opening the input file
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'OPEN_DAT'
      LOGICAL :: THERE         ! File existence variable
      INTEGER :: IERF          ! Status variable for open statement
      INTEGER :: NUM_ARGS      ! Number of command line arguments
      INTEGER :: NUM_FNAM      ! Argument number for the keyword file name
      CHARACTER(LEN=5) :: HELP ! Character for checking for help
!
!---- First executable code --------------------------------------------
!
      IERR = 0
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
        CALL GETARG( NUM_FNAM, FNDAT )
!
! ***   Special processing for command line help (print info and stop)
!       The argument -HELP (not case sensitive) is allowed
        HELP = FNDAT(1:5)
        CALL UPCASE( HELP )
        IF( HELP .EQ. '-HELP' ) THEN
          CALL COMMAND_LINE_HELP( )
          STOP
        END IF
!
      ELSE
        WRITE(*,1010)
 1010   FORMAT(/' Enter the input data file name > ')
        READ(*,*) FNDAT
      END IF
!
! *** Strip leading blanks (if any) from the file name
!
      IF( FNDAT .NE. ' ' ) FNDAT = ADJUSTL( FNDAT )
!
! *** Check if the requested file exists
      INQUIRE(FILE=FNDAT,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 1
        MESSAG(1) = 'The requested data file was not found'
        MESSAG(2) = 'File: '//TRIM(FNDAT)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Attempt to open the file
      OPEN(IDAT,FILE=FNDAT,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the input data file'
        MESSAG(2) = 'File: '//TRIM(FNDAT)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

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
!!    Paul W. Eslinger :  3 Jul 2003 : Change ESD_SPC to SPECIE for this code
!!                                     Change ESD_NUM_SPC to NUM_SPC
!!                                     Add test for plants on SOILING and SEDING
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
!
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
!
      CHARACTER(LEN=10) :: CALLER = 'PRED_CHECK'
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
!     Check the individual entries (allowed values are between 0 and 1, inclusive)
!----------------------------------------------------------------------------------
!
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        DO JSPC = 1, NUM_SPC
          IF( .NOT. SPECIE(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).LT.0.0 .OR. PREDATE(ISPC,JSPC).GT.1.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Consumption for a species is invalid (valid range is [0,1]).'
            MESSAG(2) = 'Predator was: ' // SPECIE(ISPC)%ID
            MESSAG(3) = 'Victim was: ' // SPECIE(JSPC)%ID
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
      DO ISPC = 1, NUM_SPC
!
!       Skip species not computed in this run
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
!
        ROWSUM = 0.0
        DO JSPC = 1, NUM_SPC
!         Skip species not computed in this run
          IF( .NOT. SPECIE(JSPC)%COMP ) CYCLE
          ROWSUM = ROWSUM + PREDATE(ISPC,JSPC)
        END DO
!
!       Check that plants do not eat soil or sediment
        IF( SPECIE(ISPC)%TYPE(2:2) .EQ. 'P' ) THEN
          IF( SPECIE(ISPC)%SEDING .GT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Sediment consumption for species: '// SPECIE(ISPC)%ID //' is not 0'
            MESSAG(2) = 'Species type is a plant'
            MESSAG(3) = 'The species must have a sediment consumption of 0'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            CYCLE
          END IF
          IF( SPECIE(ISPC)%SOILING .GT. 0.0 ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Soil consumption for species: '// SPECIE(ISPC)%ID //' is not 0'
            MESSAG(2) = 'Species type is a plant'
            MESSAG(3) = 'The species must have a soil consumption of 0'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            CYCLE
          END IF
        END IF
!
!       Check that plants have a rowsum of 0
        IF( SPECIE(ISPC)%TYPE(2:2) .EQ. 'P' ) THEN
          IF( ROWSUM .EQ. 0.0 ) THEN
            CYCLE
          ELSE
            IERR = IERR + 1
            MESSAG(1) = 'Total consumption for species: '// SPECIE(ISPC)%ID //' is not 0'
            MESSAG(2) = 'Species type is a plant'
            MESSAG(3) = 'The species must have a total consumption of 0'
            MESSAG(4) = 'Change the CONSUME keyword'
            CALL PRTERR( IERR, CALLER, 4 )
            CYCLE
          END IF
        END IF
!
!       Check that animals have a rowsum of 0 or 1
        IF( SPECIE(ISPC)%TYPE(2:2) .EQ. 'A' ) THEN
          IF( ABS(ROWSUM).LT.SMALL .OR. ABS(ROWSUM-1.0).LT.SMALL ) THEN
            IF( ABS(ROWSUM).LT.SMALL ) THEN
!             Sediment and soil ingestion requires food ingestion
              IF( SPECIE(ISPC)%TYPE.EQ.'TA' .AND. SPECIE(ISPC)%SOILING.GT.0.0 ) THEN
                IERR = IERR + 1
                MESSAG(1) = 'Total consumption for species: '// SPECIE(ISPC)%ID //' is 0'
                MESSAG(2) = 'Species type is a terrestrial animal'
                MESSAG(3) = 'The species must also have a soil consumption of 0'
                MESSAG(4) = 'Change the CONSUME keyword'
                CALL PRTERR( IERR, CALLER, 4 )
                CYCLE
              END IF
              IF( SPECIE(ISPC)%TYPE.EQ.'QA' .AND. SPECIE(ISPC)%SEDING.GT.0.0 ) THEN
                IERR = IERR + 1
                MESSAG(1) = 'Total consumption for species: '// SPECIE(ISPC)%ID //' is 0'
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
            MESSAG(1) = 'Total consumption for species: '// SPECIE(ISPC)%ID //' is not 0 or 1'
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
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        IF( PREDATE(ISPC,ISPC) .NE. 0.0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'Cannibal species not allowed. Species: '//SPECIE(ISPC)%ID
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
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        DO JSPC = 1, NUM_SPC
          IF( .NOT. SPECIE(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. SPECIE(ISPC)%TYPE(1:1).EQ.'Q' &
            .AND. SPECIE(JSPC)%TYPE(1:1).EQ.'T' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species: ' // SPECIE(ISPC)%ID // ' cannot eat species: ' // SPECIE(JSPC)%ID
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
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        DO JSPC = 1, NUM_SPC
          IF( .NOT. SPECIE(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. SPECIE(ISPC)%HABITAT.EQ.'UPLAND' &
            .AND. SPECIE(JSPC)%HABITAT.NE.'UPLAND' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species: ' // SPECIE(ISPC)%ID // ' cannot eat species: ' // SPECIE(JSPC)%ID
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
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        DO JSPC = 1, NUM_SPC
          IF( .NOT. SPECIE(JSPC)%COMP ) CYCLE
          IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. SPECIE(ISPC)%HABITAT.EQ.'RIPARIAN' &
            .AND. SPECIE(JSPC)%HABITAT.EQ.'UPLAND' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Species: ' // SPECIE(ISPC)%ID // ' cannot eat species: ' // SPECIE(JSPC)%ID
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
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        ISPCTMP(ISPC) = 0
      END DO
!
! *** If a row sum is 0 the species can always be computed
!
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        ROWSUM = 0.0
        DO JSPC = 1, NUM_SPC
          IF( .NOT. SPECIE(JSPC)%COMP ) CYCLE
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
      DO NLOOP = 1, NUM_SPC
        IF( .NOT. SPECIE(NLOOP)%COMP ) CYCLE
        DO ISPC = 1, NUM_SPC
          IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
          IF( ISPCTMP(ISPC) .GT. 0 ) CYCLE
          LTMP = .TRUE.
          DO JSPC = 1, NUM_SPC
            IF( .NOT. SPECIE(JSPC)%COMP ) CYCLE
            IF( PREDATE(ISPC,JSPC).GT.0.0 .AND. ISPCTMP(JSPC).EQ.0 ) THEN
              LTMP = .FALSE.
            END IF
          END DO
          IF( LTMP ) THEN
            NYES = NYES + 1
            ISPCTMP(ISPC) = NYES
          END IF
        END DO
        IF( NYES .EQ. NUM_SPC ) EXIT
      END DO
!
! *** Another look that consumed species are being modeled
!
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        DO JSPC = 1, NUM_SPC
          IF( PREDATE(ISPC,JSPC) .LE. 0.0 ) CYCLE
          IF( SPECIE(JSPC)%COMP ) CYCLE
          IERR = IERR + 1
          MESSAG(1) = 'Species: "' // TRIM(SPECIE(ISPC)%ID) // '" cannot be computed'
          MESSAG(2) = 'The food web requests consumption of species: "' // TRIM(SPECIE(JSPC)%ID) // '"'
          MESSAG(3) = 'The requested prey species is not being computed.'
          CALL PRTERR( IERR, CALLER, 3 )
        END DO
      END DO
!
! *** Final check on being able to complete the computations
!
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        IF( ISPCTMP(ISPC) .EQ. 0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'Species: ' // SPECIE(ISPC)%ID // ' cannot be computed'
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
      DO ISPC = 1, NUM_SPC
        IF( .NOT. SPECIE(ISPC)%COMP ) CYCLE
        SPECIE( ISPCTMP(ISPC) )%ORDER = ISPC
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE READ_DAT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the species and preadation matrix data
!!    from the data file.  This subroutine also allocates memory for
!!    the species related data.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jul 2003 : Version 1.0
!!    Paul W. Eslinger :  9 Oct 2007 : Upgrade error messages
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
!
! *** For explicit variable typing
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Output error flag
!                        0 = No errors, >0 = Error encountered
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'READ_DAT' ! Name of this subroutine
      INTEGER :: ISPC ! Species index
      INTEGER :: JSPC ! Species looping index
      INTEGER :: LINECNT ! Line counter
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Read the user name
      LINECNT = 1
      READ(IDAT,*,END=9995) USRNAM
!
! *** Read the number of species
      LINECNT = LINECNT + 1
      READ(IDAT,*,ERR=9996,END=9997) NUM_SPC
!
! *** Allocate memory for the species information
      CALL SPECIES_MEMORY( IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 9999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Initialize the species information
      CALL SPECIES_INITIAL( IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 9999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read the species information
!
      DO ISPC = 1, NUM_SPC
        LINECNT = LINECNT + 1
!
!       Read the data
        READ(IDAT,*,ERR=9996,END=9997) SPECIE(ISPC)%ID, SPECIE(ISPC)%TYPE, SPECIE(ISPC)%HABITAT, SPECIE(ISPC)%NAME
!        WRITE(*,*) SPECIE(ISPC)%ID, SPECIE(ISPC)%TYPE, SPECIE(ISPC)%HABITAT, SPECIE(ISPC)%NAME
!
!       Force the type to upper case for easier error checking
        CALL UPCASE( SPECIE(ISPC)%TYPE )
!
!       Force the habitat to upper case for easier error checking
        CALL UPCASE( SPECIE(ISPC)%HABITAT )
!
      END DO
!
! *** Read the predation matrix, soil ingestion and sediment ingestion
!
      DO ISPC = 1, NUM_SPC
        LINECNT = LINECNT + 1
!        WRITE(*,1010) ISPC, TRIM(SPECIE(ISPC)%ID)
! 1010   FORMAT(1X,I3,1X,A)
        READ(IDAT,*,ERR=9998,END=9999) (PREDATE(ISPC,JSPC),JSPC=1,NUM_SPC), SPECIE(ISPC)%SEDING, SPECIE(ISPC)%SOILING
      END DO
!
      RETURN
!
! *** Error branch on the read statements
!
 9995 CONTINUE
!
      IERR = 1
      MESSAG(1) = 'Error reading user name'
      MESSAG(2) = 'File: ' // TRIM(FNDAT)
      MESSAG(3) = 'Error at line number 1'
      CALL PRTERR( IERR, CALLER, 3 )
      RETURN
!
 9996 CONTINUE
!
      IERR = 2
      MESSAG(1) = 'Error reading reading species definitions'
      MESSAG(2) = 'File: ' // TRIM(FNDAT)
      MESSAG(3) = 'Error at or about line number '
      WRITE(MESSAG(3)(31:),'(I0)') LINECNT
      CALL PRTERR( IERR, CALLER, 3 )
      RETURN
!
 9997 CONTINUE
!
      IERR = 3
      MESSAG(1) = 'End of file reading reading species definitions'
      MESSAG(2) = 'File: ' // TRIM(FNDAT)
      MESSAG(3) = 'Error at or about line number '
      WRITE(MESSAG(3)(31:),'(I0)') LINECNT
      CALL PRTERR( IERR, CALLER, 3 )
      RETURN
!
 9998 CONTINUE
!
      IERR = 4
      MESSAG(1) = 'Error reading reading predation information'
      MESSAG(2) = 'File: ' // TRIM(FNDAT)
      MESSAG(3) = 'Error at or about line number '
      WRITE(MESSAG(3)(31:),'(I0)') LINECNT
      CALL PRTERR( IERR, CALLER, 3 )
      RETURN
!
 9999 CONTINUE
!
      IERR = 5
      MESSAG(1) = 'End of file reading reading predation information'
      MESSAG(2) = 'File: ' // TRIM(FNDAT)
      MESSAG(3) = 'Error at or about line number '
      WRITE(MESSAG(3)(31:),'(I0)') LINECNT
      CALL PRTERR( IERR, CALLER, 3 )
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE SPECIES_INITIAL( IERR )
!!**********************************************************************
!!
!! Purpose:
!!
!!    This subroutine provides initial values for the specie information
!!    whith memory allocated in SPECIES_MEMORY.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jul 2033 : Version 2.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE Control_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
!
      INTEGER :: ISPC, JSPC ! Species looping indices
      CHARACTER(LEN=15) :: CALLER = 'SPECIES_INITIAL' ! Name of this routine
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Check on the number of species
!
      IF( NUM_SPC .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least 1 specie required to initialize values'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** SPECIE : The species structure
!
      DO ISPC = 1, NUM_SPC
        SPECIE(ISPC)%ID      = ' '
        SPECIE(ISPC)%NAME    = ' '
        SPECIE(ISPC)%HABITAT = ' '
        SPECIE(ISPC)%ORDER   = -1
        SPECIE(ISPC)%TYPE    = ' '
        SPECIE(ISPC)%SOILING = 0.0
        SPECIE(ISPC)%SEDING  = 0.0
        SPECIE(ISPC)%COMP    = .TRUE.
      END DO
!
! *** PREDATE : Predation matrix
!
      DO ISPC = 1, NUM_SPC
        DO JSPC = 1, NUM_SPC
          PREDATE(ISPC,JSPC) = 0.0
        END DO
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE SPECIES_MEMORY( IERR )
!!**********************************************************************
!!
!! Purpose:
!!
!!    This subroutine allocates memory for variables that depend on the
!!    number of species.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Jul 2003 : Version 2.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE Control_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
!
      INTEGER :: IERA ! Error status variable from the allocate action
      CHARACTER(LEN=14) :: CALLER = 'SPECIES_MEMORY' ! Name of this routine
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Check on the number of species
!
      IF( NUM_SPC .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least 1 specie required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** SPECIE : The species structure
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
! *** PREDATE : Predation matrix
!
      ALLOCATE( PREDATE(NUM_SPC,NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating memory for PREDATE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** ISPCTMP : Species order work vector
!
      ALLOCATE( ISPCTMP(NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for ISPCTMP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

