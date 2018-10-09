!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
MODULE Stats_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains definitions for the stochastic distributions
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!===> Parameters or array dimensions for the statistical distributions
!
!     MAXDST : Maximum number of distributions including the constant
!              distribution assigned number 1
      INTEGER, PARAMETER :: MAXDST =  13
!
!     MAXGEN : Maximum number of values that can be generated in a
!              single call to SGEN (set in routine SET_STOCH_DIM)
      INTEGER :: MAXGEN
!
!     MAXSTO : Maximum number of stochastic variables that can be
!              defined (set in routine SET_STOCH_DIM)
      INTEGER :: MAXSTO
!
!     MAXTBL : Maximum number of table entries that can be made in
!              storing values for the piecewise uniform distribution
!              (set in routine SET_STOCH_DIM)
      INTEGER :: MAXTBL
!
!===> Variables controlling the amount of output
!
      LOGICAL :: BG_STOC_DEFN ! Logical flag controlling writing the
!       definition of the stochastic variables to the report file
!
      LOGICAL :: BG_STOC_STAT ! Logical flag controlling writing
!       statistics for computed values to the report file
!
      LOGICAL :: BG_STOC_VALU ! Logical flag controlling writing the
!       computed values of the stochastic variable to a file
!
!===> Character values for statistical distributions
!
!    DLABEL : Vector of distribution labels
     CHARACTER(LEN=18), DIMENSION(MAXDST) :: DLABEL
!
!    VLABEL : Vector of variable labels used to locate the parameters
!             for a specific distribution
     CHARACTER(LEN=24), ALLOCATABLE :: VLABEL(:)
!
!    VMESS  : Vector of messages associated with each variable
     CHARACTER(LEN=72), ALLOCATABLE :: VMESS(:)
!
!===> Numerical values for statistical distributions
!
!    INDSTO : Index for the last data storage in the variable and
!             parameter definition arrays
     INTEGER :: INDSTO
!
!    INDTBL : Index for last storage of the user CDF table values
!             in XUSER and UUSER
     INTEGER :: INDTBL
!
!    IUSER  : Array (MAXSTO,2) of indices for the user CDF table
!             distributions,
!              (I,1) gives the starting index for the Ith variable
!              (I,2) gives the number of values in the table for the
!              Ith variable
     INTEGER, ALLOCATABLE :: IUSER(:,:)
!
!    IWORK  : Integer work vector of length MAXGEN used in U01S1
     INTEGER, ALLOCATABLE :: IWORK(:)
!
!    WORK   : Real work vector of length MAXGEN
     REAL, ALLOCATABLE :: WORK(:)
!
!    RWORK  : Real work vector of length MAXGEN
     REAL, ALLOCATABLE :: RWORK(:)
!
!    UMIN   : Vector of length MAXSTO of minimum uniform values
     REAL, ALLOCATABLE :: UMIN(:)
!
!    UMAX   : Vector of length MAXSTO of maximum uniform values
     REAL, ALLOCATABLE :: UMAX(:)
!
!    UUSER  : Vector of all values of probability entries for piecewise
!             uniform distributions
     REAL, ALLOCATABLE :: UUSER(:)
!
!    VPARMS : Array of parameters for the statistical distribution for
!             all stochastic variables.  Up to 4 parameters are allowed.
!             The row of the matrix corresponds to a single variable
     REAL, ALLOCATABLE :: VPARMS(:,:)
!
!    VTYPE  : Vector of statistical distribution types for all stochastic
!             variables
     INTEGER, ALLOCATABLE :: VTYPE(:)
!
!    VTRUN  : Vector of truncation flags for statistical distributions
!             for all variables.  Truncation is performed in the space
!             of probabilities, rather than in the generated data space.
!                0 = no truncation, 1 = truncated on both tails
     INTEGER, ALLOCATABLE :: VTRUN(:)
!
!    XUSER  : Vector of all values of X entries for piecewise uniform
!             distributions
     REAL, ALLOCATABLE :: XUSER(:)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Stats_Mod
!
      SUBROUTINE SDECOD( TMPNAM, TMPMES, INDX, TRUNCATE, VALUE, NVALUE, IERR )
!!******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles decoding and storing the definition of
!!    a stochastic variable that was read from the keyword control
!!    file using the RDBLK keyword routines.
!!
!!  Notes:
!!
!!    The only error checking on statistical entries done in this
!!    routine is to check for a duplicate variable label.  Other error
!!    checking is performed in subroutine SERR after all keyword data
!!    has been read and stored.
!!
!!  Call list variables:
!!
!!    TMPNAM   : Character input label to be stored as the key to later
!!               location of stochastic information
!!    TMPMES   : Character input string to be stored as the message
!!               associated with the variable in TMPNAM
!!    INDX     : Index for the starting location in the vector VALUE for the
!!               data to be decoded.
!!    TRUNCATE : Logical flag, true if the truncate option is set for this
!!               distribution
!!    NVALUE   : Number of entries in the vector VALUE
!!    VALUE    : Vector of values decoded by RDBLK
!!    IERR     : Error flag, nonzero if no match is found
!!
!!  Auxiliary Routines:
!!
!!    PRTERR
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C : Add types 12 and 13
!!    Paul W. Eslinger : 25 Mar 2005 : Allow truncation on type 10
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!******************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: TMPNAM
      CHARACTER(LEN=*), INTENT(IN) :: TMPMES
      INTEGER, INTENT(IN) :: INDX
      LOGICAL, INTENT(IN) :: TRUNCATE
      INTEGER, INTENT(IN) :: NVALUE
      REAL, INTENT(IN), DIMENSION(*) :: VALUE
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER='SDECOD' ! Name of this subroutine
!
      INTEGER :: IDX   ! Local looping variable
      INTEGER :: IDIST ! Local distribution index
      INTEGER :: ICHK  ! Local looping variable
      INTEGER :: IDUP  ! Local check variable for duplicate entries
      INTEGER :: NUSER ! User CDF table local index
      INTEGER :: I     ! Local looping variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Increment the index for storing the information
!     Check that array dimensions are not exceeded
!
      INDSTO = INDSTO + 1
      IF( INDSTO .GT. MAXSTO) THEN
        IERR = 1
        MESSAG(1) = 'Array dimension exceeded - change the value for MAXSTO by'
        MESSAG(2) = 'modifying the second argument in the call to SET_STOCH_DIM'
        MESSAG(3) = 'Processing Variable: ' // TRIM(TMPNAM)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Check for duplicate variable definitions
!
      IF( INDSTO .GT. 1 ) THEN
        IDUP = 0
        DO ICHK = 1, INDSTO-1
          IF( TMPNAM .EQ. VLABEL(ICHK) ) IDUP = IDUP + 1
        END DO
        IF( IDUP .GT. 0 ) THEN
          IERR = 2
          MESSAG(1) = 'Duplicate definition for stochastic variables'
          MESSAG(2) = 'Processing Variable: ' // TRIM(TMPNAM)
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
! *** Store the label and description
!
      VLABEL(INDSTO) = TMPNAM
      VMESS(INDSTO)  = TMPMES
!
! *** Set up for extracting parameters from VALUE by distribution type
!     Start with the value at the INDX'th position
!
      IDX = INDX
      IF( IDX.LT.0 .OR. IDX.GT.NVALUE ) THEN
        IERR = 3
        MESSAG(1) = 'Bad index for the first entry to decode'
        MESSAG(2) = 'Index is less than 0, or greater than the '
        MESSAG(3) = 'number of values available from RDBLK'
        MESSAG(4) = 'Variable: ' // TMPNAM
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
      IDIST = VALUE(IDX)
      VTYPE(INDSTO) = IDIST
!
      SELECT CASE ( IDIST )
!
        CASE( 1 ) ! Constant distribution
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IF( IDX .GT. NVALUE ) THEN
            IERR = 4
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Constant Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 2 ) ! Uniform distribution
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 5
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Uniform Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 3 ) ! Discrete uniform distribution
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IF( IDX .GT. NVALUE ) THEN
            IERR = 6
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Discrete Uniform Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 4 ) ! Loguniform base 10
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 7
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Loguniform (10) Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 5 ) ! Loguniform base e
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 8
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Loguniform (e) Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 6 ) ! Triangular
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,3) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 9
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Triangular Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 7 ) ! Normal
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 10
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Normal Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 8 ) ! Lognormal base 10
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 11
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Lognormal (Base 10) Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 9 ) ! Lognormal base e
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 12
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Lognormal (Base e) Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 10 ) ! User CDF table
          IDX = IDX + 1
          NUSER = VALUE(IDX)
          IUSER(INDSTO,1) = INDTBL+1
          IUSER(INDSTO,2) = NUSER
          DO I = 1, NUSER
            INDTBL = INDTBL + 1
            IF( INDTBL .GT. MAXTBL) THEN
              IERR = 13
              MESSAG(1) = 'XUSER dimension exceeded - Variable: '//TMPNAM
              MESSAG(2) = 'Change the parameter MAXTBL and recompile, or'
              MESSAG(3) = 'User CDF Table memory allocation problem'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            IDX = IDX + 1
            UUSER(INDTBL) = VALUE(IDX)
            IDX = IDX + 1
            XUSER(INDTBL) = VALUE(IDX)
          END DO
          IF( IDX .GT. NVALUE ) THEN
            IERR = 14
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'User CDF Table'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          RETURN
!
        CASE( 11 ) ! Beta (c,d)
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,3) = VALUE(IDX)
          IDX = IDX + 1
          VPARMS(INDSTO,4) = VALUE(IDX)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 15
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Beta Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 12 ) ! Log Ratio from Normal
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX) ! Mean
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX) ! Standard deviation
          IDX = IDX + 1
          VPARMS(INDSTO,3) = VALUE(IDX) ! Lower limit (A)
          IDX = IDX + 1
          VPARMS(INDSTO,4) = VALUE(IDX) ! Upper limit (B)
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 16
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Normal Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE( 13 ) ! Hyperbolic Arcsine from Normal
          IDX = IDX + 1
          VPARMS(INDSTO,1) = VALUE(IDX) ! Mean
          IDX = IDX + 1
          VPARMS(INDSTO,2) = VALUE(IDX) ! Standard deviation
          IF( TRUNCATE ) THEN
            VTRUN(INDSTO) = 1
            IDX = IDX + 1
            UMIN(INDSTO) = VALUE(IDX)
            IDX = IDX + 1
            UMAX(INDSTO) = VALUE(IDX)
          END IF
          IF( IDX .GT. NVALUE ) THEN
            IERR = 17
            MESSAG(1) = 'More data requested than read by RDBLK'
            MESSAG(2) = 'Normal Distribution'
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
          RETURN
!
        CASE DEFAULT ! Invalid distribution index
          IERR = 18
          MESSAG(1) = 'Invalid distribution index'
          MESSAG(2) = 'Index = '
          WRITE(MESSAG(2)(9:),*) IDIST
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
      END SUBROUTINE
!
      SUBROUTINE SERR( VNAME, VIDX, IERR )
!!*********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine performs error checking on the definition of
!!    variables that are declared as stochastic variables.  The
!!    single definition associated with the variable identified in
!!    VNAME is checked for errors.  These checks satisfy two
!!    functions: 1) data for the variable has been read (subroutine
!!    SDECOD), and 2) the data is adequate for subroutine SGEN
!!    to function properly.
!!
!! Call list variables
!!
!!    VNAME : Input variable name
!!    VIDX  : Input integer index for data associated with VNAME,
!!            0 on entry means to find the proper index
!!    IERR  : Output error flag, nonzero if no match is found
!!
!!  Distributions Available:
!!
!!    IDIST Distribution   Parameters
!!    ----- ------------   -------------------------------------------
!!      1   Constant       PAR1 = Constant value
!!      2   Uniform        PAR1 = Lower limit, PAR2 = Upper limit
!!      3   Discrete       PAR1 = Smallest value, PAR2 = largest value
!!          Uniform
!!      4   Loguniform     (Base 10) PAR1 = Lower limit,
!!                         PAR2 = Upper limit
!!      5   Loguniform     (Base e) PAR1 = Lower limit,
!!                         PAR2 = Upper limit
!!      6   Triangular     PAR1 = Minimum, PAR2 = Mode,
!!                         PAR3 = Maximum
!!      7   Normal         PAR1 = Mean, PAR2 = Standard deviation
!!      8   Lognormal      (Base 10) PAR1 = Mean,
!!                         PAR2 = Standard deviation
!!      9   Lognormal      (Base e) PAR1 = Mean,
!!                         PAR2 = Standard deviation
!!     10   User CDF       User specified table of values
!!     11   Beta           PAR1 = alpha (exponent for x)
!!                         PAR2 = beta  (exponent for (1-x))
!!                         PAR3 = lower limit
!!                         PAR4 = upper limit
!!     12   Log ratio      PAR1 = Mean, PAR2 = Standard deviation (of normal)
!!          (from normal)  PAR3 = lower limit, PAR4 = upper limit
!!     13   Hyperbolic     PAR1 = Mean, PAR2 = Standard deviation (of normal)
!!          Arcsine (from normal)
!!
!!  Auxiliary Routines Needed:
!!
!!    FSINDX, PRTERR
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C : Add types 12 and 13
!!    Paul W. Eslinger : 28 Mar 2005 : Change type 10 looping on limits
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: VNAME ! Character variable name
      INTEGER :: VIDX ! Integer index for data associated with VNAME
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'SERR' ! Name of this routine
      REAL :: SMALL = 1.0E-25 ! Small value for identity checking
!
      REAL :: PAR1, PAR2, PAR3, PAR4 ! Local values of statistical parameters
      REAL :: UMINZ, UMAXZ ! Local uniform minimum and maximum
!
      INTEGER :: IBAD   ! Local bad data flag
      INTEGER :: ITRUNC ! Local truncation flag
      INTEGER :: IDIST  ! Local distribution index
      INTEGER :: I      ! Looping control variable
!
      INTEGER :: NSTRT     ! User CDF table starting index
      INTEGER :: NTEND     ! User CDF table ending index
      INTEGER :: NUSER     ! User CDF table local index
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize error-related variables
!
      IERR = 0
!
! *** Get the data index for the distribution if needed
!
      IF( VIDX .LE. 0 ) THEN
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine FSINDX'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
! *** Check on the distribution index
!
      IDIST = VTYPE(VIDX)
      IF( IDIST.LT.1 .OR. IDIST.GT.MAXDST ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid distribution index'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check for valid truncation flag value
!
      ITRUNC = VTRUN(VIDX)
      IF( ITRUNC.LT.0 .OR. ITRUNC.GT.1 ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid truncation flag'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Get the parameters
!
      PAR1 = VPARMS(VIDX,1)
      PAR2 = VPARMS(VIDX,2)
      PAR3 = VPARMS(VIDX,3)
      PAR4 = VPARMS(VIDX,4)
!
      UMINZ = UMIN(VIDX)
      UMAXZ = UMAX(VIDX)
!
      SELECT CASE ( IDIST )
!
        CASE( 1 ) ! Constant distribution
          RETURN
!
        CASE( 2 ) ! Uniform
          IF( PAR1 .GE. PAR2 ) THEN
            IERR = 3
            MESSAG(1) = 'Limits on the uniform distribution are reversed'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 4
            MESSAG(1) = 'Invalid truncation limits on the uniform distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 3 ) ! Discrete Uniform (Truncation does not apply)
          IF( PAR2 .LT. PAR1 ) THEN
            IERR = 5
            MESSAG(1) = 'Limits on the discrete uniform distribution are reversed'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 4 ) ! Log Uniform, base 10
          IF( PAR1 .GE. PAR2 ) THEN
            IERR = 6
            MESSAG(1) = 'Limits on the log 10 uniform distribution are reversed'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
            IERR = 7
            MESSAG(1) = 'Limits on the log 10 uniform distribution must be positive'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 8
            MESSAG(1) = 'Invalid truncation limits on the log 10 uniform distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 5 ) ! Log Uniform, base e
          IF( PAR1 .GE. PAR2 ) THEN
            IERR = 9
            MESSAG(1) = 'Limits on the log e uniform distribution are reversed'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
            IERR = 10
            MESSAG(1) = 'Limits on the log e uniform distribution must be positive'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 11
            MESSAG(1) = 'Invalid truncation limits on the loge uniform distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 6 ) ! Triangular
          IF( (PAR1.GT.PAR2) .OR. (PAR1.GE.PAR3) .OR. (PAR2.GT.PAR3) ) THEN
            IERR = 12
            MESSAG(1) = 'Limits on the triangular distribution are reversed'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( ABS(PAR3-PAR1) .LE. SMALL ) THEN
            IERR = 13
            MESSAG(1) = 'Invalid range for the triangular distribution'
            MESSAG(2) = 'Distribution range is too small'
            MESSAG(3) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 14
            MESSAG(1) = 'Invalid truncation limits on the triangular distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 7 ) ! Normal
          IF( PAR2 .LE. 0.0 ) THEN
            IERR = 15
            MESSAG(1) = 'Negative standard deviation for a normal distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 16
            MESSAG(1) = 'Invalid truncation limits on the normal distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 8 ) ! Lognormal, base 10
          IF( PAR2 .LE. 0.0 ) THEN
            IERR = 17
            MESSAG(1) = 'Negative standard deviation for log 10 normal distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 18
            MESSAG(1) = 'Invalid truncation limits on the log 10 normal distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 9 ) ! Lognormal, base e
          IF( PAR2 .LE. 0.0 ) THEN
            IERR = 19
            MESSAG(1) = 'Negative standard deviation for log e normal distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 20
            MESSAG(1) = 'Invalid truncation limits on the log e normal distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 10 ) ! User CDF table
          NSTRT = IUSER(VIDX,1)
          NUSER = IUSER(VIDX,2)
! ***     Check on the number of entries in the table
          IF( NUSER .LT. 1 ) THEN
            IERR = 21
            MESSAG(1) = 'NUSER - Number of values must be positive'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          NTEND = NSTRT + NUSER - 1
! ***     Check on the table indices
          IF( NSTRT.LT.1 .OR. NSTRT.GT.MAXTBL ) THEN
            IERR = 22
            MESSAG(1) = 'NSTRT - Invalid table index for user CDF table'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
! ***     Check on the table indices
          IF( NTEND.LT.1 .OR. NTEND.GT.MAXTBL ) THEN
            IERR = 23
            MESSAG(1) = 'NTEND - Invalid table index for user CDF table'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
! ***     Check that the probabilities range from 0 to 1
          IBAD = 0
          IF( UUSER(NSTRT) .NE. 0.0 ) IBAD = 1
          IF( ABS(UUSER(NTEND)-1.0) .LE. SMALL ) UUSER(NTEND) = 1.0
          IF( ABS(UUSER(NTEND)-1.0) .GT. SMALL ) IBAD = 1
          DO I = NSTRT, NTEND
             IF( UUSER(I).LT.0.0 .OR. UUSER(I).GT.1.0 ) IBAD = 1
             IF( I .EQ. NSTRT ) CYCLE
             IF( UUSER(I) .LE. UUSER(I-1) ) IBAD = 1
             IF( XUSER(I) .LT. XUSER(I-1) ) IBAD = 1
          END DO
          IF( IBAD .NE. 0 ) THEN
            IERR = 24
            MESSAG(1) = 'Invalid probability entries for the user CDF table, or'
            MESSAG(2) = 'invalid order on data values (must be strictly increasing)'
            MESSAG(3) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
        CASE( 11 ) ! Beta (c,d)
          IF( PAR1 .LE. 0.0 ) THEN
            IERR = 25
            MESSAG(1) = 'Illegal definition for the Beta distribution'
            MESSAG(2) = 'First (alpha) parameter invalid (must be >0)'
            MESSAG(3) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
          IF( PAR2 .LE. 0.0 ) THEN
            IERR = 26
            MESSAG(1) = 'Illegal definition for the Beta distribution'
            MESSAG(2) = 'Second (beta) parameter invalid (must be >0)'
            MESSAG(3) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
          IF( PAR4 .LE. PAR3 ) THEN
            IERR = 27
            MESSAG(1) = 'Illegal definition for the Beta distribution'
            MESSAG(2) = 'Lower limit must be less than the upper limit'
            MESSAG(3) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 28
            MESSAG(1) = 'Invalid truncation limits on the Beta distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 12 ) ! Log ratio from Normal
          IF( PAR2 .LE. 0.0 ) THEN
            IERR = 29
            MESSAG(1) = 'Negative standard deviation for a log ratio distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( PAR4 .LE. PAR3 ) THEN
            IERR = 30
            MESSAG(1) = 'Illegal definition for the log ratio distribution'
            MESSAG(2) = 'Lower limit must be less than the upper limit'
            MESSAG(3) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 16
            MESSAG(1) = 'Invalid truncation limits on the log ratio distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 13 ) ! Hyperbolic arcsine from Normal
          IF( PAR2 .LE. 0.0 ) THEN
            IERR = 29
            MESSAG(1) = 'Negative standard deviation for a hyperbolic arcsine distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( (UMINZ.LT.0.0  .OR. UMINZ.GT.1.0) .OR. &
              (UMAXZ.LT.UMINZ .OR. UMAXZ.GT.1.0) ) THEN
            IERR = 16
            MESSAG(1) = 'Invalid truncation limits on the log ratio distribution'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE DEFAULT ! Invalid distribution index
          IERR = 29
          MESSAG(1) = 'Invalid distribution index'
          MESSAG(2) = 'Variable: '//VNAME
          MESSAG(3) = 'Coding internal error encountered'
          MESSAG(4) = 'Should never have reached this error branch'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
!
      END SELECT
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE SGEN( VNAME, VIDX, DSEED, RVAL, NVAL, IERR )
!!*****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine will generate NVAL values from a specified
!!    probability distribution and place them in the vector RVAL.
!!
!!  Call list variables
!!
!!    VNAME : Input name of the variable that identifies which
!!            distribution to generate.
!!    VIDX  : Integer index for data associated with VNAME
!!            0 on entry means to find the proper index
!!    DSEED : Seed for the random number generator
!!    RVAL  : Output vector of length NVAL containing the generated
!!            random values from distribution IDIST
!!    NVAL  : Input number of values to generate
!!    IERR  : Output error flag, nonzero if no match is found
!!
!!  Notes:
!!
!!    1. Each distribution is generated using the Probability
!!       Integral Transformation method.  I.e., the random value
!!       X is found implicitly from the equation F(X)=U where
!!       U is a generated value from the uniform distribution,
!!       and F(X) is the cumulative distribution function for the
!!       random variable X.
!!
!!    2. Stratified sampling is used when NVAL is greater than 1.
!!
!!    3. Each distribution may be truncated between two limits that
!!       specified are specified as limits in the uniform(0,1)
!!       domain.
!!
!!    4. The user may specify a cumulative distribution function
!!       in the form of a table of values.
!!
!!    5. Adding or deleting a distribution requires changes in several
!!       places.  This subroutine, subroutines SERR, SDECOD, PRTDST,
!!       the vector DLABEL of distribution labels, and the parameter
!!       MAXDST.
!!
!!    6. If VIDX is 0, a search for the distribution associated with
!!       the label VNAME will be conducted.  If VIDX is greater than
!!       0 it will be used as the distribution index
!!
!!  Distributions Available:
!!
!!    IDIST Distribution   Parameters
!!    ----- ------------   -------------------------------------------
!!      1   Constant       PAR1 = Constant value
!!      2   Uniform        PAR1 = Lower limit, PAR2 = Upper limit
!!      3   Discrete       PAR1 = Smallest value, PAR2 = largest value
!!          Uniform
!!      4   Loguniform     (Base 10) PAR1 = Lower limit,
!!                         PAR2 = Upper limit
!!      5   Loguniform     (Base e) PAR1 = Lower limit,
!!                         PAR2 = Upper limit
!!      6   Triangular     PAR1 = Minimum, PAR2 = Mode,
!!                         PAR3 = Maximum
!!      7   Normal         PAR1 = Mean, PAR2 = Standard deviation
!!      8   Lognormal      (Base 10) PAR1 = Mean,
!!                         PAR2 = Standard deviation
!!      9   Lognormal      (Base e) PAR1 = Mean,
!!                         PAR2 = Standard deviation
!!     10   User CDF       User specified table of values
!!     11   Beta           PAR1 = alpha (exponent for x)
!!                         PAR2 = beta  (exponent for (1-x))
!!                         PAR3 = lower limit
!!                         PAR4 = upper limit
!!     12   Log ratio      PAR1 = Mean, PAR2 = Standard deviation (of normal)
!!          (from normal)  PAR3 = lower limit, PAR4 = upper limit
!!     13   Hyperbolic     PAR1 = Mean, PAR2 = Standard deviation (of normal)
!!          Arcsine (from normal)
!!
!!  Auxiliary Routines Needed:
!!
!!    FSINDX, PRTERR, PPND, USRDST, U01S1, U01, SSORTI
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C : Add types 12 and 13
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!*****************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: PPND
      REAL, EXTERNAL :: USRDST
      REAL, EXTERNAL :: ALBETA
      REAL, EXTERNAL :: XINBTA
!
! *** Call list variables
      CHARACTER(LEN=*) :: VNAME
      INTEGER :: VIDX
      REAL(KIND=8) :: DSEED
      REAL, DIMENSION(*) :: RVAL
      INTEGER :: NVAL
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'SGEN' ! Name of this routine
!
      REAL :: LOGBETA                ! Log of the complete Beta function
      REAL :: PAR1, PAR2, PAR3, PAR4 ! Local values of statistical parameters
!
      INTEGER :: IDIST     ! Local distribution index
      INTEGER :: I         ! Looping control variable
      REAL :: UMINZ, UMAXZ ! Local uniform minimum and maximum
      REAL :: UDEL         ! Local uniform delta
      REAL :: DELP         ! Local delta parameter
      REAL :: PL1          ! Local logarithm of parameter value
      REAL :: PL2          ! Local logarithm of parameter value
      REAL :: RV           ! Local random variable
      REAL :: SQP1         ! Local square root of parameters
      REAL :: SQP2         ! Local square root of parameters
      REAL :: BDIF         ! Local delta parameter
      INTEGER :: I1        ! Local integer parameter
      INTEGER :: I2        ! Local integer parameter
      INTEGER :: IDELP     ! Local integer parameter
      INTEGER :: IVAL      ! Local integer value
      INTEGER :: NSTRT     ! User CDF table starting index
      INTEGER :: NTEND     ! User CDF table ending index
      INTEGER :: NUSER     ! User CDF table local index
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize error-related variables
!
      IERR = 0
!
! *** Get the data index for the distribution if needed
!
      IF( VIDX .LE. 0 ) THEN
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected in lower level routine FSINDX'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
      IF( VIDX .GT. MAXSTO ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid value for distribution index (VIDX)'
        MESSAG(2) = 'Value is larger than the parameter MAXSTO'
        MESSAG(3) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Check on the distribution index
!
      IDIST = VTYPE(VIDX)
      IF( IDIST.LT.1 .OR. IDIST.GT.MAXDST ) THEN
        IERR = 2
        MESSAG(1) = 'IDIST - Invalid distribution index'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the number of values to generate
!
      IF( NVAL .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'NVAL: Number of realizations must be greater than 0'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( NVAL .GT. MAXGEN ) THEN
        IERR = 4
        MESSAG(1) = 'NVAL: Too many realizations'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the random number seed
!
      IF( DSEED.LT.1.0D0 .OR. DSEED.GT.2147483646.0D0) THEN
        IERR = 5
        MESSAG(1) = 'Invalid random seed (1 to 2147483646 allowed)'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Get the parameters
!
      PAR1 = VPARMS(VIDX,1)
      PAR2 = VPARMS(VIDX,2)
      PAR3 = VPARMS(VIDX,3)
      PAR4 = VPARMS(VIDX,4)
!
! *** Constant distribution
!
      IF( IDIST .EQ. 1 ) THEN
        DO I = 1, NVAL
          RVAL(I) = PAR1
        END DO
        RETURN
      END IF
!
! *** Generate the set of Uniform(0,1) values with stratification
!
      CALL U01S1( NVAL, RVAL, IWORK, DSEED, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error detected in lower level routine U01S1'
        MESSAG(2) = 'Internal coding error unexpectedly encountered'
        MESSAG(3) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Check on the uniform limits
!
      UMINZ = UMIN(VIDX)
      UMAXZ = UMAX(VIDX)
      IF( UMINZ.LT.0.0 .OR. UMAXZ.GT.1.0 .OR. UMINZ.GE.UMAXZ ) THEN
        IERR = 5
        MESSAG(1) = 'Invalid truncation limits'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Implement the (possible) range truncation on the uniform values
!     before inversion to data values.
!
      IF( UMINZ.GT.0.0 .OR. UMAXZ.LT.1.0 ) THEN
        UDEL = UMAXZ - UMINZ
        DO I = 1, NVAL
          RVAL(I) = UMINZ + UDEL*RVAL(I)
        END DO
      END IF
!
!-----------------------------------------------------------------------
!         Choose the distribution type and generate the values
!-----------------------------------------------------------------------
!
      SELECT CASE ( IDIST )
!
        CASE( 2 ) ! Uniform (PAR1,PAR2)
          DELP = PAR2 - PAR1
          DO I = 1, NVAL
            RVAL(I) = PAR1 + DELP*RVAL(I)
          END DO
!
        CASE( 3 ) ! Discrete Uniform (PAR1,PAR2)
          I1 = PAR1
          I2 = PAR2
          IDELP = I2 - I1 + 1
          DO I = 1, NVAL
            IVAL = IDELP * RVAL(I)
            RVAL(I) = I1 + IVAL
          END DO
!
        CASE( 4 ) ! Log Uniform, base 10
          PL1 = ALOG10( PAR1 )
          PL2 = ALOG10( PAR2 )
          DELP = PL2 - PL1
          DO I = 1, NVAL
            RV = PL1 + DELP * RVAL(I)
            RVAL(I) = 10.0**RV
          END DO
!
        CASE( 5 ) ! Log Uniform, base e
          PL1 = ALOG( PAR1 )
          PL2 = ALOG( PAR2 )
          DELP = PL2 - PL1
          DO I = 1, NVAL
            RV = PL1 + DELP * RVAL(I)
            RVAL(I) = EXP( RV )
          END DO
!
        CASE( 6 ) ! Triangular
          DELP = (PAR2-PAR1) / (PAR3-PAR1)
          SQP1 = SQRT( (PAR3-PAR1)*(PAR2-PAR1) )
          SQP2 = SQRT( (PAR3-PAR1)*(PAR3-PAR2) )
          DO I = 1, NVAL
            IF( RVAL(I) .LE. DELP ) THEN
              RVAL(I) = PAR1 + SQP1*SQRT( RVAL(I) )
            ELSE
              RVAL(I) = PAR3 - SQP2*SQRT( 1.0-RVAL(I) )
            END IF
          END DO
!
        CASE( 7 ) ! Normal
          DO I = 1, NVAL
            RV = RVAL(I)
            RVAL(I) = PAR2*PPND(RV,IERR) + PAR1
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected in lower level routine PPND'
              MESSAG(2) = 'Variable: '//VNAME
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END DO
!
        CASE( 8 ) ! Lognormal, base 10
          DO I = 1, NVAL
            RV = RVAL(I)
            RVAL(I) = 10.0**(PAR2*PPND(RV,IERR) + PAR1)
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected in lower level routine PPND'
              MESSAG(2) = 'Variable: '//VNAME
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END DO
!
        CASE( 9 ) ! Lognormal, base e
          DO I = 1, NVAL
            RV = RVAL(I)
            RVAL(I) = EXP( PAR2*PPND(RV,IERR) + PAR1 )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected in lower level routine PPND'
              MESSAG(2) = 'Variable: '//VNAME
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END DO
!
        CASE( 10 ) ! User CDF table
          NSTRT = IUSER(VIDX,1)
          NUSER = IUSER(VIDX,2)
! ***     Check on the number of entries in the table
          IF( NUSER .LT. 2 ) THEN
            IERR = 7
            MESSAG(1) = 'NUSER - Number of values for user CDF table must be >1'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          NTEND = NSTRT + NUSER - 1
! ***     Check on the table indices
          IF( NSTRT.LT.1 .OR. NSTRT.GT.MAXTBL ) THEN
            IERR = 8
            MESSAG(1) = 'NSTRT - Invalid table index for user CDF table'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
! ***     Check on the table indices
          IF( NTEND.LT.1 .OR. NTEND.GT.MAXTBL ) THEN
            IERR = 9
            MESSAG(1) = 'NTEND - Invalid table index for user CDF table'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          DO I = 1, NVAL
!            RVAL(I) = USRDST(RVAL(I),NUSER,UUSER(NSTRT),XUSER(NSTRT),IERR)
            RVAL(I) = USRDST( RVAL(I), NSTRT, NTEND, IERR)
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected in lower level routine USRDST'
              MESSAG(2) = 'Variable: '//VNAME
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END DO
!
        CASE( 11 ) ! Beta distribution
          LOGBETA = ALBETA( PAR1, PAR2, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected in lower level routine ALBETA'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          BDIF = PAR4 - PAR3
          DO I = 1, NVAL
            RVAL(I) = XINBTA( PAR1, PAR2, LOGBETA, RVAL(I), IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected in lower level routine XINBTA'
              MESSAG(2) = 'Variable: '//VNAME
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            RVAL(I) = PAR3 + BDIF*RVAL(I)
          END DO
!
        CASE( 12 ) ! Log ratio from Normal
          DO I = 1, NVAL
            RV = RVAL(I)
            RVAL(I) = PAR2*PPND(RV,IERR) + PAR1
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected in lower level routine PPND'
              MESSAG(2) = 'Variable: '//VNAME
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            RV = EXP(RVAL(I))
            RVAL(I) = (PAR4*RV+PAR3)/(1.0+RV)
          END DO
!
        CASE( 13 ) ! Hyperbolic Arcsine from Normal
          DO I = 1, NVAL
            RV = RVAL(I)
            RVAL(I) = SINH( PAR2*PPND(RV,IERR) + PAR1 )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected in lower level routine PPND'
              MESSAG(2) = 'Variable: '//VNAME
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END DO
!
        CASE DEFAULT ! Invalid distribution index
          IERR = 10
          MESSAG(1) = 'Invalid distribution index'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
      RETURN
      END SUBROUTINE
!
      REAL FUNCTION U01( DSEED )
!!*********************************************************************
!!
!!  Purpose:
!!
!!    This function generates a real random uniform(0,1) variate
!!    given an initial value of the seed, DSEED.  The input seed
!!    is updated to a new seed using a linear congruential method.
!!
!!  Implementation:
!!
!!    The current usage requires a system with at least a 32 bit
!!    word length for a REAL type variable.  Internal double
!!    precision computations are performed.
!!
!!  Call List:
!!
!!    DSEED : Double precision seed for random number generator.
!!            On input, it must take a value between 1.0 and
!!            2147483646.0, inclusive.  On output, it has been
!!            updated to a new value in the same range.
!!
!!  Note:
!!
!!    The user must provide an initial value for DSEED.  The value
!!    of DSEED should never be modified by the user after a call
!!    to U01.
!!
!!  Auxiliary Routines Needed:
!!
!!    None
!!
!!  Reference:
!!
!!    Lewis, Goodman, and Miller.  1969.
!!    "A Pseudo-Random Number Generator for the System/360"
!!    IBM Systems Journal, Vol. 8, No. 2, pp. 136-145.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!
!!*********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL(KIND=8) :: DSEED ! Random seed
!
! *** Local variables
      REAL(KIND=8) :: B ! Multiplier for the function
      REAL(KIND=8) :: M ! Modulus for the function
!
! *** Data definitions
      DATA B /      16807.0D+00 /
      DATA M / 2147483647.0D+00 /
!
!---- Executable code ---------------------------------------------------
!
      DSEED = IDINT( DMOD(B*DSEED,M) )
      U01   = SNGL( DSEED/M )
!
      RETURN
      END FUNCTION
!
      SUBROUTINE U01S1( NVAL, RVAL, IWORK, DSEED, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine computes a vector of randomly generated numbers on
!!    the interval (0,1) with stratification using equal probable
!!    intervals and exactly one value per strata.
!!
!!  Auxiliary Routines Needed:
!!
!!    SSORTI, U01, PRTERR
!!
!!  Algorithm:
!!
!!    The algorithm implemented divides the interval (0,1) into N
!!    subintervals of length 1/N.  A random value is generated in
!!    each subinterval using the function U01.  Finally, the set of
!!    generated values is rearranged into a random order.
!!
!!  Call List:
!!
!!    DSEED  : Input - Double precision
!!             Seed for the random number generator. It must in the
!!             range 1.0 and 2147483646.0 inclusive.
!!    NVAL   : Input - Integer
!!             Number of random values to generate, also, the number
!!             of strata to use
!!    RVAL   : Output - Real
!!             Vector of length NVAL of uniform (0,1) values
!!    IWORK  : Input - Integer
!!             Work array of length NVAL
!!    IERR   : Output - Integer
!!              0 = No errors
!!              1 = Illegal value for NVAL
!!              2 = Illegal value for DSEED
!!              9999 = Error detected in a lower level routine
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger : 28 Mar 2005 : Add roundoff error logic
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: U01
!
! *** Call list variables
      INTEGER, INTENT(IN) :: NVAL
      REAL, DIMENSION(*) :: RVAL
      INTEGER, DIMENSION(*) :: IWORK
      REAL(KIND=8) :: DSEED
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'U01S1' ! Name of this routine
!
      INTEGER :: I     ! Local looping variable
      INTEGER :: KFLAG ! Local sort type selection flag
      REAL :: RN       ! Local sample size (as a real type)
      REAL :: RIM1     ! Local index (as a real type)
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize error-related variables
!
      IERR = 0
!
! *** Check on proper number of entries in the vector
!
      IF( NVAL .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid number of data'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Check for a legal random number seed
!
      IF( DSEED.LT.1.0D0 .OR. DSEED.GT.2147483646.0D0) THEN
        IERR = 2
        MESSAG(1) = 'Invalid seed'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** The stratified samples will be generated sequentially across
!     the interval [0,1] - they must then be randomly permuted.
!     Generate a vector of values from the uniform distribution
!     and use their ranks to permute the generated values.
!
      DO I = 1, NVAL
        RVAL(I) = U01( DSEED )
        IWORK(I)  = I
      END DO
!
! *** After the sort, the work vector contains the ranks for doing
!     the permutations
!
      KFLAG = 2
      CALL SSORTI( RVAL, IWORK, NVAL, KFLAG, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 9999
        MESSAG(1) = 'Error detected in a lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Generate the stratified values sequentially and store them in
!     the permuted locations
!
      RN = NVAL
      DO I = 1, NVAL
        RIM1 = I - 1
        RVAL(IWORK(I)) = (RIM1 + U01(DSEED)) / RN
!
!       PWE - Add logic to prevent roundoff to 0 or 1
        IF( RVAL(IWORK(I)) .EQ. 0.0 )  RVAL(IWORK(I)) = TINY(1.0)
        IF( RVAL(IWORK(I)) .EQ. 1.0 )  RVAL(IWORK(I)) = 0.9999999
!       PWE - Add logic to prevent roundoff to 0 or 1
!
      END DO
!
      RETURN
      END SUBROUTINE
!
      REAL FUNCTION USRDST( RV, NSTRT, NTEND, IERR )
!!*********************************************************************
!!
!!  Purpose:
!!
!!    This function transforms a uniform value on the interval [0,1]
!!    to a value from a user specified distribution.  The user
!!    specified distribution is defined data points (UUSER(I),XUSER(I)).
!!    The algorithm used is linear interpolation.
!!
!!  Notes:
!!
!!    Error checking on NSTRT and NTEND are assumed to be performed
!!    by the calling routine.  This improves the efficiency of the
!!    code when generating large numbers of realizations.
!!
!!  Formal Parameters:
!!
!!    RV    : Input - Real: The uniform value to transform to a value
!!            from the specified distribution.
!!
!!    NSTRT : Input - Integer: The starting location in the (UUSER,XUSER)
!!            data for the definition of this distribution.
!!
!!    NTEND : Input - Integer: The ending location in the (UUSER,XUSER)
!!            data for the definition of this distribution.
!!
!!    XUSER : Input - Real Vector: The distribution values
!!            corresponding to the percentiles in UUSER.
!!
!!    UUSER : Input - Real Vector: The percentiles corresponding to
!!            the values of the distribution given in XUSER.
!!
!!    IERR  : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error encountered
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!
!!*********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Stats_Mod, ONLY: UUSER, XUSER
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL, INTENT(IN) :: RV
      INTEGER, INTENT(IN) :: NSTRT
      INTEGER, INTENT(IN) :: NTEND
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'USRDST' ! Name of this routine
      REAL :: DELU ! Local work variable
      INTEGER :: I ! Local looping variable
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize error-related variables
!
      IERR = 0
!
! *** Check on the range of the input uniform value
!
      IF( RV.LE.0.0 .OR. RV.GE.1.0 ) THEN
        IERR = 3
        MESSAG(1) = 'Invalid uniform value'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Map the uniform value to an X value
!
      USRDST = 0.0
      DO I = NSTRT+1, NTEND
        IF( UUSER(I-1).LE. RV .AND. RV.LE.UUSER(I) ) THEN
          DELU = (RV-UUSER(I-1)) / (UUSER(I)-UUSER(I-1))
          USRDST = XUSER(I-1) + DELU * (XUSER(I)-XUSER(I-1))
          RETURN
        END IF
      END DO
!
! *** Error Check
!
      IERR = 4
      MESSAG(1) = 'Unexpected error - Invalid UUSER percentiles'
      CALL PRTERR( IERR, CALLER, 1 )
!
      RETURN
      END FUNCTION
!
      SUBROUTINE USTAT( X, N, UWORK, XMIN, XV01, XV05, XV10, XV25, XMED, &
        XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine will compute some summary statistics for
!!    the N values in the vector X.
!!
!!  Change History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  3 Jul 2001 : Add small sample percentiles
!!    Paul W. Eslinger :  9 Jan 2002 : Add 1st and 99th percentiles
!!    Paul W. Eslinger :  1 Jun 2007 : Fix indices for small sample sizes
!!
!!  Auxiliary Routines Required:
!!
!!    SORT : A subroutine to do a sort on a REAL vector
!!
!!  Variable Descriptios:
!!
!!    Inputs
!!    ----------------------------------------------------
!!    X    : Vector of input values.
!!    N    : Number of values in the vector X.
!!    UWORK : A work vector with the same dimension as X.
!!
!!    Outputs
!!    ----------------------------------------------------
!!    XMIN : Minimum value in X.
!!    XV05 :  1% percentile of the values in X.
!!    XV05 :  5% percentile of the values in X.
!!    XV10 : 10% percentile of the values in X.
!!    XV25 : 25% percentile of the values in X.
!!    XMED : 50% percentile of the values in X (median)
!!    XV75 : 75% percentile of the values in X.
!!    XV90 : 90% percentile of the values in X.
!!    XV95 : 95% percentile of the values in X.
!!    XV95 : 99% percentile of the values in X.
!!    XMAX : Maximum value in X.
!!    XAVG : Average (mean) value in X.
!!    XSTD : Standard deviation of the values in X.
!!    IERR : Error flag.
!!             0 : Normal termination
!!            >0 : Error encountered
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
      REAL, DIMENSION(*), INTENT(IN) :: X
      INTEGER :: N
      REAL, DIMENSION(N) :: UWORK
      REAL :: XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX
      REAL :: XAVG, XSTD
      INTEGER :: IERR
!
! *** Local variables
      REAL(KIND=8) :: VSUM
      INTEGER :: IP01, IP05, IP10, IP25, IP50, IP75, IP90, IP95, IP99
      CHARACTER(LEN=5) :: CALLER = 'USTAT' ! Name of this routine
      INTEGER :: I ! Local looping variable
      INTEGER :: KFLAG ! Local sort type selection flag
!
      INTEGER, DIMENSION(25,11) :: IPCTS ! Matrix of percentiles by sample size
!                             Min, .01, .05, .10, .25, .50, .75, .90, .95, 0.99, Max
!
! *** Data initialization
      DATA IPCTS  /1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                   1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                   1,1,1,1,1,1,1,1,2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                   1,1,1,1,1,2,2,2,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
                   1,1,2,2,2,2,3,3,4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, &
                   1,1,2,2,3,3,4,4,5, 5, 6, 6, 7, 7, 8, 8, 9, 9,10,10,11,11,12,12,13, &
                   1,2,2,3,4,4,5,5,6, 7, 8, 9,10,11,12,12,13,13,15,15,16,16,18,19,19, &
                   1,2,3,3,5,5,6,6,7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23, &
                   1,2,3,4,5,5,7,7,8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24, &
                   1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25, &
                   1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25/
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize and check for error on entry
!
      XMIN = 0.0
      XV01 = 0.0
      XV05 = 0.0
      XV10 = 0.0
      XV25 = 0.0
      XMED = 0.0
      XV75 = 0.0
      XV90 = 0.0
      XV95 = 0.0
      XV99 = 0.0
      XMAX = 0.0
!
      XAVG = 0.0
      XSTD = 0.0
!
! *** Error check on the sample size
!
      IERR = 0
      IF( N .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'Sample size less than 1'
        CALL PRTERR( IERR, CALLER, 1 )
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
!
! *** Set up indices for limits
!
      IF( N .GT. 25 ) THEN
        IP01 = 0.01*N
        IP01 = MAX( IP01, 1 )
        IP05 = 0.05*N
        IP05 = MAX( IP05, 1 )
        IP10 = 0.10*N
        IP10 = MAX( IP10, 1 )
        IP25 = 0.25*N
        IP25 = MAX( IP25, 1 )
        IP50 = 0.50*N
        IP50 = MAX( IP50, 1 )
        IP75 = 0.75*N
        IP75 = MAX( IP75, 1 )
        IP90 = 0.90*N
        IP90 = MAX( IP90, 1 )
        IP95 = 0.95*N
        IP95 = MAX( IP95, 1 )
        IP99 = 0.99*N
        IP99 = MAX( IP99, 1 )
      ELSE
        IP01 = IPCTS(N, 2)
        IP05 = IPCTS(N, 3)
        IP10 = IPCTS(N, 4)
        IP25 = IPCTS(N, 5)
        IP50 = IPCTS(N, 6)
        IP75 = IPCTS(N, 7)
        IP90 = IPCTS(N, 8)
        IP95 = IPCTS(N, 9)
        IP99 = IPCTS(N,10)
      END IF
!
! *** Store the percentile values
!
      XMIN = UWORK(1)
      XV01 = UWORK(IP01)
      XV05 = UWORK(IP05)
      XV10 = UWORK(IP10)
      XV25 = UWORK(IP25)
      XMED = UWORK(IP50)
      XV75 = UWORK(IP75)
      XV90 = UWORK(IP90)
      XV95 = UWORK(IP95)
      XV99 = UWORK(IP99)
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
      IF( N .EQ. 1 ) THEN
        RETURN
      ELSE
        VSUM  = 0.0D0
        DO I = 1, N
          VSUM  = VSUM  + (UWORK(I)-XAVG)**2
        END DO
        XSTD = SQRT( VSUM/DBLE(N-1) )
      END IF
!
! *** Normal exit
!
      RETURN
      END SUBROUTINE
!
      REAL FUNCTION ALBETA( A, B, IERR )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  Purpose:
!!
!!    This function computes the natural logarithm of the Beta(A,B)
!!    function for A and B greater than zero.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Original source
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! *** Global parameters, variables, and arrays
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: ALGAMA
!
! *** Call list variables
      REAL, INTENT(IN) :: A ! Alpha value
      REAL, INTENT(IN) :: B ! Beta value
      INTEGER :: IERR ! Error flag, nonzero if an error is detected
!
! *** Local variables
      CHARACTER(LEN= 6) :: CALLER = 'ALBETA' ! Name of this routine
!
      REAL :: APB                 ! Local sum of A and B
      INTEGER :: IER1, IER2, IER3 ! Error number indicators
!
!---- Executable code --------------------------------------------------
!
      ALBETA = 0.0
!
! *** Check for legal inputs
!
      IERR = 0
!
      IF( A .LE. 0.0  ) THEN
        IERR = 1
        MESSAG(1) = 'The parameter A for the Beta(A,B) function is invalid'
        MESSAG(2) = 'A value greater the 0 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( B .LE. 0.0  ) THEN
        IERR = 2
        MESSAG(1) = 'The parameter B for the Beta(A,B) function is invalid'
        MESSAG(2) = 'A value greater the 0 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Use the Beta - Gamma functional relationship
!
      APB = A + B
      ALBETA = ALGAMA(A,IER1) + ALGAMA(B,IER2) - ALGAMA(APB,IER3)
      IF( IER1.NE.0 .OR. IER2.NE.0 .OR. IER3.NE.0  ) THEN
        IERR = 3
        MESSAG(1) = 'The log of Gamma(A), Gamma(B) or Gamma(A+B) is invalid'
        MESSAG(2) = 'Unexpected error from the function ALGAMA'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END FUNCTION
!
      REAL FUNCTION ALGAMA( X, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This function computes the natural logarithm of the Gamma
!!    function at the real argument X when X is greater than zero.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Original source
!!
!!  Reference:
!!
!!    Algorithm ACM 291
!!    M.C. Pike and I.D. Hill
!!    Communications of the ACM. (1966)
!!    Vol. 9, P. 684
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL :: X
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN= 6) :: CALLER = 'ALGAMA' ! Name of this routine
!
      REAL :: F ! Local variable
      REAL :: Y ! Local variable
      REAL :: Z ! Local variable
!
      REAL :: ZERO  = 0.0 ! Local named number
      REAL :: HALF  = 0.5 ! Local named number
      REAL :: ONE   = 1.0 ! Local named number
      REAL :: SEVEN = 7.0 ! Local named number
!
      REAL :: A1 = 0.918938533204673 ! ALOG(2*PI)/2
      REAL :: A2 = 0.000595238095238 ! 1/1680
      REAL :: A3 = 0.000793650793651 ! 1/1260
      REAL :: A4 = 0.002777777777778 ! 1/360
      REAL :: A5 = 0.083333333333333 ! 1/12
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
      ALGAMA = ZERO
!
      IF( X .LE. ZERO ) THEN
        IERR = 1
        MESSAG(1) = 'The parameter X for the log Gamma function is invalid'
        MESSAG(2) = 'A value greater the 0 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      Y = X
      F = ZERO
      IF( Y .GT. SEVEN ) GO TO 30
      F = Y
   10 Y = Y + ONE
      IF( Y .GE. SEVEN ) GO TO 20
      F = F * Y
      GO TO 10
   20 F = -ALOG(F)
   30 Z = ONE / (Y*Y)
!
      ALGAMA = F + (Y-HALF) * ALOG(Y) - Y + A1 + (((-A2*Z+A3)*Z-A4)*Z+A5)/Y
!
      RETURN
      END FUNCTION
!
      REAL FUNCTION BETAIN( X, P, Q, LOGBETA, IERR )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  Purpose:
!!
!!    This function computes the ratio of the incomplete Beta function
!!    with the complete Beta function for arguments X between zero and
!!    one, and P and Q positive.
!!
!!    Another way to state the purpose is: This subroutine evaluates
!!    the cumulative distribution function at the location X of the
!!    Beta distribution with parameters P and Q.
!!
!!  Formal Parameters:
!!
!!    Variable  Definition
!!    --------  --------------------------------------------------------
!!    X         Input - Real: The value X at which to evaluate the
!!              Beta distribution on (0,1) with parameters P and Q.
!!
!!    P         Input - Real: The first parameter (i.e. X**P) for the
!!              Beta density.
!!
!!    Q         Input - Real: The second parameter for the Beta density.
!!
!!    LOGBETA   Input - Real : The natural log of the complete Beta
!!              function using the parameters P and Q on the
!!              interval (0,1).
!!
!!  Reference:
!!
!!    Algorithm AS 63
!!    Applied Statistics (1973),
!!    Vol. 22, No. 3
!!
!!    Modified as per remark ASR 19
!!    Applied Statistics (1977),
!!    Vol. 26, No. 1
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Original Source
!!    Paul W. Eslinger : 31 May 2007 : Revise comments, add IMPLICIT NONE
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! *** Global parameters, variables, and arrays
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL, INTENT(IN) :: LOGBETA
      REAL, INTENT(IN) :: X
      REAL, INTENT(IN) :: P
      REAL, INTENT(IN) :: Q
      INTEGER :: IERR ! Error flag, nonzero if an error is detected
!
! *** Local variables
      CHARACTER(LEN= 6) :: CALLER = 'BETAIN' ! Name of this routine
      LOGICAL :: BINDEX ! Local flag for tail choice to maintain accuracy
      REAL :: ACU, PSQ, CX, XX, PP, QQ, AI, TERM, RX, TEMP
      INTEGER :: NS
!
! *** Define accuracy and initialize
      DATA ACU /0.1E-7/
!
!---- Executable code --------------------------------------------------
!
! *** Test for admissibility of arguments
!
      IERR = 0
!
      IF( P .LE. 0.0 ) THEN
        IERR = 1
        MESSAG(1) = 'The first parameter for the BETA distribution is invalid'
        MESSAG(2) = 'A value greater than 0 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( Q .LE. 0.0 ) THEN
        IERR = 2
        MESSAG(1) = 'The second parameter for the BETA distribution is invalid'
        MESSAG(2) = 'A value greater than 0 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( X.LT.0.0 .OR. X.GT.1.0 ) THEN
        IERR = 3
        MESSAG(1) = 'The uniform entry for the BETA distribution is invalid'
        MESSAG(2) = 'A value between 0 and 1 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Correct return for the endpoints of the interval (0,1)
!
      BETAIN = X
      IF( X.EQ.0.0 .OR. X.EQ.1.0 ) RETURN
!
! *** Change tail if necessary and determine S
!
      PSQ = P + Q
      CX  = 1.0 - X
      IF( P .GE. PSQ*X ) THEN
        XX = X
        PP = P
        QQ = Q
        BINDEX = .FALSE.
      ELSE
        XX = CX
        CX = X
        PP = Q
        QQ = P
        BINDEX = .TRUE.
      END IF
!
      AI = 1.0
      NS = QQ + CX * PSQ
      TERM   = 1.0
      BETAIN = 1.0
!
! *** Use Soper's reduction formulae
!
      RX = XX / CX
    3 TEMP = QQ - AI
      IF( NS .EQ. 0 ) RX = XX
    4 TERM = TERM * TEMP * RX / (PP + AI)
      BETAIN = BETAIN + TERM
      TEMP = ABS(TERM)
      IF( TEMP.LE.ACU .AND. TEMP.LE.(ACU*BETAIN) ) GO TO 5
      AI = AI + 1.0
      NS = NS - 1
      IF( NS .GE. 0 ) GO TO 3
      TEMP = PSQ
      PSQ = PSQ + 1.0
      GO TO 4
!
! *** Calculate result
!
    5 CONTINUE
      BETAIN = BETAIN * EXP(PP * ALOG(XX) + (QQ - 1.0) * ALOG(CX) - LOGBETA) / PP
      IF( BINDEX ) BETAIN = 1.0 - BETAIN
!
      RETURN
      END FUNCTION
!
      REAL FUNCTION XINBTA( P, Q, LOGBETA, ALPHA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes the inverse of the Beta distribution
!!    defined on the interval (0,1) with parameters P and Q.
!!
!!
!!  Formal Parameters:
!!
!!    Variable  Definition
!!    --------  --------------------------------------------------------
!!    ALPHA     Input - Real: The uniform value to invert to the value
!!              XINBTA from a Beta distribution.
!!
!!    P         Input - Real: The first parameter (i.e. X**P) for the
!!              Beta density.
!!
!!    Q         Input - Real: The second parameter for the Beta density.
!!
!!    LOGBETA   Input - Real: The natural log of the complete Beta
!!              function for the interval (0,1).
!!
!!
!!  Reference:
!!
!!    Algorithm AS 64/AS 109
!!    Applied Statistics
!!    Journal of the Royal Statistical Society (Series C)
!!    1977, Vol. 26, p. 111
!!
!!    Also in:
!!    Applied Statistics Algorithms
!!    P. Griffiths and I. D. Hill, Editors
!!    Ellis Horwood Limited
!!    Chichester, England  1985
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Underflow error checking added
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: BETAIN
!
! *** Call list variables
      REAL :: P, Q, LOGBETA, ALPHA
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN= 6) :: CALLER = 'XINBTA' ! Name of this routine
!
      LOGICAL :: BINDEX ! Local flag for tail choice to maintain accuracy
      REAL :: A, PP, QQ, R, Y, T, XEXP, S, H, W, PREV, YPREV, SQ, G, ADJ, TX
!
! *** Define accuracy and initialize
!
      REAL :: ZERO, HALF, ONE, TWO, THREE, FOUR, FIVE, SIX, NINE
      DATA    ZERO, HALF, ONE, TWO, THREE, FOUR, FIVE, SIX, NINE &
             / 0.0,  0.5, 1.0, 2.0,   3.0,  4.0,  5.0, 6.0,  9.0 /
!
      REAL :: ACU, LOWER,  UPPER,  CONST1,  CONST2,  CONST3,  CONST4
      DATA    ACU, LOWER,  UPPER,  CONST1,  CONST2,  CONST3,  CONST4 &
        / 1.0E-14, 0.0001, 0.9999, 2.30753, 0.27061, 0.99229, 0.04481 /
!
      REAL SMALL, SMLEXP
      DATA SMALL, SMLEXP / 1.0E-7, -75.0 /
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
      XINBTA = ALPHA
!
! *** Test for abmissibility of parameters
!
      IF( P .LE. 0.0 ) THEN
        IERR = 1
        MESSAG(1) = 'The first parameter for the BETA distribution is invalid'
        MESSAG(2) = 'A value greater than 0 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( Q .LE. 0.0 ) THEN
        IERR = 2
        MESSAG(1) = 'The second parameter for the BETA distribution is invalid'
        MESSAG(2) = 'A value greater than 0 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( ALPHA.LT.0.0 .OR. ALPHA.GT.1.0 ) THEN
        IERR = 3
        MESSAG(1) = 'The uniform entry for the BETA distribution is invalid'
        MESSAG(2) = 'A value between 0 and 1 is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( ALPHA.EQ.ZERO .OR. ALPHA.EQ.ONE ) RETURN
!
! *** Change tail if necessary
!
      IF( ALPHA .LE. HALF ) THEN
        A  = ALPHA
        PP = P
        QQ = Q
        BINDEX = .FALSE.
      ELSE
        A  = ONE - ALPHA
        PP = Q
        QQ = P
        BINDEX = .TRUE.
      END IF
!
! *** Calculate the initial approximation
!
      R = SQRT( -ALOG(A*A) )
      Y = R - (CONST1+CONST2*R) / (ONE + (CONST3+CONST4*R)*R)
      IF( PP.GT.ONE .AND. QQ.GT.ONE ) GO TO 30
      R = QQ + QQ
      T = ONE / (NINE*QQ)
      T = R * (ONE-T+Y*SQRT(T))**3
      IF( T .LE. ZERO ) GO TO 10
      T = (FOUR*PP + R - TWO) / T
      IF( T .LE. ONE ) GO TO 20
      XINBTA = ONE - TWO/(T+ONE)
      GO TO 40
   10 CONTINUE
!
! *** Check for underflow error on initial approximation
!
!      XINBTA = ONE - EXP( (ALOG((ONE-A)*QQ) + LOGBETA) / QQ )
      XEXP = (ALOG((ONE-A)*QQ) + LOGBETA) / QQ
      IF( XEXP .GT. SMLEXP ) THEN
        XINBTA = ONE - EXP( XEXP )
      ELSE
        XINBTA = ONE - SMALL
      END IF
      GO TO 40
   20 CONTINUE
!
! *** Check for underflow error on initial approximation
!
!      XINBTA = EXP(  ( ALOG(A*PP) + LOGBETA ) / PP )
      XEXP = ( ALOG(A*PP) + LOGBETA ) / PP
      IF( XEXP .GT. SMLEXP ) THEN
        XINBTA = EXP( XEXP )
      ELSE
        XINBTA = SMALL
      END IF
      GO TO 40
   30 CONTINUE
      R = (Y*Y-THREE) / SIX
      S = ONE / (PP+PP-ONE)
      T = ONE / (QQ+QQ-ONE)
      H = TWO / (S+T)
      W = Y*SQRT(H+R)/H - (T-S)*(R+FIVE/SIX-TWO/(THREE*H))
      XINBTA = PP / (PP+QQ*EXP(W+W))
!
! *** Solve for X by a modified Newton-Rahpson method,
!     using the function BETAIN
!
   40 CONTINUE
      R = ONE - PP
      T = ONE - QQ
      YPREV = ZERO
      SQ = ONE
      PREV = ONE
      IF( XINBTA .LT. LOWER ) XINBTA = LOWER
      IF( XINBTA .GT. UPPER ) XINBTA = UPPER
   50 CONTINUE
      Y = BETAIN( XINBTA, PP, QQ, LOGBETA, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error in lower routine BETAIN'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
   60 CONTINUE
      Y = (Y-A) * EXP( LOGBETA + R*ALOG(XINBTA) + T*ALOG(ONE-XINBTA) )
      IF( Y*YPREV .LE. ZERO ) PREV = SQ
      G = ONE
   70 CONTINUE
      ADJ = G*Y
      SQ = ADJ*ADJ
      IF( SQ .GE. PREV ) GO TO 80
      TX = XINBTA - ADJ
      IF( TX.GE.ZERO .AND. TX.LE.ONE ) GO TO 90
   80 CONTINUE
      G = G / THREE
      GO TO 70
   90 CONTINUE
      IF( PREV .LE. ACU ) GO TO 100
      IF( Y*Y .LE. ACU ) GO TO 100
      IF( TX.EQ.ZERO .OR. TX.EQ.ONE ) GO TO 80
      IF( TX .EQ. XINBTA ) GO TO 100
      XINBTA = TX
      YPREV = Y
      GO TO 50
  100 CONTINUE
      IF( BINDEX ) XINBTA = ONE - XINBTA
!
      RETURN
      END FUNCTION
!
      SUBROUTINE INIT_STOCH( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes global variables for the stochstic
!!    generation suite of subroutines.
!!
!!  Calling Hierarchy
!!
!!    This subroutine must be preceeded by a call to STOCH_MEMORY
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: I ! Loop control variable
!
!---- Executable code ---------------------------------------------------
!
! *** Debug and output values
!
      BG_STOC_VALU = .FALSE. ! Output flag for printing stochastic values
      BG_STOC_DEFN = .FALSE. ! Output flag for definition of stochastic variables
      BG_STOC_STAT = .FALSE. ! Output flag for statistics on stochastic values
!
! *** Statistical distribution labels
!
      DLABEL( 1) = 'Constant          '
      DLABEL( 2) = 'Uniform           '
      DLABEL( 3) = 'Discrete Uniform  '
      DLABEL( 4) = 'Loguniform (10)   '
      DLABEL( 5) = 'Loguniform (e)    '
      DLABEL( 6) = 'Triangular        '                                                                       
      DLABEL( 7) = 'Normal            '
      DLABEL( 8) = 'Lognormal (10)    '
      DLABEL( 9) = 'Lognormal (e)     '
      DLABEL(10) = 'User CDF Table    '
      DLABEL(11) = 'Beta              '
      DLABEL(12) = 'Log ratio         '
      DLABEL(13) = 'Hyperbolic Arcsine'
!
! *** Indices for storing statistical distribution information
!
      INDSTO = 0
      INDTBL = 0
!
! *** Initialize by the number of stochastic distributions
!
      DO I = 1, MAXSTO
        VLABEL(I) = ' '
        UMIN(I) = 0.0
        UMAX(I) = 1.0
        IUSER(I,1) = 0
        IUSER(I,2) = 0
        VTYPE(I) = 0
        VTRUN(I) = 0
        VPARMS(I,1) = 0.0
        VPARMS(I,2) = 0.0
        VPARMS(I,3) = 0.0
        VPARMS(I,4) = 0.0
      END DO
!
! *** Initialize by the number of stochastic table entries
!
      DO I = 1, MAXTBL
        UUSER(I) = 0.0
        XUSER(I) = 0.0
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE STOCH_MEMORY( NSTOCH, NTABLE, NREAL, IERR )
!!**********************************************************************
!!
!! Purpose:
!!
!!    This subroutine schedules stochastic memory allocation for all
!!    of the variables needed for the stochastic variables generation
!!    routines.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
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
      INTEGER :: IERR   ! Error flag, nonzero if no error
      INTEGER :: NSTOCH ! Number of stochastic variables
      INTEGER :: NTABLE ! Number of table entries
      INTEGER :: NREAL  ! Number of realizations
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'STOCH_MEMORY' ! Name of this routine
      INTEGER :: NTABUSE ! Local table index (so a minimum of 1 is used)
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Error check on NSTOCH here
!
      IF( NSTOCH .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'The number of stochastic variables must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Error check on NTABLE here (use a minimum of 1)
!
      IF( NTABLE .LT. 1 ) THEN
        NTABUSE = 1
      ELSE
        NTABUSE = NTABLE
      END IF
!
! *** Error check on NREAL here
!
      IF( NREAL .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'The number of realizations must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Allocate memory for arrays depending on the number of stochastic variables
!
      CALL SET_STOCH_DIM( 'VARIABLE', .FALSE., NSTOCH, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine SET_STOCH_DIM'
        MESSAG(2) = 'Not able to allocate memory for stochastic variables'
        MESSAG(3) = 'Processing group "VARIABLE"'
        MESSAG(4) = 'Terminal error encountered'
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Allocate memory depending on the number of table entries
!
      CALL SET_STOCH_DIM( 'TABLE', .FALSE., NTABUSE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine SET_STOCH_DIM'
        MESSAG(2) = 'Not able to allocate memory for stochastic variables'
        MESSAG(3) = 'Processing group "TABLE"'
        MESSAG(4) = 'Terminal error encountered'
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Allocate memory depending on the number of realizations
!
      CALL SET_STOCH_DIM( 'WORK', .FALSE., NREAL,  IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine SET_STOCH_DIM'
        MESSAG(2) = 'Not able to allocate memory for stochastic variables'
        MESSAG(3) = 'Processing group "WORK"'
        MESSAG(4) = 'Terminal error encountered'
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE STONE( VNAM, VIDX, NREAL, SDSTOC, VOUT, IVLU, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations for one stochastic
!!    variable associated with the character string in VNAM.
!!
!!  Call list definitions
!!
!!     VNAM   : Character label identifying the stochastic variable
!!     VIDX   : Stochastic variable index
!!     NREAL  : Number of realizations to generate
!!     SDSTOC : Input seed for the random number generator
!!     VOUT   : Ouput vector of NREAL generated values
!!     IVLU   : Unit number for optional output messages
!!     IERR   : Output integer error flag
!!                 0 = No errors
!!                >0 = Error here or in lower level routine
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 26 Sep 2000 : Version 1.00.B
!!    Paul W. Eslinger : 31 May 2007 : Revise output file number logic
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!    Paul W. Eslinger : 13 Sep 2007 : Revise debug output on values
!!
!!**********************************************************************
!
! *** Global Variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list definitions
      CHARACTER(LEN=*), INTENT(IN) :: VNAM ! Label identifying the stochastic variable
      INTEGER             :: VIDX   ! Index into stored data for this variable
      INTEGER, INTENT(IN) :: NREAL  ! Number of realizations to generate
      REAL(KIND=8)        :: SDSTOC ! Input seed for the random number generator
      REAL, DIMENSION(*)  :: VOUT   ! Ouput vector of NREAL generated values
      INTEGER, INTENT(IN) :: IVLU   ! Unit number for optional output messages
      INTEGER             :: IERR   ! Output integer error flag
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'STONE' ! Name of this routine for error messages
!
      INTEGER :: IREL ! Local looping variable
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
      REAL, ALLOCATABLE :: PWORK(:) ! Temporary work vector of length NREAL
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Error check on the inputs
!
      IF( NREAL .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'No realizations are requested'
        MESSAG(2) = 'Variable: '//TRIM(VNAM)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Get the data index for the distribution
!     Allow for option that index is already set
!
      IF( VIDX .LE. 0 ) THEN
        CALL FSINDX( VNAM, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected in lower level routine FSINDX'
          MESSAG(2) = 'Variable: '//TRIM(VNAM)
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      ELSE
        IF( VIDX .GT. INDSTO ) THEN
          IERR = 2
          MESSAG(1) = 'Index value (VIDX) was greater than the number of distributions'
          MESSAG(2) = 'Variable: '//TRIM(VNAM)
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
! *** Error check the variable definition
!
      CALL SERR( VNAM, VIDX, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error detected in lower level routine SERR'
        MESSAG(2) = 'Variable: '//TRIM(VNAM)
        MESSAG(3) = 'Label: '//TRIM(VMESS(VIDX))
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Optionally write the variable definition to the output file
!
      IF( BG_STOC_DEFN ) THEN
        CALL PRTDST( VNAM, VIDX, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected in lower level routine PRTDST'
          MESSAG(2) = 'Variable: '//TRIM(VNAM)
          MESSAG(3) = 'Label: '//TRIM(VMESS(VIDX))
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
      END IF
!
! *** Generate the vector of values
!
      CALL SGEN( VNAM, VIDX, SDSTOC, VOUT, NREAL, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error detected in lower level routine SGEN'
        MESSAG(2) = 'Variable: '//TRIM(VNAM)
        MESSAG(3) = 'Label: '//TRIM(VMESS(VIDX))
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Optionally compute and output statistics for this sample
!
      IF( BG_STOC_STAT ) THEN
!
        ALLOCATE( PWORK(NREAL) )
!
        CALL USTAT( VOUT, NREAL, PWORK, XMIN, XV01, XV05, XV10, XV25, XMED, &
          XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine USTAT'
          MESSAG(2) = 'Variable: '//TRIM(VNAM)
          MESSAG(3) = 'Label: '//TRIM(VMESS(VIDX))
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
        CALL PRTSTAT( VNAM, VMESS(VIDX), XMIN, XV01, XV05, XV10, XV25, XMED, &
          XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IVLU )
!
        DEALLOCATE( PWORK )
!
      END IF
!
! *** Optionally output the generated values in the output vector
!
      IF( BG_STOC_VALU ) THEN
!        WRITE(IVLU,1010) TRIM(VNAM), (VOUT(IREL),IREL=1,NREAL)
! 1010   FORMAT('"',A,'"',1P,1000(:,',',E12.5))
        CALL STBUG( VNAM, NREAL, VOUT, IVLU, 'ROW', IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected in lower level routine STBUG'
          MESSAG(2) = 'Variable: '//TRIM(VNAM)
          MESSAG(3) = 'Label: '//TRIM(VMESS(VIDX))
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE STBUG( VNAM, NREAL, VOUT, IVLU, ORIENT, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes all realizations for one stochastic
!!    variable associated with the character string in VNAM to an
!!    open file on unit IVLU
!!
!!  Call list definitions
!!
!!     VNAM   : Character string identifying the stochastic variable
!!     NREAL  : Number of realizations
!!     VOUT   : Vector of NREAL generated values
!!     IVLU   : Unit number for outputs
!!     ORIENT : Output values in a row or in a column
!!                 COLUMN = Use a column
!!                 Anything else means use a row
!!     IERR   : Output integer error flag
!!                 0 = No errors
!!                >0 = Error here or in lower level routine
!!
!!  History:
!!
!!    Paul W. Eslinger : 13 Sep 2007 : Original source
!!
!!**********************************************************************
!
! *** Global Variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list definitions
      CHARACTER(LEN=*)   :: VNAM   ! Label identifying the stochastic variable
      INTEGER            :: NREAL  ! Number of realizations
      REAL, DIMENSION(*) :: VOUT   ! Vector of NREAL generated values
      INTEGER            :: IVLU   ! Unit number for output messages
      CHARACTER(LEN=*)   :: ORIENT ! Orientation of output values (ROW or COLUMN)
      INTEGER            :: IERR   ! Output integer error flag
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'STBUG' ! Name of this routine for error messages
!
      INTEGER :: IREL ! Local looping variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Error check on the inputs
!
      IF( NREAL .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Realization count is invalid'
        MESSAG(2) = 'Variable: '//TRIM(VNAM)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Output the values
!
      IF( ORIENT .EQ. 'COLUMN' ) THEN
        WRITE(IVLU,1010) TRIM(VNAM)
 1010   FORMAT('"',A,'"')
        DO IREL = 1, NREAL
          WRITE(IVLU,1020) VOUT(IREL)
 1020     FORMAT(1P,E13.6)
        END DO
      ELSE
        IF( NREAL .GT. 10000 ) THEN
          IERR = 2
          MESSAG(1) = 'Realization count is too large for format statement'
          MESSAG(2) = 'Use of the COLUMN modifier is suggested'
          MESSAG(3) = 'Variable: '//TRIM(VNAM)
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
        WRITE(IVLU,1030) TRIM(VNAM), (VOUT(IREL),IREL=1,NREAL)
 1030   FORMAT('"',A,'"',1P,10000(:,',',E12.5))
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE FSINDX( VNAME, VIDX, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a variable name
!!    in VNAME and a variable label in the vector VLABEL.  If found,
!!    the index identifies the storage location for all data defining
!!    the distribution of the variable.  If not found, an error message
!!    is written.
!!
!!  Auxiliary Routines Needed:
!!
!!    PRTERR
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Stats_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: VNAME ! Character variable name
      INTEGER :: IERR ! Error flag, nonzero if no match is found
      INTEGER :: VIDX ! Integer index for data associated with VNAME
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'FSINDX' ! Name of this routine
      INTEGER :: I ! Local looping variable
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize error-related variables
!
      IERR = 0
!
! *** Look for a match on the variable labels
!
      VIDX = 0
      DO I = 1, MAXSTO
        IF( VNAME .EQ. VLABEL(I) ) THEN
          VIDX = I
          RETURN
        END IF
      END DO
!
! *** Error exit when a match is not found
!
      IERR = 1
      MESSAG(1) = 'No match for index for ' // VNAME
      CALL PRTERR( IERR, CALLER, 1 )
!
      RETURN
      END SUBROUTINE
!
      REAL FUNCTION PPND( P, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This function evaluates the normal deviate corresponding to the
!!    lower tail area of P.  In other words, it computes the inverse
!!    of the standard normal cumulative distribution function.  An
!!    error flag (IERR) is passed back to the calling routine.
!!
!!  Reference:
!!
!!    Algorithm AS 111
!!    The Percentage Points of the Normal Distribution
!!    Applied Statistics (1977)
!!    Journal of the Royal Statistical Society, Series C
!!    Vol. 26, No. 1
!!
!!  Notes:
!!
!!    1. The hash sums are the sums of the moduli of the coefficients
!!       they have no inherent meanings, but can be used for checking
!!       transcriptions.
!!
!!    2. Internal double precision has been added by P.W. Eslinger
!!
!!    3. IERR test locations and structure is different from the
!!       algorithm that was published.
!!
!!    4. The error flag IERR has the following definition:
!!         IERR = 0, no errors encountered
!!         IERR = 1, the value for P did not satisfy 0<P<1
!!
!!  Auxiliary Routines Needed:
!!
!!    PRTERR
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger : 31 May 2007 : Revise comments
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
      REAL :: P ! Lower tail area of a normal variable
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'PPND' ! Name of this routine
!
      REAL(KIND=8) :: ZERO, SPLIT, HALF, ONE
      REAL(KIND=8) :: A0, A1, A2, A3, B1, B2, B3, B4, C0, C1, C2, C3
      REAL(KIND=8) :: D1, D2, DP, Q, R
!
! *** Data definitions
!
      DATA ZERO /0.0D0/, HALF /0.5D0/, ONE /1.0D0/
      DATA SPLIT /0.42D0/
!
      DATA A0 /   2.50662823884D0/
      DATA A1 / -18.61500062529D0/
      DATA A2 /  41.39119773534D0/
      DATA A3 / -25.44106049637D0/
!
      DATA B1 /  -8.47351093090D0/
      DATA B2 /  23.08336743743D0/
      DATA B3 / -21.06224101826D0/
      DATA B4 /   3.13082909833D0/
!
! *** HASH SUM AB 143.70383558076
!
      DATA C0 /  -2.78718931138D0/
      DATA C1 /  -2.29796479134D0/
      DATA C2 /   4.85014127135D0/
      DATA C3 /   2.32121276858D0/
!
      DATA D1 /   3.54388924762D0/
      DATA D2 /   1.63706781897D0/
!
! *** HASH SUM CD  17.43746520924
!
!---- Executable code ---------------------------------------------------
!
      DP = DBLE( P )
      PPND = ZERO
!
! *** Error checking on P
!
      IERR = 0
      IF( DP.LE.ZERO .OR. DP.GE.ONE ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid tail probability.'
        MESSAG(2) = 'Valid limits are 0 to 1, noninclusive'
        MESSAG(3) = 'Value entered WAs: '
        WRITE(MESSAG(3)(20:),*) P
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Algorithm
!
      Q = DP - HALF
      IF( DABS(Q) .LE. SPLIT ) THEN
        R = Q * Q
        PPND = SNGL(Q * (((A3 * R + A2) * R + A1) * R + A0) / &
          ((((B4 * R + B3) * R + B2) * R + B1) * R + ONE))
      ELSE
        R = DP
        IF( Q .GT. ZERO ) R = ONE - DP
        R = DSQRT( -DLOG(R) )
        PPND = SNGL((((C3 * R + C2) * R + C1) * R + C0) / &
          ((D2 * R + D1) * R + ONE))
        IF( Q .LT. ZERO ) PPND = -PPND
      END IF
!
      RETURN
      END FUNCTION
!
      SUBROUTINE PRTDST( VNAME, VIDX, IVLU, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints the definition of the statistical
!!    distribution for a single variable to the report file.
!!
!!  Call list variables
!!
!!    VNAME : Input variable name
!!    VIDX  : Input index into stored data for variable VNAM
!!    IERR  : Output error flag, nonzero indicates error
!!
!!  Distributions Available:
!!
!!    IDIST Distribution   Parameters
!!    ----- ------------   -------------------------------------------
!!      1   Constant       PAR1 = Constant value
!!      2   Uniform        PAR1 = Lower limit, PAR2 = Upper limit
!!      3   Discrete       PAR1 = Smallest value, PAR2 = largest value
!!          Uniform
!!      4   Loguniform     (Base 10) PAR1 = Lower limit,
!!                         PAR2 = Upper limit
!!      5   Loguniform     (Base e) PAR1 = Lower limit,
!!                         PAR2 = Upper limit
!!      6   Triangular     PAR1 = Minimum, PAR2 = Mode,
!!                         PAR3 = Maximum
!!      7   Normal         PAR1 = Mean, PAR2 = Standard deviation
!!      8   Lognormal      (Base 10) PAR1 = Mean,
!!                         PAR2 = Standard deviation
!!      9   Lognormal      (Base e) PAR1 = Mean,
!!                         PAR2 = Standard deviation
!!     10   User CDF       User specified table of values
!!     11   Beta           PAR1 = alpha (exponent for x)
!!                         PAR2 = beta  (exponent for (1-x))
!!                         PAR3 = lower limit
!!                         PAR4 = upper limit
!!     12   Log ratio      PAR1 = Mean, PAR2 = Standard deviation (of normal)
!!          (from normal)  PAR3 = lower limit, PAR4 = upper limit
!!     13   Hyperbolic     PAR1 = Mean, PAR2 = Standard deviation (of normal)
!!          Arcsine (from normal)
!!
!!  Auxiliary Routines Needed:
!!
!!    FSINDX, PRTERR
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C : Add types 12 and 13
!!    Paul W. Eslinger :  4 Sep 2002 : Update format statements
!!    Paul W. Eslinger : 26 Sep 2005 : Update format statements
!!    Paul W. Eslinger : 31 May 2007 : Revise output file number logic
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: VNAME ! Variable name
      INTEGER :: VIDX ! Index into stored data for variable VNAM
      INTEGER :: IVLU ! Unit number for the output file
      INTEGER :: IERR ! Error flag, nonzero indicates error
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'PRTDST' ! Name of this routine
      REAL :: PAR1, PAR2, PAR3, PAR4 ! Local values of statistical parameters
      INTEGER :: NSTRT  ! User CDF table starting index
      INTEGER :: NTEND  ! User CDF table ending index
      INTEGER :: NUSER  ! User CDF table local index
      INTEGER :: I1     ! Local integer parameter
      INTEGER :: I2     ! Local integer parameter
      INTEGER :: IDIST  ! Local distribution index
      INTEGER :: I      ! Looping control variable
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize error-related variables
!
      IERR = 0
!
! *** Write the first label
!
      WRITE(IVLU,1000) TRIM(VNAME)
 1000 FORMAT(/'Definition for variable "',A,'"')
!
! *** Get the data index for the distribution if needed
!
      IF( VIDX .LE. 0 ) THEN
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
      IF( VIDX .GT. MAXSTO ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid value for distribution index (VIDX)'
        MESSAG(2) = 'Value is larger than the parameter MAXSTO'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      WRITE(IVLU,1010) TRIM(VMESS(VIDX))
 1010 FORMAT(3X,A)
!
! *** Check on the distribution index
!
      IDIST = VTYPE(VIDX)
      IF( IDIST.LT.1 .OR. IDIST.GT.MAXDST ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid distribution index'
        MESSAG(2) = 'Variable: '//VNAME
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      WRITE(IVLU,1020) DLABEL(IDIST)
 1020 FORMAT('   The distribution type is ',A)
!
! *** Get the parameters
!
      PAR1 = VPARMS(VIDX,1)
      PAR2 = VPARMS(VIDX,2)
      PAR3 = VPARMS(VIDX,3)
      PAR4 = VPARMS(VIDX,4)
!
! *** Compute the distribution values
!
      SELECT CASE ( IDIST )
!
        CASE( 1 ) ! Constant distribution
          WRITE(IVLU,1030) PAR1
 1030     FORMAT('   The constant value is ',1P,E11.4)
          RETURN
!
        CASE( 2 ) ! Uniform
          WRITE(IVLU,1040) PAR1, PAR2
 1040     FORMAT('   The lower limit is ',1P,E11.4/ &
                 '   The upper limit is ',E11.4)
!
        CASE( 3 ) ! Discrete Uniform
          I1 = PAR1
          I2 = PAR2
          WRITE(IVLU,1150) I1, I2
 1150     FORMAT('   The lower limit is ',I7/ &
                 '   The upper limit is ',I7)
!
        CASE( 4 ) ! Log Uniform, base 10
          WRITE(IVLU,1040) PAR1, PAR2
!
        CASE( 5 ) ! Log Uniform, base e
          WRITE(IVLU,1040) PAR1, PAR2
!
        CASE( 6 ) ! Triangular
          WRITE(IVLU,1050) PAR1, PAR2, PAR3
 1050     FORMAT('   The minimum is ',1P,E11.4/ &
                 '   The mode is    ',E11.4/ &
                 '   The maximum is ',E11.4)
!
        CASE( 7 ) ! Normal
          WRITE(IVLU,1060) PAR1, PAR2
 1060     FORMAT('   The mean is               ',1P,E11.4/ &
                 '   The standard deviation is ',E11.4)
!
        CASE( 8 ) ! Lognormal, base 10
          WRITE(IVLU,1070) PAR1, PAR2
 1070     FORMAT('   The mean (of the logs) is               ',1P,E11.4/ &
                 '   The standard deviation (of the logs) is ',E11.4)
!
        CASE( 9 ) ! Lognormal, base e
          WRITE(IVLU,1070) PAR1, PAR2
!
        CASE( 10 ) ! User CDF table
          NSTRT = IUSER(VIDX,1)
          NUSER = IUSER(VIDX,2)
! ***     Check on the number of entries in the table
          IF( NUSER .LT. 2 ) THEN
            IERR = 3
            MESSAG(1) = 'Number of values for user CDF table must be >1'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          NTEND = NSTRT + NUSER - 1
! ***     Check on the table indices
          IF( NSTRT.LT.1 .OR. NSTRT.GT.MAXTBL ) THEN
            IERR = 4
            MESSAG(1) = 'NSTRT - Invalid table index for user CDF table'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
! ***     Check on the table indices
          IF( NTEND.LT.1 .OR. NTEND.GT.MAXTBL ) THEN
            IERR = 5
            MESSAG(1) = 'NTEND - Invalid table index for user CDF table'
            MESSAG(2) = 'Variable: '//VNAME
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          WRITE(IVLU,1080) NUSER
 1080     FORMAT('   There are ',I0,' entries in the definition.'/ &
                 '   Probability      X')
          DO I = NSTRT, NTEND
            WRITE(IVLU,1090) UUSER(I), XUSER(I)
 1090       FORMAT(3X,1P,E11.4,1X,E11.4)
          END DO
!
        CASE( 11 ) ! Beta (c,d)
          WRITE(IVLU,1120) PAR1, PAR2, PAR3, PAR4
 1120     FORMAT('   The alpha parameter is ',1P,E11.4/ &
                 '   The beta parameter is  ',E11.4/ &
                 '   The lower limit is     ',E11.4/ &
                 '   The upper limit is     ',E11.4 )
!
        CASE( 12 ) ! Log ratio from Normal
          WRITE(IVLU,1130) PAR1, PAR2, PAR3, PAR4
 1130     FORMAT('   The underlying normal mean is               ',1P,E11.4/ &
                 '   The underlying normal standard deviation is ',E11.4/ &
                 '   The lower limit is     ',E11.4/ &
                 '   The upper limit is     ',E11.4 )
!
        CASE( 13 ) ! Hyperbolic arcsine from Normal
          WRITE(IVLU,1140) PAR1, PAR2
 1140     FORMAT('   The underlying normal mean is               ',1P,E11.4/ &
                 '   The underlying normal standard deviation is ',E11.4)
!
        CASE DEFAULT ! Invalid distribution index
          IERR = 6
          MESSAG(1) = 'Invalid distribution index'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
! *** Echo of truncation statistics
!
      IF( VTRUN(VIDX) .EQ. 1 ) THEN
        WRITE(IVLU,1100) UMIN(VIDX)
 1100   FORMAT('   The lower probability truncation limit is ',1P,E11.4)
        WRITE(IVLU,1110) UMAX(VIDX)
 1110   FORMAT('   The upper probability truncation limit is ',1P,E11.4)
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PRTSTAT( VNAMT, VMEST, XMIN, XV01, XV05, XV10, XV25, XMED, &
        XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IVLU )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints summary statistics for the generated
!!    values for a single variable to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  9 Jan 2002 : Version 1.1
!!    Paul W. Eslinger : 30 Jan 2002 : Increase output precision
!!    Paul W. Eslinger : 31 May 2007 : Revise output file number logic
!!
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: VNAMT ! Variable name
      CHARACTER(LEN=*), INTENT(IN) :: VMEST ! Variable message line
!
      REAL, INTENT(IN) :: XMIN ! Minimum value
      REAL, INTENT(IN) :: XV01 !  5th percentile of the values
      REAL, INTENT(IN) :: XV05 !  5th percentile of the values
      REAL, INTENT(IN) :: XV10 ! 10th percentile of the values
      REAL, INTENT(IN) :: XV25 ! 25th percentile of the values
      REAL, INTENT(IN) :: XMED ! 50th percentile of the values (median)
      REAL, INTENT(IN) :: XV75 ! 75th percentile of the values
      REAL, INTENT(IN) :: XV90 ! 90th percentile of the values
      REAL, INTENT(IN) :: XV95 ! 95th percentile of the values
      REAL, INTENT(IN) :: XV99 ! 95th percentile of the values
      REAL, INTENT(IN) :: XMAX ! Maximum value in X
      REAL, INTENT(IN) :: XAVG ! Average (mean) value
      REAL, INTENT(IN) :: XSTD ! Standard deviation
!
      INTEGER :: IVLU ! Unit number for the output file
!
!---- Executable code ---------------------------------------------------
!
! *** Write the first label
!
      WRITE(IVLU,1000) VNAMT
 1000 FORMAT(/'Summary statistics for variable: ',A)
!
      WRITE(IVLU,1100) TRIM(VMEST)
 1100 FORMAT(3X,A)
!
! *** Write the summary statistics to the report file
!
      WRITE(IVLU,1200)
 1200 FORMAT( &
        '    Minimum ','   1% Level ','   5% Level ','  10% Level ','  25% Level ', &
        '    Median  ','  75% Level ','  90% Level ','  95% Level ','  99% Level ', &
        '    Maximum ','     Mean   ','   St. Dev.'/                &
        ' -----------',' -----------',' -----------',' -----------',' -----------', &
        ' -----------',' -----------',' -----------',' -----------',' -----------', &
        ' -----------',' -----------',' -----------')
      WRITE(IVLU,1210) XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, &
        XV95, XV99, XMAX, XAVG, XSTD
 1210 FORMAT(1P,13(1X,E11.4))
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE FREE_STOCH_DIM( GROUP, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine deallocates the memory for the stochastic variables
!!    The deallocation is performed for three groups of variables, one
!!    group at a time.
!!
!!    The groups of variables are:
!!
!!      TABLE    : Arrays that depend on the number of table entries for
!!                 user defined distributions
!!
!!      WORK     : Work arrays than depend in the number of realizations
!!                 to be generated.
!!
!!      VARIABLE : Arrays that depend on the number of stochastic
!!                 variables that are to be generated
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*) :: GROUP ! Group of variables to deallocate
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'FREE_STOCH_DIM' ! Name of this routine
      INTEGER :: IERA ! Local error number variable
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
      SELECT CASE ( GROUP )
!
        CASE( 'WORK' ) ! >>>>>> Work vectors that depend on the number of values to be generated
!
! ***     Variable: WORK
!
          DEALLOCATE( WORK, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 2
            MESSAG(1) = 'DEALLOCATE function on the vector WORK failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: RWORK
!
          DEALLOCATE( RWORK, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 3
            MESSAG(1) = 'DEALLOCATE function on the vector RWORK failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: IWORK
!
          DEALLOCATE( IWORK, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 4
            MESSAG(1) = 'DEALLOCATE function on the vector IWORK failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!
        CASE( 'TABLE' ) ! >>>>>> Work vectors that depend on the number of table values
!
! ***     Variable: XUSER
!
          DEALLOCATE( XUSER, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 5
            MESSAG(1) = 'DEALLOCATE function on the vector XUSER failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: UUSER
!
          DEALLOCATE( UUSER, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 6
            MESSAG(1) = 'DEALLOCATE function on the vector UUSER failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!
        CASE( 'VARIABLE' ) ! >>>>>> Arrays that depend on the number of stochastic variables
!
! ***     Variable: VLABEL
!
          DEALLOCATE( VLABEL, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 8
            MESSAG(1) = 'DEALLOCATE function on the vector VLABEL failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VMESS
!
          DEALLOCATE( VMESS, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 9
            MESSAG(1) = 'DEALLOCATE function on the vector VMESS failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VTYPE
!
          DEALLOCATE( VTYPE, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 10
            MESSAG(1) = 'DEALLOCATE function on the vector VTYPE failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VTRUN
!
          DEALLOCATE( VTRUN, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 11
            MESSAG(1) = 'DEALLOCATE function on the vector VTRUN failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: UMIN
!
          DEALLOCATE( UMIN, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 12
            MESSAG(1) = 'DEALLOCATE function on the vector UMIN failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: UMAX
!
          DEALLOCATE( UMAX, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 13
            MESSAG(1) = 'DEALLOCATE function on the vector UMAX failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: IUSER
!
          DEALLOCATE( IUSER, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 14
            MESSAG(1) = 'DEALLOCATE function on the vector IUSER failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VPARMS
!
          DEALLOCATE( VPARMS, STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 15
            MESSAG(1) = 'DEALLOCATE function on the vector VPARMS failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE DEFAULT ! Invalid group
          IERR = 16
          MESSAG(1) = 'Invalid group selector'
          MESSAG(2) = 'Group entered was: '//TRIM(GROUP)
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE SET_STOCH_DIM( GROUP, REALLOC, NVALS, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine allocates the memory for the stochastic variables.
!!    The variables are defined in three groups.  Each group can be
!!    either defined for the first time (if unallocated), or can be
!!    reallocated (if allocated).
!!
!!    The groups of variables are:
!!
!!      TABLE    : Arrays that depend on the number of table entries for
!!                 user defined distributions
!!
!!      WORK     : Work arrays than depend in the number of realizations
!!                 to be generated.
!!
!!      VARIABLE : Arrays that depend on the number of stochastic
!!                 variables that are to be generated
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger : 31 May 2007 : Revise output file number logic
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: NVALS ! Length of the vectors for this group
      CHARACTER(LEN=*), INTENT(IN) :: GROUP ! Group of variables to allocate
      LOGICAL, INTENT(IN) :: REALLOC ! Logical flag to reallocate this group
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'SET_STOCH_DIM' ! Name of this routine
      LOGICAL :: CHECK_ALLOC ! Logical allocated check
      INTEGER :: IERA ! Local error number
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
      SELECT CASE ( GROUP )
!
        CASE( 'WORK' ) ! >>>>>> Work vectors that depend on the number of values to be generated
!
          IF( NVALS .LT. 1 ) THEN
            IERR = 1
            MESSAG(1) = 'Number of realizations less than 1'
            MESSAG(2) = 'Cannot allocate memory for stochastic variables'
            MESSAG(3) = 'Working on GROUP = "WORK"'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
! ***     Save dimension for later error checking
!
          MAXGEN = NVALS
!
! ***     Variable: WORK
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( WORK )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( WORK, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 2
                MESSAG(1) = 'DEALLOCATE function on the vector WORK failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( WORK(NVALS), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 3
            MESSAG(1) = 'ALLOCATE function on the vector WORK failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: RWORK
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( RWORK )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( RWORK, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 4
                MESSAG(1) = 'DEALLOCATE function on the vector RWORK failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( RWORK(NVALS), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 5
            MESSAG(1) = 'ALLOCATE function on the vector RWORK failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: IWORK
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( IWORK )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( IWORK, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 6
                MESSAG(1) = 'DEALLOCATE function on the vector IWORK failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( IWORK(NVALS), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 7
            MESSAG(1) = 'ALLOCATE function on the vector IWORK failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 'TABLE' ) ! >>>>>> Work vectors that depend on the number of table values
!
          IF( NVALS .LT. 1 ) THEN
            IERR = 8
            MESSAG(1) = 'Number of table values is less than 1'
            MESSAG(2) = 'Cannot allocate memory for stochastic variables'
            MESSAG(3) = 'Working on GROUP = "TABLE"'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
! ***     Save dimension for later error checking
!
          MAXTBL = NVALS
!
! ***     Variable: XUSER
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( XUSER )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( XUSER, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 9
                MESSAG(1) = 'DEALLOCATE function on the vector XUSER failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( XUSER(NVALS), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 10
            MESSAG(1) = 'ALLOCATE function on the vector XUSER failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: UUSER
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( UUSER )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( UUSER, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 9
                MESSAG(1) = 'DEALLOCATE function on the vector UUSER failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( UUSER(NVALS), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 10
            MESSAG(1) = 'ALLOCATE function on the vector XUSER failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 'VARIABLE' ) ! >>>>>> Arrays that depend on the number of stochastic variables
!
          IF( NVALS .LT. 1 ) THEN
            IERR = 11
            MESSAG(1) = 'Number of requested stochastic variables is less than 1'
            MESSAG(2) = 'Cannot allocate memory for stochastic variables'
            MESSAG(3) = 'Working on GROUP = "WORK"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Save dimension for later error checking
!
          MAXSTO = NVALS
!
! ***     Variable: VLABEL
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( VLABEL )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( VLABEL, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 12
                MESSAG(1) = 'DEALLOCATE function on the vector VLABEL failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( VLABEL(MAXSTO), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 13
            MESSAG(1) = 'ALLOCATE function on the vector VLABEL failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VMESS
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( VMESS )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( VMESS, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 14
                MESSAG(1) = 'DEALLOCATE function on the vector VMESS failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( VMESS(MAXSTO), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 15
            MESSAG(1) = 'ALLOCATE function on the vector VMESS failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VTYPE
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( VTYPE )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( VTYPE, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 16
                MESSAG(1) = 'DEALLOCATE function on the vector VTYPE failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( VTYPE(MAXSTO), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 17
            MESSAG(1) = 'ALLOCATE function on the vector VTYPE failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VTRUN
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( VTRUN )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( VTRUN, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 18
                MESSAG(1) = 'DEALLOCATE function on the vector VTRUN failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( VTRUN(MAXSTO), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 19
            MESSAG(1) = 'ALLOCATE function on the vector VTRUN failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: UMIN
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( UMIN )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( UMIN, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 20
                MESSAG(1) = 'DEALLOCATE function on the vector UMIN failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( UMIN(MAXSTO), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 21
            MESSAG(1) = 'ALLOCATE function on the vector UMIN failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: UMAX
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( UMAX )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( UMAX, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 22
                MESSAG(1) = 'DEALLOCATE function on the vector UMAX failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!!
          ALLOCATE( UMAX(MAXSTO), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 23
            MESSAG(1) = 'ALLOCATE function on the vector UMAX failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: IUSER
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( IUSER )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( IUSER, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 24
                MESSAG(1) = 'DEALLOCATE function on the vector IUSER failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( IUSER(MAXSTO,2), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 25
            MESSAG(1) = 'ALLOCATE function on the vector IUSER failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
! ***     Variable: VPARMS
!
          IF( REALLOC ) THEN
            CHECK_ALLOC = ALLOCATED( VPARMS )
            IF( CHECK_ALLOC ) THEN
              DEALLOCATE( VPARMS, STAT=IERA )
              IF( IERA .NE. 0 ) THEN
                IERR = 26
                MESSAG(1) = 'DEALLOCATE function on the vector VPARMS failed'
                MESSAG(2) = 'Deallocating because reallocation requested'
                MESSAG(3) = 'System error code returned is '
                WRITE(MESSAG(3)(31:),*) IERA
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          ALLOCATE( VPARMS(MAXSTO,4), STAT=IERA )
          IF( IERA .NE. 0 ) THEN
            IERR = 27
            MESSAG(1) = 'ALLOCATE function on the vector VPARMS failed'
            MESSAG(2) = 'System error code returned is '
            WRITE(MESSAG(2)(31:),*) IERA
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE DEFAULT ! Invalid group
          IERR = 28
          MESSAG(1) = 'Invalid group selector'
          MESSAG(2) = 'Group entered was: '//TRIM(GROUP)
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
      RETURN
      END SUBROUTINE

