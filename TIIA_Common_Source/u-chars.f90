!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
      LOGICAL FUNCTION STRCOMP( STRING1, STRING2, NCHAR )
!!*****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine compares NCHAR characters from two character strings
!!    and checks for a match that is not case sensitive.
!!
!!  Inputs:
!!
!!    Variable  Description
!!    --------  ---------------------------------------
!!    STRING1   First character string
!!    STRING2   Second character string
!!    NCHAR     Number of characters to compare
!!
!!  Auxiliary Routines:
!!
!!    UPCASE - Subroutine that converts one or more contiguous characters
!!             to upper case.
!!
!!  History:
!!
!!    Paul W. Eslinger : 22 Oct 2003 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!*****************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: STRING1
      CHARACTER(LEN=*), INTENT(IN) :: STRING2
      INTEGER, INTENT(IN) :: NCHAR
!
! *** Local variables
      CHARACTER(LEN=1) :: S1, S2 ! Temporary 1-character strings
      INTEGER :: ICH ! Looping index
!
!---- Executable code --------------------------------------------
!
      STRCOMP = .FALSE.
!
      DO ICH = 1, NCHAR
        S1 = STRING1(ICH:ICH)
        CALL UPCASE( S1 )
        S2 = STRING2(ICH:ICH)
        CALL UPCASE( S2 )
        IF( S1 .NE. S2 ) RETURN
      END DO
!
      STRCOMP = .TRUE.
!
      RETURN
      END FUNCTION
!
      SUBROUTINE UPCASE( CVAR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine converts all of the lower case (a-z) characters
!!    in CVAR to upper case (A-Z) characters.  All other characters are
!!    unmodified.  The number of characters in CVAR to be examined
!!    are found by triming all blanks from the implied length of the
!!    character string.
!!
!!  History:
!!
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*) :: CVAR ! Variable to convert to uppercase
!
! *** Local variables
      INTEGER :: CLEN ! Number of characters in the input character variable
      INTEGER :: I  ! Looping variable
      INTEGER :: IC ! Ascii sequence number for a character
!
!---- Executable code ---------------------------------------------------
!
! *** Number of characters in the string
!
      CLEN = LEN_TRIM( CVAR )
!
! *** Return without action for a null string
!
      IF( CLEN .EQ. 0 ) RETURN
!
! *** Convert all lowercase alphabetic characters to uppercase
!
      DO I = 1, CLEN
        IC = ICHAR( CVAR(I:I) )
        IF( IC.GE.97 .AND. IC.LE.122 ) CVAR(I:I) = CHAR(IC-32)
      END DO
!
      RETURN
      END SUBROUTINE

