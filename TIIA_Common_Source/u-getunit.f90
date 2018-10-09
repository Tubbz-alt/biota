!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
      INTEGER FUNCTION GET_UNIT_NUMBER( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This functions finds the first unused unit number in the range
!!    7 to 1000 to use for a file operation.
!!
!!    A value of -1 is returned if the search was unsuccessful.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Apr 2003 : Original code
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!    Paul W. Eslinger : 28 Jun 2012 : Increase the uper limit on files
!!
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      LOGICAL :: CONNECTED ! Logical flag whether unit number was connected
      INTEGER :: IFIL      ! Looping index for file numbers
!
!---- Executable code --------------------------------------------
!
      DO IFIL = 7, 10000
        INQUIRE( UNIT=IFIL, OPENED=CONNECTED )
        IF( .NOT. CONNECTED ) THEN
          GET_UNIT_NUMBER = IFIL
          RETURN
        END IF
      END DO
!
! *** Didn't find a valid unit number
!
      GET_UNIT_NUMBER = -1
!
      RETURN
      END FUNCTION GET_UNIT_NUMBER

