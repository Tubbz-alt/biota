!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
MODULE Errors_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to writing user defined error messages to an ASCII
!    file opened on unit IRPT_ERR.  The messages are written using the routine PRTERR.
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger : 10 Oct 2002 : Extend message length
!    Paul W. Eslinger : 30 May 2007 : Update comments
!
!     Unit number for writing the error messages
      INTEGER :: IRPT_ERR
!
!     Maximum number of lines in an error message
      INTEGER, PARAMETER :: MAXMES = 5
!
!     Vector of message lines
      CHARACTER(LEN=256), DIMENSION(MAXMES) :: MESSAG
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Errors_Mod
!
      SUBROUTINE PRTERR( IERR, CALLER, MLINES )
!!************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints a user defined error message to an ASCII
!!    file opened for output on the unit IRPT_ERR.
!!
!!  Call List Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ----------------------------------------
!!    IERR     (Integer)    Error number from the calling routine
!!                          If 0, output message rather than error
!!    CALLER   (Character)  Name of the calling routine
!!    MLINES   (Integer)    Number of lines in the error message
!!
!!  Module Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ----------------------------------------
!!    IRPT_ERR (Integer)    File unit for output (opened previously)
!!    MESSAG   (Character)  Message vector
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Sep 2002 : Update format statements
!!    Paul W. Eslinger :  9 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger : 30 May 2007 : Update comments
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
      INTEGER, INTENT(IN) :: IERR
      CHARACTER(LEN=*), INTENT(IN) :: CALLER
      INTEGER, INTENT(IN) :: MLINES
!
! *** Local variables
      INTEGER :: I ! Local looping control variable
!
!---- Executable code ---------------------------------------------------
!
! *** Write out the error number and the calling routine
      IF( IERR .EQ. 0 ) THEN
        WRITE(IRPT_ERR,1000) TRIM(CALLER)
 1000   FORMAT(/'Message originating in routine ',A)
      ELSE
        WRITE(IRPT_ERR,1010) IERR, TRIM(CALLER)
 1010   FORMAT(/'Error number ',I0,' encountered in routine ',A)
      END IF
!
! *** Write out the first line of the error message
      IF( MLINES .GT. 0 ) THEN
        WRITE(IRPT_ERR,1020) TRIM( MESSAG(1) )
 1020   FORMAT('Message: ',A)
      END IF
!
! *** Write out any trailing lines for the message
      IF( MLINES .GT. 1 ) THEN
        DO I = 2, MIN(MLINES,MAXMES)
          WRITE(IRPT_ERR,1030) TRIM( MESSAG(I) )
 1030   FORMAT(9X,A)
        END DO
      END IF
!
      RETURN
      END SUBROUTINE PRTERR

