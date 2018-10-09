!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
MODULE QA_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains quality assurance information
!
!  History:
!    Paul W. Eslinger : 23 Jan 2000 : Version 1.0
!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 29 Jun 2012 : Make consistent across TIIA codes
!
      LOGICAL, PARAMETER :: QA_Flag = .TRUE. ! Logical flag
!       .TRUE.  = Code was developed according to software program requirements
!       .FALSE. = Code is experimental and does not meet all software program requirements
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE QA_Mod

      SUBROUTINE QA_Disclaimer( Irpt, Center )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a QA disclaimer to a report file.
!!
!!  History:
!!    Paul W. Eslinger : 29 Jun 2012 : Original source
!!
!!**************************************************************************************************
!
! *** Global variables
      USE QA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** call list variables
      INTEGER :: Irpt ! Output unit number (file must be open)
      LOGICAL, INTENT(IN) :: Center ! Center output on an 80 column page (TRUE)
!
!---- First executable code --------------------------------------------
!
! *** Code status disclaimer
!
      IF( QA_Flag ) THEN ! QA status is good
        IF( Center ) THEN
          WRITE(IRPT,1010) 
 1010     FORMAT(/15X,' The software used to generate this output has been'/ &
                  15X,'developed under Battelle software control procedures.')
        ELSE
          WRITE(IRPT,1020) 
 1020     FORMAT(/' The software used to generate this output has been'/ &
                  'developed under Battelle software control procedures.')
        END IF
      ELSE ! QA Status is bad
        IF( Center ) THEN
          WRITE(IRPT,1030)
 1030     FORMAT(/11X,'The software used to generate this output is experimental'/ &
                  11X,'   and has not been formally tested or peer reviewed.')
        ELSE
          WRITE(IRPT,1040)
 1040     FORMAT(/'The software used to generate this output is experimental'/ &
                  'and has not been formally tested or peer reviewed.')
        END IF
      END IF
!
      RETURN
      END SUBROUTINE QA_Disclaimer

      SUBROUTINE QA_CopyrightFull( Irpt, Center )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a QA copyright to a report file.
!!
!!  History:
!!    Paul W. Eslinger :  9 Jul 2012 : Original source
!!    Paul W. Eslinger : 23 Aug 2012 : Updated copyright
!!
!!**************************************************************************************************
!
! *** Global variables
      USE QA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** call list variables
      INTEGER :: Irpt ! Output unit number (file must be open)
      LOGICAL, INTENT(IN) :: Center ! Center output on an 80 column page (TRUE)
!
!---- First executable code --------------------------------------------
!
      IF( Center ) THEN
        WRITE(IRPT,1010) 
 1010   FORMAT(17X,'------------------------------------------------'/&
               17X,'Toolkit for Integrated Impact Assessments (TIIA)'/&
               17X,'Copyright (c) Battelle Memorial Institute, 2018.'/&
               17X,'              All Rights Reserved.              '/&
               17X,'------------------------------------------------')
      ELSE
        WRITE(IRPT,1020) 
 1020   FORMAT( 1X,'------------------------------------------------'/&
                1X,'Toolkit for Integrated Impact Assessments (TIIA)'/&
                1X,'Copyright (c) Battelle Memorial Institute, 2018.'/&
                1X,'              All Rights Reserved.              '/&
                1X,'------------------------------------------------')
      END IF
!
      RETURN
      END SUBROUTINE QA_CopyrightFull

      SUBROUTINE QA_CopyRightSingle( Irpt, Center )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a one-line QA copyright to a report file.
!!
!!  History:
!!    Paul W. Eslinger : 29 Jun 2012 : Original source
!!    Paul W. Eslinger : 23 Aug 2012 : Updated copyright
!!
!!**************************************************************************************************
!
! *** Global variables
      USE QA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** call list variables
      INTEGER :: Irpt ! Output unit number (file must be open)
      LOGICAL, INTENT(IN) :: Center ! Center output on an 80 column page (TRUE)
!
!---- First executable code --------------------------------------------
!
      IF( Center ) THEN
        WRITE(IRPT,1010) 
 1010   FORMAT(17X,'Toolkit for Integrated Impact Assessments (TIIA)'/&
               17X,'Copyright (c) Battelle Memorial Institute, 2018.')
      ELSE
        WRITE(IRPT,1020) 
 1020   FORMAT(1X,'Toolkit for Integrated Impact Assessments (TIIA)'/&
               1X,'Copyright (c) Battelle Memorial Institute, 2018.')
      END IF
!
      RETURN
      END SUBROUTINE QA_CopyRightSingle

      SUBROUTINE QA_Reference( Irpt )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a QA copyright to a report file.
!!
!!  History:
!!    Paul W. Eslinger :  9 Jul 2012 : Original source
!!
!!**************************************************************************************************
!
! *** Global variables
      USE QA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** call list variables
      INTEGER :: Irpt ! Output unit number (file must be open)
!
!---- First executable code --------------------------------------------
!
      WRITE(Irpt,1010) 
 1010 FORMAT(/ &
       'Eslinger, P.W. and T.B. Miley.   PNWD-4357, Rev. 1.  July 2012.' /&
       'User Instructions for the Computer Codes of the Toolkit for Integrated Impact' /&
       'Assessments (TIIA), Version 1. Battelle Memorial Institute, Columbus, Ohio 43201')
!
      RETURN
      END SUBROUTINE QA_Reference

