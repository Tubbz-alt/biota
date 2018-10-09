!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
      SUBROUTINE SORT( X, N, KFLAG, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine will sort the array X in increasing
!!    or decreasing order.
!!
!!  Reference:
!!
!!    Singleton, R. C.
!!    Algorithm 347
!!    An Efficient Algorithm for Sorting with Minimal Storage
!!    C.A.C.M., 12(3), 1969, Pages 185-187.
!!
!!  Variable Descriptions:
!!
!!    X     - Array of values to be sorted
!!    N     - Number of values in array X to be sorted
!!    KFLAG - Control parameter
!!             =  1 Means sort X in increasing order (ignoring Y)
!!             = -1 Means sort X in decreasing order (ignoring Y)
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
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
      REAL, DIMENSION(*) :: X
      INTEGER, INTENT (IN) :: N, KFLAG
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'SORT' ! Name of this routine
      INTEGER, DIMENSION(21) :: IL, IU
      INTEGER :: NN, I, M, J, K, IJ, L
      REAL :: R, T, TT
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check for proper dimensioning
!
      IF( N .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid number of data - negative entry.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Check for valid sort order indicator
!
      IF( .NOT.(KFLAG.EQ.1 .OR. KFLAG.EQ.-1) ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid sort indicator.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      NN = N
!
! *** CHECK FOR SORT IN INCREASING ORDER
!
      IF (KFLAG .EQ. 1 ) GO TO 100
!
! *** ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
!
      DO I = 1, NN
        X(I) = -X(I)
      END DO
!
! *** SORT THE VECTOR
!
  100 CONTINUE
!
      M=1
      I=1
      J=NN
      R=.375
  110 IF (I .EQ. J) GO TO 155
  115 IF (R .GT. .5898437) GO TO 120
      R=R+3.90625E-2
      GO TO 125
  120 R=R-.21875
  125 K=I
!
!  *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
!
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 130
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
  130 L=J
!
! *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
!
      IF (X(J) .GE. T) GO TO 140
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 140
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
      GO TO 140
  135 TT=X(L)
      X(L)=X(K)
      X(K)=TT
!
! *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
! *** SMALLER THAN T
!
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140
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
      IF (L-I .LE. J-K) GO TO 150
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 160
  150 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 160
!
! *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
!
  155 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  160 IF (J-I .GE. 1) GO TO 125
      IF (I .EQ. 1) GO TO 110
      I=I-1
  165 I=I+1
      IF (I .EQ. J) GO TO 155
      T=X(I+1)
      IF (X(I) .LE. T) GO TO 165
      K=I
  170 X(K+1)=X(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 170
      X(K+1)=T
      GO TO 165
!
! *** CLEAN UP FOR SORT IN DECREASING ORDER
!
  300 IF( KFLAG .EQ. 1 ) RETURN
!
      DO I = 1, NN
        X(I) = -X(I)
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE SSORTI( X, Y, N, KFLAG, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    SSORTI sorts array X and optionally makes the same interchanges
!!    in array Y.  The array X may be sorted in increasing order or
!!    decreasing order.  A modified QUICKSORT algorithm is used.
!!    The vector X is real and the vector Y is integer.
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
!!    X     : real array of values to be sorted
!!    Y     : integer array to be (optionally) carried along
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
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
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
      INTEGER, INTENT (IN) :: N, KFLAG
      REAL, DIMENSION(*) :: X
      INTEGER, DIMENSION(*) :: Y
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      INTEGER :: TY, TTY
      INTEGER, DIMENSION(21) :: IL, IU
      CHARACTER(LEN=6) :: CALLER = 'SSORTI' ! Name of this routine
      INTEGER :: NN, I, M, J, K, KK, IJ, L
      REAL :: R, T, TT
!
!---- Executable code ---------------------------------------------------
!
      NN = N
      KK = IABS(KFLAG)
!
! *** Check for proper dimensioning
!
      IF( N .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid number of data - negative entry.'
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

