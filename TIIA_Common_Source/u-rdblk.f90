!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2012.
!----------------------------------------------------
!
MODULE Rdblk_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information related to the RDBLK keyword
!    decoding routines
!
!  History:
!
!    Dave W. Langford : Original Programmer
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger : 11 Feb 2001 : Tie NVALS and NKEYS
!    Paul W. Eslinger : 13 Mar 2003 : Tie NVALS and MAXQQQ, extend lengths
!    Paul W. Eslinger :  5 Jan 2006 : Extend keyword line length
!    Paul W. Eslinger : 15 May 2006 : Allow more values per keyword
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Parameters:
!
!     LENCRD : Maximum number of characters per input card
      INTEGER, PARAMETER :: LENCRD = 2048
!
!     LENQQQ : Maximum number of characters per quoted string
      INTEGER, PARAMETER :: LENQQQ = 200
!
!     NKEYS  : Maximum number of keyword modifiers per block
      INTEGER, PARAMETER :: NKEYS  = 5000
!
!     NVALS  : Maximum number of data values per block
      INTEGER, PARAMETER :: NVALS  = NKEYS
!
!     MAXQQQ : Maximum number of strings enclosed in quotes per block
      INTEGER, PARAMETER :: MAXQQQ = NKEYS
!
! Character Information:
!
!     KNAME   : Input line keyword name
      CHARACTER(LEN=8) :: KNAME
!
!     CVALUE : List of character modifiers in input block
      CHARACTER(LEN=8), DIMENSION(NKEYS) :: CVALUE
!
!     INFO : Input line with the keyword stripped off
      CHARACTER(LEN=1), DIMENSION(LENCRD) :: INFO
!
!     QUOTE  : Input items which were enclosed in double quotes
      CHARACTER(LEN=LENQQQ), DIMENSION(MAXQQQ) :: QUOTE
!
! Numeric information:
!
!     VALUE : Numeric data read from input data block
      REAL, DIMENSION(NVALS) :: VALUE
!
! Block size/location tracking information
!
!     ILINE  : Line number of the next line to read from the input file
      INTEGER :: ILINE
!
!     LSTLIN : Line number of last keyword.  Identifies current command.
      INTEGER :: LSTLIN
!
!     NCVALU : Number of character items read from the last block.
      INTEGER :: NCVALU
!
!     NDXCHR : Command line indices for input string entities.
      INTEGER, DIMENSION(NKEYS) :: NDXCHR
!
!     NDXQQQ : Command line indices for strings enclosed in double quotes
      INTEGER, DIMENSION(MAXQQQ) :: NDXQQQ
!
!     NDXVAL : Command line indices for input value entities.
      INTEGER, DIMENSION(NVALS) :: NDXVAL
!
!     NEXIST : Indicates the index of the keyword or modifier
!              found by the logical function EXIST.
      INTEGER :: NEXIST
!
!     NQUOTE : Number of character strings found in double quotes.
      INTEGER :: NQUOTE
!
!     NTITY  : Number of string and value entities read from last block.
      INTEGER :: NTITY
!
!     NVALUE : Number of numeric items read from the last block.
      INTEGER :: NVALUE
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Rdblk_Mod
!
      SUBROUTINE RDBLK( IKEY, IPERR, TITLE, IRERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!   This module reads a block of data from a text input data file
!!   (logical unit IKEY).  The block includes a keyword card and all
!!   subsequent continuation cards, to the next keyword card.
!!
!!    Output from this set of subroutines includes:
!!
!!      A) The keyword from the keyword line (KNAME, Char*8),
!!      B) The data values from that card and subsequent continuation
!!         cards (VALUE,real),
!!      C) Secondary keywords on the input card block (CVALUE, Char*8).
!!      D) Strings enclosed in double quotes.  (QUOTE, Char*64)
!!
!!  Call Arguments:
!!
!!   IKEY  : Logical unit number for text data input.  (input)
!!   IPERR : Logical unit number for error messages.  (input)
!!   IRERR : Error message indicator (0=no error).  (output)
!!   TITLE : Keyword line text, with the keyword deleted.  This
!!           returns the contents of the keyword line after the
!!           first separator. (output)
!!
!!  Usage Considerations:
!!
!!    1. All output keywords are stored as upper case characters.  All
!!    numeric values are stored as real numbers (not integer).  Quote
!!    strings are remain literally as given in the input deck.
!!
!!    2. Strings which are enclosed in double quotes are treated
!!    separately, to allow literal text (or strings of multiple words)
!!    to be read into the system.  These are referred to as 'quoted
!!    strings', and were originally intended to be used for file names.
!!    The input string can be of any length, but is truncated at
!!    (LENQQQ) characters.
!!
!!    3. The routines RDBLK and UPCASE require alphabetic characters to
!!    be 'represented' contiguously in memory (a thru z, and A thru Z).
!!    In routine SEPCHR, a TAB character is a valid separator only if it
!!    converts to a character code of less than 10 decimal.  ASCII code
!!    is generally expected, but not necessary if these conditions
!!    prevail.
!!
!!    4. Logical function CEXIST is not called from these routines, but
!!    is provided as a utility routine for the user.
!!
!!    5.  On input to this routine, it is assumed the next line of the
!!    input data file is already present in array INFO, except on the
!!    first call (ILINE=0).  ILINE must be initialized to zero by the
!!    main routine (or block data).
!!
!!    6. Array INFO must not be modified between calls to RDBLK.  It
!!    saves the previous keyword line from the input file.
!!
!!    7. The subroutine PRTERR is not used with these routines because
!!    there is no guarantee that the report file has been opened when
!!    an error is encountered by the RDBLK routines.
!!
!! Variables:
!!
!!   CVALUE = Array of secondary keywords from the input data block.
!!            Only the first eight characters are saved.  (CHAR*8)
!!   QUOTE  = Character string entity in the input block which was
!!            enclosed in double quotes.  (CHAR*64)
!!   IKEY   = Logical unit number for data read.
!!   ILINE  = Current line number of the input data file.
!!   INFO   = Saves the contents of the next line of the input file.
!!            (Dimension LENCRD, Char*1)
!!   LSTLIN = Line number of last keyword.  Identifies current command.
!!   KNAME   = Keyword from the current data block.  (CHAR*8)
!!   NCVALU = Number of secondary keywords from data block.
!!   NDXCHR = Indices in the data block of keyword entities.
!!   NDXQQQ = Indices in the data block of quoted entities.
!!   NDXVAL = Indices in the data block of numeric entities.
!!   NVALUE = Number of data values from the data block.
!!   NTITY  = Number of keyword and value entities read from a data
!!            block.
!!   TITLE  = Returns the entire contents of the keyword line,
!!            following the first separator.
!!   VALUE  = Array of data values from the input data block.
!!            These are stored as real values (not integers).
!!
!!  Auxiliary Routines:
!!
!!    RDBLK   Controlling routine for decoding keywords
!!    COMCHR  Identifies comment characters.
!!    DCDSEP  Parses the input line into numeric and character data.
!!    FINSEP  Locates separators in the input line.
!!    SAVTTL  Moves the entire contents of a keyword line, following
!!            the first separator, into an output character array.
!!    SEPCHR  Determines if a character is a legal separator.
!!    UPCASE  Converts character strings to upper case.  Depends on
!!            contiguous 'ordering' of alphabetic characters.
!!    XFKEY   Transfers a keyword from a character string to a char*8
!!            variable.  Blank fills for strings less than length 8.
!!    CEXIST  Logical function to determine if an 8 character string
!!            is present among the secondary keywords (CVALUE).
!!    NXTVAL  Returns the index and value in the VALUE array of the
!!            first numeric entry after the last keyword identified
!!            by the most recent call to CEXIST.
!!    NXTQOT  Returns the index and text in the QUOTE array of the
!!            first quote string after the last keyword identified
!!            by the most recent call to CEXIST.
!!
!!  History:
!!
!!    Dave Langford    : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 16 Mar 2000 : Convert to Fortran 90
!!                                     Add IRERR error return
!!    Paul W. Eslinger : 13 Mar 2003 : Increase card length logic
!!    Paul W. Eslinger :  5 Jan 2006 : Extend keyword line length
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: COMCHR, SEPCHR
!
! *** Call list variables
      INTEGER :: IKEY
      INTEGER :: IPERR
      INTEGER :: IRERR
      CHARACTER*(LENCRD) :: TITLE
!
! *** Local variables
      INTEGER, DIMENSION(LENCRD) :: LOC
      INTEGER :: I    ! Local looping control variable
      INTEGER :: IERR ! Error number from read statement
      INTEGER :: NUM  ! Local number for separator character
!
!---- Executable code ---------------------------------------------------
!
      IRERR = 0
!
!---- Read first line(s) of data file, to a keyword card
!
      IF( ILINE .LE. 0 ) THEN
  100   CONTINUE
        ILINE = ILINE+1
        READ(IKEY,500,END=470,ERR=480,IOSTAT=IERR) (INFO(I),I=1,LENCRD)
        IF( ((INFO(1).LT.'A').OR.(INFO(1).GT.'Z')).AND. &
          ((INFO(1).LT.'a').OR.(INFO(1).GT.'z')) ) GO TO 100
      END IF
      LSTLIN = ILINE
!
      NTITY  = 0
      NCVALU = 0
      NQUOTE = 0
      NVALUE = 0
      NEXIST = 0
!
!---- Move keyword into array KNAME, with blank filling.
!
!      CALL XFKEY( INFO(1), KNAME )
      CALL XFKEY( INFO, KNAME )
!
!---- Move line data into array TITLE
!
      CALL SAVTTL( TITLE )
!
!---- Return if END card encountered
!
      IF( KNAME.EQ.'END     ' ) RETURN
!
!---- Locate all separators in this line.  This is
!     the top of loop for reading continuation cards.
!
  110 CONTINUE
      CALL FINSEP( NUM, LOC )
!
!---- Move character data into array CVALUE, and
!     numeric data into array VALUE.
!
      IF( NUM .GT. 1 ) CALL DCDSEP( NUM, LOC, IPERR, IRERR )
      IF( IRERR .NE. 0 ) RETURN
!
!---- Read next line and reformat
!
  120 CONTINUE
      ILINE = ILINE+1
      READ(IKEY,500,END=130,ERR=480,IOSTAT=IERR) (INFO(I),I=1,LENCRD)
!
!---- Return if next keyword card was found
!
      IF( ((INFO(1).GE.'A').AND.(INFO(1).LE.'Z')).OR. &
          ((INFO(1).GE.'a').AND.(INFO(1).LE.'z')) ) RETURN
!
!---- If comment card, read the next line.
!
      IF( COMCHR(1,INFO(1)) ) GO TO 120
!
!---- Check for a continuation card.  Loop up to decode, if found.
!
      IF( .NOT.SEPCHR(INFO(1)) ) THEN
         WRITE(IPERR,510)
         GO TO 490
      END IF
      GO TO 110
!
!---- Artificial END card generated
!
  130 CONTINUE
      WRITE(IPERR,520)
      INFO(1)='E'
      INFO(2)='N'
      INFO(3)='D'
      INFO(4)=' '
      RETURN
!
!---- Error handing
!
  470 CONTINUE
      WRITE(IPERR,970)
      GO TO 490
  480 CONTINUE
      WRITE(IPERR,980)
  490 CONTINUE
      WRITE(IPERR,990) ILINE, IERR
      IRERR = 1
      RETURN
!
  500 FORMAT(2048A1)
  510 FORMAT(/' ** Error! A continuation card is expected, but the current'/ &
              '    line does not begin with a valid separator character.')
  520 FORMAT(/' ** Warning! End-of-File encountered on the input file.'/ &
              '    An artificial end card was generated.')
  970 FORMAT(/' ** Error! End-of-File encountered in the input file'/ &
              '    prior to reading a keyword card.')
  980 FORMAT(/' ** Error reading data from the input file.')
  990 FORMAT(4X,'Reading line number ',I5/ &
             4X,'I/O Error Status is ',I5/ &
             4X,'Error encountered in routine RDBLK')
!
      END SUBROUTINE
!
      LOGICAL FUNCTION CEXIST( KEYWRD )
!!**********************************************************************
!!
!!  Purpose:
!!
!!   This function determines if an eight letter keyword is one of
!!   the secondary keywords of the current data block.
!!
!!  Call Argument:
!!
!!    KEYWRD : The character string for which to search the list of
!!             secondary keywords.  (Input)
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*) :: KEYWRD
!
! *** Local variables
      INTEGER :: N ! Local looping control variable
!
!---- Executable code ---------------------------------------------------
!
      DO N = 1, NCVALU
        IF( CVALUE(N) .EQ. KEYWRD ) THEN
          NEXIST = N
          CEXIST = .TRUE.
          RETURN
        END IF
      END DO
!
      NEXIST = 0
      CEXIST = .FALSE.
!
      RETURN
      END FUNCTION
!
      LOGICAL FUNCTION COMCHR( IPLACE, CH )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine determines if the character CH is a comment
!!    identifying character.  Comment lines are ignored, as are
!!    characters after a comment identifier on one line.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!! Call Arguments:
!!
!!   CH     : Character to examine.  (input)
!!   IPLACE : Indicates the position of character CH in the input
!!            line.  Note: the '/' character indicates a comment
!!            only if it is the first character in a line. (input)
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IPLACE
      CHARACTER(LEN=1) :: CH
!
!---- Executable code ---------------------------------------------------
!
      IF( (CH.EQ.'!').OR.(CH.EQ.'$') ) THEN
        COMCHR = .TRUE.
      ELSE
        IF( CH.EQ.'/' .AND. IPLACE.EQ.1 ) THEN
          COMCHR = .TRUE.
        ELSE
          COMCHR = .FALSE.
        END IF
      END IF
!
      RETURN
      END FUNCTION
!
      SUBROUTINE DCDSEP( NUM, LOC, IPERR, IRERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!   This subroutine examines character data in the input line (INFO),
!!   decodes character data into the array CVALUE, and decodes numeric
!!   data into array VALUE.
!!
!!  Call Arguments:
!!
!!    LOC   : Locates the field separators from the input file. (input)
!!    NUM   : Indicates the number of separators present. (input)
!!    IPERR : Logical unit number for error messages. (input)
!!    IRERR : Error message indicator (0=no error).  (output)
!!
!!  Comments:
!!
!!    1. Real numbers may be written in exponential format, but only
!!    an 'E' or an 'e' are recognized exponent indicators.  The double
!!    precision 'D' exponent indicator is not valid.
!!
!!    2.  A '*' character interior to a line is taken to indicate a
!!    repetition of the following value or character field.  It must
!!    be preceded by an unsigned integer value.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger :  5 May 2000 : Version 1.0 - Add IRERR logic
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: NUM
      INTEGER, DIMENSION(LENCRD) :: LOC
      INTEGER :: IPERR
      INTEGER :: IRERR
!
! *** Local variables
      CHARACTER(LEN=1) :: CH
      CHARACTER(LEN=1), DIMENSION(LENCRD) :: DUMY
!
      LOGICAL :: DECSET
      LOGICAL :: EXPSET
      LOGICAL :: MINUS
!
      INTEGER :: I ! Looping control variable
      INTEGER :: J ! Looping index variable
      INTEGER :: K ! Looping index variable
!
      INTEGER :: IEND   ! Local counting variable
      INTEGER :: IS     ! Local counting variable
      INTEGER :: ISETNO ! Local counting variable
      INTEGER :: ISTART ! Local counting variable
      INTEGER :: MULT   ! Local counting variable
      INTEGER :: NCHARS ! Local counting variable
      INTEGER :: NDIGIT ! Local counting variable
      INTEGER :: NUMB   ! Local counting variable
!
      REAL :: RDATA ! Local real variable
      REAL :: D     ! Local real variable
!
!---- Executable code ---------------------------------------------------
!
      IRERR = 0
!
!---- Loop on the number of separators, minus one.
!     Initialize the character count between adjacent separators.
!
      DO 220 I = 1,(NUM-1)
        J = I+1
        ISTART = LOC(I)+1
        NCHARS = LOC(J)-ISTART
        IF( NCHARS.LT.1 ) GO TO 220
!
!----   Look for string in double quotes.  An inefficient transfer loop
!       is used, since this algorithm is not expected to be used much.
!
        IF( INFO(LOC(I)).EQ.'"' ) THEN
          IF( NQUOTE.GE.MAXQQQ ) GO TO 460
          NQUOTE = NQUOTE+1
          ISTART = ISTART-1
          DO J = 1,LENQQQ
            IF( J.LE.NCHARS ) THEN
              K = ISTART+J
              QUOTE(NQUOTE)(J:J) = INFO(K)
            ELSE
              QUOTE(NQUOTE)(J:J) = ' '
            END IF
          END DO
          NTITY = NTITY+1
          NDXQQQ(NQUOTE) = NTITY
          GO TO 220
        END IF
!
!----   Initializations for decoding loop
!
        DECSET = .FALSE.
        EXPSET = .FALSE.
        IEND   = LOC(J)-1
        ISETNO = 0
        MULT   =-1
        NDIGIT = 0
        RDATA  = 0.0
!
        IS   = LOC(I)
        IEND = LOC(J)-1
!
!----   Check for sign of numeric result
!
  110   CONTINUE
        MINUS = .FALSE.
        IF( INFO(IS+1).EQ.'+' ) THEN
          IS = IS+1
          IF( IS.GE.IEND ) GO TO 190
        END IF
        IF( INFO(IS+1).EQ.'-' ) THEN
          MINUS = .TRUE.
          IS    = IS+1
          IF( IS.GE.IEND ) GO TO 190
        END IF
!
!----   Primary loop decodes positive integer values
!
  120   CONTINUE
        ISETNO = ISETNO+1
        NUMB   = 0
        NDIGIT = 0
  130   CONTINUE
        IS = IS+1
        CH = INFO(IS)
        IF( (CH.GE.'0').AND.(CH.LE.'9') ) THEN
          READ(CH,940) J
          NUMB   = NUMB*10+J
          NDIGIT = NDIGIT+1
          IF( NDIGIT .GT. 10 ) GO TO 450
          IF( IS.GE.IEND ) THEN
            IF( MINUS ) NUMB = -NUMB
            GO TO 150
          END IF
          GO TO 130
        END IF
        IF( MINUS ) NUMB = -NUMB
!
!----   Check for repetition factor
!
        IF( CH.EQ.'*' ) THEN
          IF( (MULT.GT.0) .OR.(NDIGIT.LT.1) ) GO TO 190
          IF( (IS.GE.IEND).OR.(ISETNO.GT.1) ) GO TO 190
          IF( MINUS ) GO TO 110
          MULT   = NUMB
          ISTART = IS+1
          NCHARS = LOC(I+1)-ISTART
          GO TO 110
        END IF
!
!----   Check for decimal point.  Note that a 'minus' is saved.
!
        IF( CH.EQ.'.' ) THEN
          IF( DECSET.OR.EXPSET ) GO TO 190
          DECSET = .TRUE.
          IF( NDIGIT.GT.0 ) RDATA = FLOAT(NUMB)
          IF( IS.GE.IEND ) GO TO 170
          GO TO 120
        END IF
!
!----   Check for exponent marker
!
        IF( (CH.EQ.'E').OR.(CH.EQ.'e') ) THEN
          IF( EXPSET ) GO TO 190
          IF( (ISETNO.LT.2).AND.(NDIGIT.LT.1) ) GO TO 190
          EXPSET = .TRUE.
          IF( NDIGIT.GT.0 ) THEN
            D = FLOAT(NUMB)
            IF( DECSET ) THEN
              DO J = 1,NDIGIT
                D = 0.1*D
              END DO
            END IF
            RDATA = RDATA+D
          END IF
          IF( IS.GE.IEND ) GO TO 170
          GO TO 110
        END IF
!
!----   Cannot decipher string as a number
!
        GOTO 190
!
!----   Finish decoding numeric value
!
  150   CONTINUE
        IF( EXPSET ) THEN
          IF( NUMB.NE.0) RDATA = RDATA*(10.0**NUMB)
        ELSEIF( DECSET ) THEN
          D = FLOAT(NUMB)
          DO J = 1,NDIGIT
            D = 0.1*D
          END DO
          RDATA = RDATA+D
        ELSE
          RDATA = FLOAT(NUMB)
        END IF
!
!----   Fill in numeric data.  Add multiple values, if requested.
!
  170   CONTINUE
        NVALUE = NVALUE+1
        IF( NVALUE.GT.NVALS ) GO TO 470
        VALUE(NVALUE) = RDATA
        IF( NVALUE.LE.NKEYS ) THEN
          NTITY = NTITY+1
          NDXVAL(NVALUE) = NTITY
        END IF
!
        IF( MULT.GT.1 ) THEN
          IF( (NVALUE+MULT-1).GT.NVALS ) GO TO 470
          DO J = 2,MULT
            NVALUE = NVALUE+1
            VALUE(NVALUE) = RDATA
            IF( NVALUE.LE.NKEYS ) NDXVAL(NVALUE) = NTITY
          END DO
        END IF
        GOTO 220
!
!----   Transfer string as a character variable
!
  190   CONTINUE
        NCVALU = NCVALU+1
        IF( NCVALU.GT.NKEYS ) GO TO 480
        K=0
        DO J=ISTART,IEND
          K=K+1
          DUMY(K) = INFO(J)
        END DO
        DUMY(K+1) = ' '
        CALL XFKEY( DUMY, CVALUE(NCVALU) )
        NTITY = NTITY+1
        NDXCHR(NCVALU) = NTITY
!
!----   Fill in multiple character values, if requested
!
        IF( MULT.GT.1 ) THEN
          IF( (NCVALU+MULT-1).GT.NKEYS ) GO TO 480
          K = NCVALU
          DO J = 2,MULT
            NCVALU = NCVALU+1
            CVALUE(NCVALU) = CVALUE(K)
            NDXCHR(NCVALU) = NTITY
          END DO
        END IF
!
  220 CONTINUE
!
      RETURN
!
!---- Error handling
!
  450 CONTINUE
      IRERR = 1
      WRITE(IPERR,950)
      GOTO 490
  460 CONTINUE
      IRERR = 2
      WRITE(IPERR,960) MAXQQQ
      GOTO 490
  470 CONTINUE
      IRERR = 3
      WRITE(IPERR,970) NVALS
      GOTO 490
  480 CONTINUE
      IRERR = 4
      WRITE(IPERR,980) NKEYS
  490 CONTINUE
      WRITE(IPERR,990) ILINE
      RETURN
!
  940 FORMAT(I1)
  950 FORMAT(/' Error found in input data deck.  A number was encountered that'/ &
              ' was more than 10 digits long.  The limit is 10 digits.')
  960 FORMAT(/' Error in input data deck.  Too many strings in double quotes'/ &
              ' were found within a single data block.  Limit is ',I5,'.')
  970 FORMAT(/' Error in input data deck.  Too many numeric values'/ &
              ' were found within a single data block.  Limit is ',I5,'.')
  980 FORMAT(/' Error in input data deck.  Too many character values'/ &
              ' were found within a single data block.  Limit is ',I5,'.')
  990 FORMAT(/' Error at line ',I5,' of the input data file.'/ &
              ' Error termination in routine DCDSEP.')
      END SUBROUTINE
!
      SUBROUTINE FINSEP( NUM, LOC )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine locates all separators in the current input line,
!!    contained in the array INFO.
!!
!!  Call Arguments:
!!
!!    NUM : The total number of separators found.  (output)
!!    LOC : The location of each separator in the current input string.
!!          (output)
!!
!!  Comments:
!!
!!    1.  A dummy separator is placed at position LENCRD+1, if the last
!!    character in array INFO is a relevant character (not a comment
!!    or separator).
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: COMCHR, SEPCHR
!
! *** Call list variables
      INTEGER :: NUM
      INTEGER, DIMENSION(LENCRD) :: LOC
!
! *** Local variables
      LOGICAL :: INQUOT
      INTEGER :: I ! Looping control variable
      INTEGER :: LENGTH ! Local index for length of line processed
!
!---- Executable code ---------------------------------------------------
!
!---- Initialize pointers, exit if comment line
!
      NUM    = 0
      LENGTH = 0
      IF( COMCHR(1,INFO(1)) ) RETURN
!
!---- Determine length of line
!
      DO I = 1, LENCRD
        IF( INFO(I) .NE. ' ' ) LENGTH = I
      END DO
      IF( LENGTH .LT. LENCRD ) LENGTH = LENGTH+1
      IF( LENGTH .EQ. 0 ) RETURN
!
!---- Loop on the characters of the input line.  Check first for a
!     string in double quotes, then a comment character, then for a
!     separator character.
!
      INQUOT = .FALSE.
      DO 110 I = 1, LENGTH
!
!---- Look first for anything in double quotes
!
        IF( INFO(I).EQ.'"' ) THEN
          NUM = NUM+1
          LOC(NUM) = I
          INQUOT = .NOT.INQUOT
          GO TO 110
        END IF
        IF( INQUOT ) GO TO 110
!
!---- Look for comment character.  Terminate line if found
!
        IF( COMCHR(I,INFO(I)) ) THEN
          IF( (I.GT.1).AND.(NUM.GT.0) ) THEN
            IF( LOC(NUM).LT.(I-1) ) THEN
              NUM = NUM+1
              LOC(NUM) = I
            END IF
          END IF
          RETURN
        END IF
!
!--- Look for separator character
!
        IF( SEPCHR(INFO(I)) ) THEN
          NUM = NUM+1
          LOC(NUM) = I
        END IF
!
  110 CONTINUE
!
!---- Add ending separator location if:
!      1) Still within a double quotation
!      2) End of line has been reached without hitting a separator
!
      IF( INQUOT .OR. ((LENGTH.EQ.LENCRD).AND.(.NOT.SEPCHR(INFO(LENCRD)))) ) THEN
        NUM = NUM+1
        LOC(NUM) = LENCRD+1
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE SAVTTL( TITLE )
!!**********************************************************************
!!
!!  Purpose:
!!
!!   This subroutine saves the contents of the input line, following
!!   the first separator.
!!
!!  Call Argument:
!!
!!    TITLE : The text of the command line, with the leading keyword
!!            stripped off.  (output)
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford    : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 19 Sep 2002 : Increase card length logic
!!    Paul W. Eslinger :  5 Jan 2006 : Extend keyword line length
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: SEPCHR
!
! *** Call list variables
      CHARACTER(LEN=LENCRD) :: TITLE
!
! *** Local variables
      INTEGER :: IST ! Index of first quaote mark
      INTEGER :: I   ! Looping control variable
      INTEGER :: NB  ! Local index value for number of blanks
!
!---- Executable code ---------------------------------------------------
!
      IST=0
      DO I = 1, LENCRD
        IF( IST.EQ.0 ) THEN
          IF( SEPCHR(INFO(I)) ) IST = I
        ELSE
          IF( .NOT.SEPCHR(INFO(I)) ) THEN
             IST = I
             GO TO 110
          END IF
        END IF
      END DO
!
      WRITE(TITLE,500) (INFO(I),I=1,LENCRD)
  500 FORMAT(2048A1)
      RETURN
!
  110 CONTINUE
!
      NB = IST-1
      WRITE(TITLE,500) (INFO(I),I=IST,LENCRD),(' ',I=1,NB)
!
      RETURN
      END SUBROUTINE
!
      LOGICAL FUNCTION SEPCHR( CH )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This function determines if a character is a legal command
!!    line separator.
!!
!!  Call Argument:
!!
!!    CH : Character to examine for being a separator.  (input)
!!
!!  Comment:
!!
!!    The current conventions for RDBLK also use double quotes
!!    (") as a separator character.  It is used to indicate long
!!    text strings to be saved as literals.  However, they must
!!    be located in routines FINSEP and DCDSEP, rather than
!!    identified here.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=1) :: CH
!
!---- Executable code ---------------------------------------------------
!
      IF( (ICHAR(CH).LT.10) .OR. &
        (CH.EQ.' ') .OR. (CH.EQ.',')  .OR. &
        (CH.EQ.'=') .OR. (CH.EQ.':')  .OR. &
        (CH.EQ.';') .OR. (CH.EQ.'''') .OR. &
        (CH.EQ.'(') .OR. (CH.EQ.')') ) THEN
        SEPCHR=.TRUE.
      ELSE
        SEPCHR=.FALSE.
      END IF
!
      RETURN
      END FUNCTION
!
      SUBROUTINE NXTQOT( KINDEX, FSTRNG )
!!**********************************************************************
!!
!!  Purpose:
!!
!!   This subroutine returns the index and value of the 'next' quoted
!!   string in the data block, after the last keyword identified by a
!!   call to the function CEXIST.
!!
!!
!!  Call Arguments:
!!
!!    KINDEX : Index of desired quoted string, in the array QUOTE.
!!             (output)
!!    FSTRNG : Desired quoted string.  (output)
!!
!!
!!  Comment:
!!
!!    1.  If no previous call was made to CEXIST, if the desired
!!    keyword was not found, or if no subsequent quoted string
!!    data is present in the data block, the index is returned
!!    as zero.
!!
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 16 March 2000 : Version 1.0 : Add check on NQUOTE
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: KINDEX
      CHARACTER(LEN=*) :: FSTRNG
!
! *** Local variables
      INTEGER :: I      ! Local looping control value
      INTEGER :: N      ! Local index value
      INTEGER :: NSERCH ! Local looping value
!
!---- Executable code ---------------------------------------------------
!
      KINDEX   = 0
      FSTRNG = ' '
!
!---- No quote strings were entered
!
      IF( NQUOTE .LT. 1 ) RETURN
!
!---- No 'previous' call to CEXIST, or the keyword was not found
!
      IF( (NEXIST.LT.1) .OR. (NEXIST.GT.NKEYS) ) RETURN
!
!---- Check to see if numeric data is present
!
      N = NDXCHR(NEXIST)
      IF( N .GE. NDXQQQ(NQUOTE) ) RETURN
!
!---- Locate index and text of the next item in the input list
!
      NSERCH = MIN( NQUOTE, MAXQQQ )
      DO I = 1, NSERCH
        IF( NDXQQQ(I) .GT. N ) THEN
          KINDEX = I
          FSTRNG = QUOTE(KINDEX)
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE NXTVAL( KINDEX, KVAL )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine returns the index and value of the 'next'
!!    numeric value in the the data block, after the last keyword
!!    identified by a call to the function CEXIST.
!!
!!  Call Arguments:
!!
!!    KINDEX : Index of desired value, in the data array VALUE. (output)
!!    KVAL   : Desired numeric value.  (output)
!!
!! Comment:
!!
!!   If no previous call was made to CEXIST, if the desired keyword
!!   was not found, or if no subsequent numeric data is present in
!!   the data block, the index and value are returned as zero.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 16 March 2000 : Version 1.0 : Add check on NVALUE
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: KINDEX ! Index of desired value, in the data array VALUE
      REAL :: KVAL      ! Desired numeric value
!
! *** Local variables
      INTEGER :: N ! Local index value
      INTEGER :: I ! Local looping control value
!
!---- Executable code ---------------------------------------------------
!
      KINDEX = 0
      KVAL   = 0
!
!---- No 'previous' call to CEXIST, or the keyword was not found
!
      IF( (NEXIST.LT.1) .OR. (NEXIST.GT.NKEYS) ) RETURN
!
!---- No values were entered
!
      IF( NVALUE .LT. 1 ) RETURN
!
!---- Check to see if numeric data are present
!
      N = NDXCHR(NEXIST)
      IF( N .GE. NDXVAL(NVALUE) ) RETURN
!
!---- Locate index and value of the next item in the input list
!
      DO I = 1, NVALUE
        IF( NDXVAL(I).GT.N ) THEN
          KINDEX = I
          KVAL   = VALUE(KINDEX)
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE XFKEY( INFO, KEYWRD )
!!**********************************************************************
!!
!!  Purpose:
!!
!!   This subroutine examines the first eight characters in array
!!   INFO, in order to move that data into variable KEYWRD.  The
!!   keyword in KEYWRD is also converted to all upper case.
!!
!!  Call Arguments:
!!
!!    INFO   : The first eight characters of a keyword line.  (Input)
!!    KEYWRD : The keyword, as taken from the keyword line.  (Output)
!!
!! Comment:
!!
!!   If a separator is present, the relevant characters are
!!   transferred to variable KEYWRD, and the remainder of KEYWRD
!!   is blank filled.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: SEPCHR
!
! *** Call list variables
      CHARACTER(LEN=1), DIMENSION(8) :: INFO
      CHARACTER(LEN=8) :: KEYWRD
!
! *** Local variables
      CHARACTER(LEN=1), DIMENSION(8) :: JNFO ! Local vector of same size as INFO
      INTEGER :: I ! Looping variable
      INTEGER :: J ! Looping variable
!
!---- Executable code ---------------------------------------------------
!
      DO I = 1, 8
        JNFO(I) = INFO(I)
        IF( SEPCHR(INFO(I)) ) THEN
          DO J = I, 8
            JNFO(J) = ' '
          END DO
          EXIT
        END IF
      END DO
!
      WRITE(KEYWRD,'(8A1)') (JNFO(I),I=1,8)
      CALL UPCASE( KEYWRD )
!
      RETURN
      END SUBROUTINE

