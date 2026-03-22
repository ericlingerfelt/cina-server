MODULE convert
!DESC = This module contains general procedures for convert variables
!DESC = from one format to another
!
! Procedures in this module are:
! 
! convert_ver                        Version of this file
! lowercase(in)                      Convert string to lowercase
! nextnum(string,value,endnum,ovfl)  Find the first value in string
! sort2Darray(array,rows,cols,col)   Sort rows in 2D array
! str2line(in)                       Remove wierd ASCII characters from a string
! line2str(in)                       Decode strings from the str2line function
! get_date                           Get date as '02/14/2003'
! get_time                           Get time as '09:01:52:128'

! By default all procedures and global variables are private
  PRIVATE

  PUBLIC  :: convert_ver,lowercase,nextnum,sort2Darray,str2line,line2str
  PUBLIC  :: get_date,get_time,get_prop_value,set_prop_value,next_in_list
  PUBLIC  :: replaceall,num_occurrances

CONTAINS
!---------------------------------------------------------------------
  FUNCTION convert_ver
!PURPOSE = Returns the cvs revision number for this file
!STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: convert_ver
    CHARACTER(LEN=20),PARAMETER   :: CONVERT_VERSION = '$Revision: 1.1.1.1 $'

    convert_ver = CONVERT_VERSION(12:LEN_TRIM(CONVERT_VERSION)-2)

  END FUNCTION convert_ver

!---------------------------------------------------------------------
  FUNCTION lowercase(in)
!PURPOSE = Converts any uppercase alphabet letters in a string to lowercase
!STATUS = Complete and tested
!DESC = Works on standard and extended ASCII character strings
    IMPLICIT NONE
    CHARACTER(LEN=*)              :: in
    CHARACTER(LEN=LEN(in))        :: lowercase
    INTEGER(KIND=2)               :: i,j,l

    lowercase = ''
    l = LEN_TRIM(in)
    DO i = 1,l
       j = IACHAR(in(i:i))
       IF ((j >= 65) .AND. (j <= 90)) THEN
          lowercase(i:i) = CHAR(j + 32)
       ELSE
          lowercase(i:i) = in(i:i)
       END IF
    END DO
  END FUNCTION lowercase

!---------------------------------------------------------------------
  SUBROUTINE nextnum(buffer,ans,endnum,status,plevel)
!PURPOSE = Get the next number from a string
!STATUS = Complete but needs more testing
!DESC = Scans buffer starting at index endnum for a number.
!DESC = If found, the answer is returned in ans and the index to just after
!DESC = the end of the number in buffer is returned in endnum, and status = 0
!DESC = 
!DESC = If a number is not found, ans = 0D0, endnum = 0, and status = 0
!DESC = If the number is too large to represent in ans, then status = 1
!DESC = and ans will be the largest number available
!DESC =
!DESC = status will equal 2 when the number had to be truncated
!DESC =
!DESC = This routine looks for numbers in the following form
!DESC = [sign] [digits] [decimal point] [digits] [exp. indicator] [sign] [digits]
!        neg    dig1                      dig2                    exp_neg   dig3
!       Above is the variables where parts of the number are stored

    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: buffer
    REAL(KIND=8),INTENT(OUT)      :: ans
    INTEGER(KIND=4),INTENT(INOUT) :: endnum
    INTEGER(KIND=4),INTENT(OUT)   :: status
    INTEGER(KIND=1),INTENT(IN)    :: plevel
    OPTIONAL                      :: plevel

    CHARACTER(LEN=2),PARAMETER    :: ws = ' ,'
    CHARACTER(LEN=10),PARAMETER   :: digit = '0123456789'
    CHARACTER(LEN=4),PARAMETER    :: exp_ind = 'EeDd'
    CHARACTER(LEN=3),PARAMETER    :: other = '.+-'
    CHARACTER(LEN=17),PARAMETER   :: all = digit//exp_ind//other   !'0123456789.+-EeDd'

    LOGICAL(KIND=1)               :: neg,exp_neg,loop
    INTEGER(KIND=1)               :: pl
    CHARACTER(LEN=40)             :: dig2                ! dig2 is a string so that 1.0001 will be '0001'
    CHARACTER(LEN=1)              :: chr,LF
    CHARACTER(LEN=3)              :: AD
    CHARACTER(LEN=80)             :: tmps
    INTEGER(KIND=4)               :: ind,ind_max,ind_start,j,dig
    REAL(KIND=8)                  :: dig1,dig3,tmp

    ind_max = LEN_TRIM(buffer)
    ans = 0d0
    dig1 = 0d0
    dig2 = ''
    dig3 = 0d0
    exp_neg = .FALSE.
    ind = endnum
    status = 0

    IF (PRESENT(plevel)) THEN
       pl = plevel
    ELSE
       pl = 1
    END IF

    IF (pl >= 4) THEN
       AD = 'YES'
       LF = ''
    ELSE
       AD = 'NO'
       LF = ACHAR(10)
    END IF

    IF (ind > ind_max) THEN
       endnum = 0; RETURN
    END IF

! Find the first character in all and return if none were found
    loop = .TRUE.
    DO WHILE(loop)
       neg = .FALSE.
       dig = 1
       IF (pl >= 3) THEN
          WRITE(tmps,'(A,I0,A,A)',IOSTAT=j) 'ind_start=',ind,' | ',TRIM(buffer(ind:))
          WRITE(*,'(T1,A)',ADVANCE=AD) TRIM(tmps)
       END IF
       ind_start = ind
       ind = SCAN(buffer(ind:),digit//other) + ind - 1
       IF (pl >= 3) THEN
          IF (pl == 3) WRITE(*,'(2A)',ADVANCE=AD) REPEAT(' ',LEN(tmps)),LF
          WRITE(tmps,'(A,I0,A,A)',IOSTAT=j) 'ind_after=',ind,' | ',TRIM(buffer(ind:))
          WRITE(*,'(T1,A)',ADVANCE=AD) TRIM(tmps)
       END IF
       IF (ind < ind_start) THEN
          endnum = 0
          RETURN
       END IF

       chr = buffer(ind:ind)
       IF (chr == '+') THEN
          IF (ind < ind_max) THEN
             ind=ind+1; chr = buffer(ind:ind)
          ELSE
             endnum = 0
             RETURN
          END IF
       END IF
       IF (chr == '-') THEN
          IF (ind < ind_max) THEN
             ind=ind+1; chr = buffer(ind:ind); neg = .TRUE.
          ELSE 
             endnum = 0
             RETURN
          END IF
       END IF
! Look for a digit or a decimal point followed by a digit
       IF (chr == '.') THEN
          IF (ind < ind_max) THEN
             ind=ind+1; chr = buffer(ind:ind); dig = 2
! Signal to read into dig2 instead of dig1
          ELSE 
             endnum = 0
             RETURN
          END IF
       END IF
       IF (SCAN(chr,digit) /= 0) THEN
          loop = .FALSE.
          j = ICHAR(chr) - 48
          IF (dig == 1) THEN 
             dig1 = j
          ELSE 
             dig2 = chr
          END IF
          ind = ind + 1; chr = buffer(ind:ind)
       END IF
    END DO

    IF (pl >= 3) THEN
       IF (pl == 3) WRITE(*,'(2A)',ADVANCE=AD) REPEAT(' ',LEN(tmps)),LF
       WRITE(tmps,'(A,I0,A,G,2A)',IOSTAT=j) 'A: dig=',dig,' dig1=',dig1,' dig2=',dig2
       WRITE(*,'(T1,A)',ADVANCE=AD) TRIM(tmps)
    END IF

! Read in digits before the decimal point
    IF (dig == 1) THEN
       loop = .TRUE.
       DO WHILE(loop)
          IF (SCAN(chr,digit) /= 0) THEN
! Check for overflow
             IF (dig1 > HUGE(dig1) * 0.1) THEN
                status = 1
                ans = HUGE(dig1)
                IF (neg) ans = -ans
! find the end of the character string to return in endnum
                endnum = VERIFY(buffer(ind:),digit) + ind
                IF (endnum == 0) endnum = ind_max + 1
                RETURN
             END IF

             dig1 = dig1*10d0 + REAL(ICHAR(chr) - 48)
! Check if some precision will be lost
             IF (CEILING(LOG10(dig1)) > PRECISION(dig1)) status = 2

             IF (ind < ind_max) THEN
                ind = ind + 1; chr = buffer(ind:ind)
             ELSE
                ans = dig1
                endnum = ind + 1
                IF (neg) ans = -ans
                RETURN
             END IF
          END IF
          IF (chr == '.') THEN
             IF (ind < ind_max) THEN
                ind = ind + 1; chr = buffer(ind:ind); dig = 2
                loop = .FALSE.
             ELSE
                ans = dig1
                endnum = ind
                IF (neg) ans = -ans
                RETURN
             END IF
          END IF
! If a character other than a digit, decimal, or exponent indicator
          IF (VERIFY(chr,digit//'.'//exp_ind) /= 0) THEN
             ans = dig1
             endnum = ind
             IF (neg) ans = -ans
             RETURN
          END IF
          IF (SCAN(chr,exp_ind) /= 0) THEN
             IF (ind < ind_max) THEN
                ind = ind + 1; chr = buffer(ind:ind); dig = 3
                loop = .FALSE.
             ELSE
                ans = dig1
                endnum = ind
                IF (neg) ans = -ans
                RETURN
             END IF
          END IF
       END DO
    END IF

    IF (pl >= 3) THEN
       IF (pl == 3) WRITE(*,'(2A)',ADVANCE=AD) REPEAT(' ',LEN(tmps)),LF
       WRITE(tmps,'(A,I0,A,G,2A)',IOSTAT=j) 'B: dig=',dig,' dig1=',dig1,' dig2=',dig2
       WRITE(*,'(T1,A)',ADVANCE=AD) TRIM(tmps)
    END IF

! Read in digits between the decimal point and exponent indicator
    IF (dig == 2) THEN
       loop = .TRUE.
       DO WHILE(loop)
          IF (VERIFY(chr,digit//exp_ind) /= 0) THEN
             dig2 = TRIM(dig2) // '.'
             READ(dig2,'(G)') tmp
             IF (tmp > 0D0) THEN
                ans = dig1 + tmp * (10D0 ** (-(LEN_TRIM(dig2)-1)))
             ELSE
                ans = dig1
             END IF
             endnum = ind
             IF (neg) ans = -ans
             RETURN
          END IF
          IF (SCAN(chr,digit) /= 0) THEN
! Prevent overflow by only adding chr if there is room for a period
! and the total number of digits can be representated
             IF ((LEN_TRIM(dig2) < LEN(dig2) - 1) .AND. (LEN_TRIM(dig2) < PRECISION(dig1))) THEN
                dig2 = TRIM(dig2) // chr
! Check if the number of digits in dig1 and dig2 are > precision
! j holds the number of digits of dig1.  
! A small number was added so that if dig1 = 10, j = 2 instead of 1
! because log10(10) = 1 even though 10 has 2 digits
                j = CEILING(LOG10(dig1 + 0.1))
                IF (j+LEN_TRIM(dig2) > PRECISION(dig1)) status = 2
             ELSE
! Otherwise ignore remaining characters
                status = 2
             END IF
             IF (pl >= 4) THEN
!IF (pl == 3) WRITE(*,'(2A)',ADVANCE=AD) REPEAT(' ',LEN(tmps)),LF
                WRITE(tmps,'(4A)',IOSTAT=j) 'BB: dig2=',dig2,' chr=',chr
                WRITE(*,'(T1,A)',ADVANCE=AD) TRIM(tmps)
             END IF
             IF (ind < ind_max) THEN
                ind = ind + 1; chr = buffer(ind:ind)
             ELSE
                dig2 = TRIM(dig2) // '.'
                READ(dig2,'(G)') tmp
                IF (tmp > 0D0) THEN
                   ans = dig1 + tmp * (10D0 ** (-(LEN_TRIM(dig2)-1)))
                ELSE
                   ans = dig1
                END IF
! find the end of the character string to return in endnum
                endnum = VERIFY(buffer(ind:),digit) + ind
                IF (neg) ans = -ans
                RETURN
             END IF
          END IF
          IF (SCAN(chr,exp_ind) /= 0) THEN
             IF (ind < ind_max) THEN
                ind = ind + 1; chr = buffer(ind:ind); dig = 3
                loop = .FALSE.
             ELSE
                dig2 = TRIM(dig2) // '.'
                READ(dig2,'(G)') tmp
                IF (tmp > 0D0) THEN
                   ans = dig1 + tmp * (10D0 ** (-(LEN_TRIM(dig2)-1)))
                ELSE
                   ans = dig1
                END IF
                endnum = ind
                IF (neg) ans = -ans
                RETURN
             END IF
          END IF
       END DO
    END IF

    IF (pl >= 3) THEN
       IF (pl == 3) WRITE(*,'(2A)',ADVANCE='NO') REPEAT(' ',LEN(tmps)),LF
       WRITE(tmps,'(A,I0,A,G,2A)',IOSTAT=j) 'C: dig=',dig,' dig1=',dig1,' dig2=',dig2
       WRITE(*,'(T1,A)',ADVANCE=AD) TRIM(tmps)
    END IF

! The exp_ind has been found, check if a sign is present
    IF (chr == '+') THEN
       ind = ind + 1; chr = buffer(ind:ind)
    ELSEIF (chr == '-') THEN
       ind = ind + 1; chr = buffer(ind:ind)
       exp_neg = .TRUE.
    END IF

! Read in the digits for the exponent
    DO 
       IF (SCAN(chr,digit) /= 0) THEN
          dig3 = dig3*10D0 + REAL(ICHAR(chr) - 48)
!! Check for overflow
          IF (dig3 > RANGE(dig3)) THEN
             status = 1
             ans = HUGE(dig3)
             IF (neg) ans = -ans
! find the end of the character string to return in endnum
             endnum = VERIFY(buffer(ind:),digit) + ind
             IF (endnum == 0) endnum = ind_max + 1
             RETURN
          END IF
          IF (ind < ind_max) THEN
             ind = ind + 1; chr = buffer(ind:ind)
          ELSE
             IF (exp_neg) dig3 = -dig3
             dig2 = TRIM(dig2) // '.'
             READ(dig2,'(G)') tmp
             IF (tmp > 0D0) THEN
                ans = (dig1 + tmp * (10D0 ** (-(LEN_TRIM(dig2)-1)))) * (10D0 ** dig3)
             ELSE
                ans = dig1 * (10D0 ** dig3)
             END IF
             endnum = ind + 1
             IF (neg) ans = -ans
             IF (pl >= 4) WRITE(*,'(A,G,A,G)') 'D1: dig3=',dig3,' ans=',ans
             RETURN
          END IF
       ELSE
          IF (exp_neg) dig3 = -dig3
          dig2 = TRIM(dig2) // '.'
          READ(dig2,'(G)') tmp
          IF (tmp > 0D0) THEN
             ans = (dig1 + tmp * (10D0 ** (-(LEN_TRIM(dig2)-1)))) * (10D0 ** dig3)
          ELSE
             ans = dig1 * (10D0 ** dig3)
          END IF
          endnum = ind
          IF (neg) ans = -ans
          IF (pl >= 4) WRITE(*,'(A,G,A,G)') 'D2: dig3=',dig3,' ans=',ans
          RETURN
       END IF
    END DO
  END SUBROUTINE nextnum

!---------------------------------------------------------------------
  FUNCTION sort2Darray(array,rows,cols,col)
!PURPOSE = Sort the rows in a 2D array according to column index col
!STATUS = Complete and tested
!DESC = The section of the array sorted is array(1:rows,1:cols)
!DESC = The column that will end up from least to greatest is col
!
!CAUTION = This does not check rows, cols, or col to make sure they
!CAUTION = are inside the array bounds
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)     :: rows,cols,col
    REAL(KIND=8),INTENT(IN)        :: array(:,:)
    REAL(KIND=8)                   :: sort2Darray(UBOUND(array,1),UBOUND(array,2))
    INTEGER(KIND=4)                :: ind(rows),i
    LOGICAL                        :: mask(rows)

! Find the index values of the sorted column so that
! ind(1) is the smallest, ind(2) is the next smallest, etc.
    mask = .TRUE.
    DO i = 1,rows
       ind(i) = MINLOC(array(1:rows,col),1,mask)
       mask(ind(i)) = .FALSE.
    END DO

! Copy the array into sort2Darray but sort it with ind
    DO i = 1,rows
       sort2Darray(i,1:cols) = array(ind(i),1:cols)
    END DO
  END FUNCTION sort2Darray

!---------------------------------------------------------------------
  FUNCTION str2line(in)
!PURPOSE = Remove wierd ASCII characters from a string
!STATUS = Complete and tested
!CAUTION = The return value is 4 times as long as the input. Use TRIM to shorten
!DESC = ASCII characters with values less than 32 or greater than 126
!DESC = are encoded to produce a string without these characters.
!DESC = For example, the newline character is represented with "\0A"
!DESC = and the carriage return character is represented with "\0D"
!DESC = Any "\" characters in the original string are replaced with "\\"
!DESC = Use the line2str function to decode this conversion 
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: in
    CHARACTER(LEN=LEN(IN)*4)      :: str2line
    CHARACTER(LEN=1)              :: c
    INTEGER(KIND=4)               :: cin,cout,l

    cin = 1
    cout = 1
    str2line = ''
    l = LEN(in)
    DO cin = 1, l
       c = in(cin:cin)
       SELECT CASE (IACHAR(c))
       CASE (32:126)         ! Normal character
! Check for "\" character
          IF (c == ACHAR(92)) THEN
             str2line(cout:cout+1) = ACHAR(92)//ACHAR(92)  ! Replace with "\\"
             cout = cout + 2
          ELSE
             str2line(cout:cout) = c   ! Copy without changing
             cout = cout + 1
          END IF
       CASE DEFAULT          ! Other character
          WRITE(str2line(cout:cout+2),'(A1,Z2.2)')  ACHAR(92)//ACHAR(92),IACHAR(c)
          cout = cout + 3
       END SELECT
    END DO

  END FUNCTION str2line

!---------------------------------------------------------------------
  FUNCTION line2str(in)
!PURPOSE = Decode strings from the str2line function
!STATUS = Complete and tested
!RETURNS = String the same length as the input
!DESC = line2str is the inverse of str2line
    IMPLICIT NONE
    CHARACTER(LEN=22),PARAMETER   :: H = '0123456798ABCDEFabcdef'
    CHARACTER(LEN=*),INTENT(IN)   :: in
    CHARACTER(LEN=LEN(IN))        :: line2str
    CHARACTER(LEN=1)              :: c
    INTEGER(KIND=4)               :: cin,cout,i,l

    cin = 1
    cout = 1
    line2str = ''
    l = LEN(in)
    DO WHILE (cin <= l)
       c = in(cin:cin)
! Look for "\"
       IF (c == ACHAR(92)) THEN
          cin = cin + 1    ! Advance cursor past first "\"

! If a "\", look for another one
          IF (in(cin:cin) == ACHAR(92)) THEN
! Have "\\"
             line2str(cout:cout) = ACHAR(92)
          ELSE
! Should have 2 HEX digits
             IF (VERIFY(in(cin:cin+1),H) == 0) THEN
! If a hex digit
                READ(in(cin:cin+1),'(Z2)') i
                line2str(cout:cout) = ACHAR(i)
                cin = cin + 1
             ELSE
! If other character then treat "\" as a normal character
! This case should only happen if string is encoded incorrectly
                line2str(cout:cout+1) = ACHAR(92) // in(cin:cin)
                cout = cout + 1
             END IF
          END IF
          cout = cout + 1
       ELSE
! If a normal character
          line2str(cout:cout) = c
          cout = cout + 1
       END IF
       cin = cin + 1
    END DO
  END FUNCTION line2str

!---------------------------------------------------------------------
  FUNCTION get_date
!PURPOSE = Get date as '02/14/2003'
!STATUS = Complete and tested
!RETURNS = CHARACTER(LEN=10)
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: get_date,d

    CALL DATE_AND_TIME(d)
    get_date = d(5:6) // '/' // d(7:8) // '/' // d(1:4)

  END FUNCTION get_date

!---------------------------------------------------------------------
  FUNCTION get_time
!PURPOSE = Get time as '09:01:52:128'
!STATUS = Complete and tested
!RETURNS = CHARACTER(LEN=12)
    IMPLICIT NONE
    CHARACTER(LEN=12)             :: get_time,t

    CALL DATE_AND_TIME(TIME=t)
    get_time = t(1:2) // ':' // t(3:4) // ':' // t(5:6) // ':' // t(8:10)

  END FUNCTION get_time

!---------------------------------------------------------------------
!****if* convert/get_prop_value
! NAME
!  FUNCTION get_prop_value(prop_name,prop_string,prop_separator,value_separator)
! PURPOSE
!  Return a rate property value from a string
! STATUS
!  Complete and tested
! USAGE
!  Properties consist of name/value pairs separated by value_separator.
!  Properties are stored in prop_string separated by prop_separator.
! INPUTS
!  prop_name: Name of property to obtain value of
!  prop_string: String of property names and values
!  prop_separator: Character sequence used to separate properties
!  value_separator: Character sequence used to separate names from values
! RETURN VALUE
!  String containing value.  String will be blank if property isn't in list
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2004 jpscott       Start function
! SOURCE

  FUNCTION get_prop_value(prop_name,prop_string,prop_separator, &
       value_separator)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: prop_name,prop_string
    CHARACTER(LEN=*),INTENT(IN)   :: prop_separator,value_separator
    CHARACTER(LEN=LEN(prop_string)):: get_prop_value
    INTEGER(KIND=4)               :: name_start,name_end,value_start

    name_start = INDEX(prop_separator // TRIM(prop_string) // &
         prop_separator, prop_separator // TRIM(prop_name) // &
         value_separator)
!print *,'name_start ',name_start,'"', &
!     prop_string(name_start:name_start),'"'

    IF (name_start > 0) THEN
! Name is in prop_list
       name_end = INDEX(TRIM(prop_string(name_start:))//prop_separator, &
            prop_separator) + name_start - 2
!print *,'name_end ',name_end,'"',prop_string(name_end:name_end),'"'
       
       value_start = LEN(prop_separator) + LEN_TRIM(prop_name) + &
            LEN(value_separator) + name_start - 1
!print *,'value_start ',value_start,'"', &
!prop_string(value_start:value_start),'"'

       get_prop_value = prop_string(value_start:name_end)
    ELSE
       get_prop_value = ''
    END IF

  END FUNCTION get_prop_value
!***

!---------------------------------------------------------------------
!****is* convert/set_prop_value
! NAME
!  SUBROUTINE set_prop_value(prop_name,prop_value,prop_string,
!       prop_separator,value_separator)
! PURPOSE
!  Set a rate property value in a string
! STATUS
!  Complete and tested
! CAUTION
!  prop_string will be truncated if too small to hold new value
! USAGE
!  Properties consist of name/value pairs separated by value_separator.
!  Properties are stored in prop_string separated by prop_separator.
!  Properties are removed from prop_string if their value is empty
!  
!  prop_string is the only input that is modified by this subroutine
! INPUTS
!  prop_name: Name of property to obtain value of
!  prop_value: Value of property for a certain name
!  prop_string: String of property names and values
!  prop_separator: Character sequence used to separate properties
!  value_separator: Character sequence used to separate names from values
! OUTPUTS
!  prop_string: String of property with new value
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2004 jpscott       Start subroutine
! SOURCE

  SUBROUTINE set_prop_value(prop_name,prop_value,prop_string, &
       prop_separator,value_separator)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: prop_name,prop_value
    CHARACTER(LEN=*),INTENT(IN)   :: prop_separator,value_separator
    CHARACTER(LEN=*),INTENT(INOUT):: prop_string
    INTEGER(KIND=4)               :: name_start,name_end,value_start

    name_start = INDEX(prop_separator // TRIM(prop_string) // &
         prop_separator,prop_separator // TRIM(prop_name) // &
         value_separator)
!print *,'name_start ',name_start,'"', &
!     prop_string(name_start:name_start),'"'

    IF (name_start > 0) THEN
! Property is in prop_string so replace value with prop_value
       name_end = INDEX(TRIM(prop_string(name_start:))//prop_separator, &
            prop_separator) + name_start - 2
!print *,'name_end ',name_end,'"',prop_string(name_end:name_end),'"'
       
       value_start = LEN(prop_separator) + LEN_TRIM(prop_name) + &
            LEN(value_separator) + name_start - 1
!print *,'value_start ',value_start,'"', &
!prop_string(value_start:value_start),'"'

       prop_string = prop_string(1:value_start-1) // TRIM(prop_value) // &
            prop_string(name_end+1:)
    ELSE
! Property is not in prop_string so add it
       IF (prop_string == '') THEN
          prop_string = TRIM(prop_name) // value_separator // prop_value
       ELSE
          prop_string = TRIM(prop_string) // prop_separator // &
               TRIM(prop_name) // value_separator // prop_value
       END IF
    END IF
  END SUBROUTINE set_prop_value
!***

!---------------------------------------------------------------------
!****if* convert/next_in_list
! NAME
!  FUNCTION next_in_list(item,list,separator)
! PURPOSE
!  Return next item in a separated list
! STATUS
!  Complete and tested
! INPUTS
!  list: delimated character array containing list items
!  separator: character array sequence deliminating list items
! OUTPUTS
!  item: character array with next item in list
!  list: character array with remaining list items (item is removed list)
! USAGE
!  The first item in a list is removed from the list and returned
! RETURN VALUE
!  LOGICAL(KIND=4) that is .TRUE. if the list still has items in it
! SOURCE

  FUNCTION next_in_list(item,list,separator)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT)   :: list
    CHARACTER(LEN=*),INTENT(OUT)     :: item
    CHARACTER(LEN=*),INTENT(IN)      :: separator
    LOGICAL(KIND=4)                  :: next_in_list
    INTEGER(KIND=4)                  :: i
    
    i = INDEX(list,separator)
    IF (i > 0) THEN
       item = list(:i-1)
       list = list(i+1:)
       next_in_list = .TRUE.
    ELSE
       item = list
       list = ''
       next_in_list = .FALSE.
    END IF
  END FUNCTION next_in_list
!***

!---------------------------------------------------------------------
  FUNCTION replaceall(str,find,replace)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: find,replace
    CHARACTER(LEN=*),INTENT(INOUT)   :: str
    CHARACTER(LEN=LEN(str))          :: replaceall
    INTEGER(KIND=4)                  :: cnt_src,cnt_dest,n,l
    INTEGER(KIND=4)                  :: len_find,len_replace

    cnt_src = 1
    cnt_dest = 1
    len_find = LEN_TRIM(find)
    len_replace = LEN_TRIM(replace)
    n = INDEX(str,find)

    DO WHILE (n > cnt_src - 1)
! Copy up to string to replace
       l = n - cnt_src + cnt_dest
       replaceall(cnt_dest:l-1) = str(cnt_src:n-1)
       
! Append replace to replaceall
       replaceall(l:l+len_replace-1) = replace(1:len_replace)
       
       cnt_src = n + len_find
       cnt_dest = l + len_replace
       n = INDEX(str(cnt_src:),find) + cnt_src - 1
    END DO

    replaceall(cnt_dest:) = str(cnt_src:)
  END FUNCTION replaceall

!---------------------------------------------------------------------
  FUNCTION num_occurrances(str,find)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: str,find
    INTEGER(KIND=4)                  :: num_occurrances,start,i

    num_occurrances = 0
    start = 1
    
    DO WHILE (.TRUE.)
       i = INDEX(str(start:),find)
       IF (i < 1) RETURN

       start = start + i
       num_occurrances = num_occurrances + 1
    END DO
  END FUNCTION num_occurrances

!---------------------------------------------------------------------
!---------------------------------------------------------------------
END MODULE convert
