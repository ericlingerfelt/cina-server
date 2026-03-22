MODULE cinaadmin_core
  !DESC = This module contains procedures and data types for the cinaadmin program
  !CAUTION = This module may be used by any program so put any security related procedures in cinaadmin
  !
  ! Procedures in this module are:
  !
  ! FUNCTION cinaadmin_core_ver                Version of this file
  ! SUBROUTINE chk_options(PROGRAM_VER)         Process command line options
  ! FUNCTION decode_user_entry(tmps)            Decode user entry string into cinaadm data type
  ! FUNCTION encode_user_entry(padm)            Encode user entry string from cinaadm data type
  ! FUNCTION chk_timeout(date,time,timeout)     Check if timeout minutes has elapsed since date and time

  ! Use Intel Fortran compiler portability function interface module
  USE IFLPORT
  USE cina_core

  ! By default all procedures and global variables are private
  PRIVATE

  PUBLIC   :: cinaadmin_core_ver,chk_admoptions,decode_user_entry
  PUBLIC   :: encode_user_entry,chk_timeout

  TYPE,PUBLIC                      :: cinaadm
     CHARACTER(LEN=50)             :: USER = ''
     CHARACTER(LEN=25)             :: PW = ''
     INTEGER(KIND=4)               :: TIMEOUT = -1
  END TYPE cinaadm

  TYPE,PUBLIC                      :: p_admin_options
     INTEGER(KIND=1)               :: help
     INTEGER(KIND=1)               :: version
     CHARACTER(LEN=100)            :: dir = ''
     CHARACTER(LEN=20)             :: action = ''
  END TYPE p_admin_options
  
CONTAINS
  !---------------------------------------------------------------------
  FUNCTION cinaadmin_core_ver()
    !PURPOSE = Return the cvs revision number for this file
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: cinaadmin_core_ver
    CHARACTER(LEN=20),PARAMETER   :: CINAADM_C_VERSION = '$Revision: 1.1.1.1 $'

    cinaadmin_core_ver = CINAADM_C_VERSION(12:LEN_TRIM(CINAADM_C_VERSION)-2)

  END FUNCTION cinaadmin_core_ver

  !---------------------------------------------------------------------
  SUBROUTINE chk_admoptions(paopt,padm)
    !PURPOSE = Process any command line options
    !STATUS = Complete and tested
    !DESC = This subroutine does not return if showing help or version info
    USE convert
    USE io
    IMPLICIT NONE
    TYPE(cinaadm),INTENT(INOUT)     :: padm(:)
    TYPE(p_admin_options),INTENT(INOUT) :: paopt
    CHARACTER(LEN=80)                :: tmps,tmps2
    INTEGER(KIND=4)                  :: n,i,j

    ! Read in the number of command-line arguments
    n = IARGC()

    IF (n < 1) THEN                    ! If no options are present, print help message
       paopt%help = 1
       RETURN
    END IF

    CALL GETARG(1,paopt%action)
    SELECT CASE (lowercase(paopt%action))
    CASE ('-h ','--help ')
       paopt%help = 1
       RETURN
    CASE ('-v ','--version ')
       paopt%version = 1
       RETURN
    CASE ('adduser ', 'changeuser ')
       CALL GETARG(2,padm(1)%USER)
       IF (n < 2) THEN                    ! If user is not specified, print help message
          paopt%help = 1
          RETURN
       END IF

       i = 3
       DO WHILE (i <= n)
          CALL GETARG(i,tmps)
          CALL GETARG(i+1,tmps2)
          SELECT CASE (TRIM(lowercase(tmps)))
          CASE ('-p ')
             padm(1)%PW = tmps2
             i = i + 1
          CASE ('-t ')
             READ(tmps2,'(I)') padm(1)%TIMEOUT
             IF (padm(1)%TIMEOUT < 1) CALL printerror('The timeout must be > 0 minutes',1)
             i = i + 1
          CASE DEFAULT
             CALL printerror('Unknown option "'//TRIM(tmps)//'"',1)
          END SELECT
          i = i + 1
       END DO
       
    CASE ('listusers ', 'removeusers ')
       i = 10  !UBOUND(padm)
       IF (n-1 < i) i = n-1
       DO j = 1, i
          CALL GETARG(1+j,padm(j)%USER)
       END DO
       RETURN
    CASE ('autologout ')
       RETURN
    CASE DEFAULT
       CALL printerror('Unknown action "'//TRIM(paopt%action)//'"',1)
    END SELECT

  END SUBROUTINE chk_admoptions

  !---------------------------------------------------------------------
  FUNCTION decode_user_entry(tmps)
    !PURPOSE = Decode user entry string into cinaadm data type
    !STATUS = Complete and tested
    USE io
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)    :: tmps
    TYPE(cinaadm)                 :: decode_user_entry
    INTEGER(KIND=4)                :: i,j,c
    CHARACTER(LEN=25)              :: str
    
    ! Decode username
    c = 1
    DO WHILE (tmps(c:c) /= ACHAR(8))
       decode_user_entry%USER(c:c) = tmps(c:c)
       c = c + 1
       IF (c > LEN(decode_user_entry%USER)) CALL printerror('Invalid string to decode.',1)
    END DO

    ! Decode password
    j = 122 - IACHAR(tmps(c+26:c+26))   ! Get the number of characters in the password
    DO i = 1, j
       decode_user_entry%PW(i:i) = ACHAR(158 - IACHAR(tmps(c+i:c+i)))
    END DO
    c = c + 27

    ! Decode Timeout
    j = c + 1
    str = ''
    DO WHILE (tmps(j:j) /= ACHAR(8))
       str(j-c:j-c) = tmps(j:j)
       j = j + 1
       IF (j-c > 4) CALL printerror('Invalid timeout to decode.',1)
    END DO
    READ(str,'(I)') decode_user_entry%TIMEOUT

  END FUNCTION decode_user_entry

  !---------------------------------------------------------------------
  FUNCTION encode_user_entry(padm)
    !PURPOSE = Encode user entry string from cinaadm data type
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=100)             :: encode_user_entry
    TYPE(cinaadm),INTENT(IN)      :: padm
    INTEGER(KIND=4)                :: i,j,c ! c is the index into output
    REAL(KIND=8)                   :: r

    encode_user_entry = ''

    ! Encode username
    i = LEN_TRIM(padm%USER)
    encode_user_entry(1:i) = TRIM(padm%USER)
    IF (i <= 0) encode_user_entry(1:50) = 'Invalid Entry'
    IF (LEN_TRIM(padm%PW) <= 0) encode_user_entry(1:50) = 'Invalid Entry'
    c = LEN_TRIM(encode_user_entry) + 1
    encode_user_entry(c:c) = ACHAR(8)

    ! Encode password
    i = LEN_TRIM(padm%PW)
    DO j = 1, i
       encode_user_entry(c+j:c+j) = ACHAR(158-IACHAR(padm%PW(j:j)))
    END DO
    DO j = i+1, 25
       CALL RANDOM_NUMBER(r)
       encode_user_entry(c+j:c+j) = ACHAR(INT(r*94)+32)
    END DO
    c = c + 26
    ! Position 76 stores the length of the password
    encode_user_entry(c:c) = ACHAR(122-i)
    c = c + 1
    encode_user_entry(c:c) = ACHAR(8)

    ! Encode Timeout
    i = padm%TIMEOUT
    IF (i <= 0) i = 20
    IF (i > 999) i = 999
    WRITE(encode_user_entry(c+1:),'(I0)') i
    c = LEN_TRIM(encode_user_entry) + 1
    encode_user_entry(c:c) = ACHAR(8)

    encode_user_entry(c+1:c+1) = '~'

  END FUNCTION encode_user_entry

  !---------------------------------------------------------------------
  FUNCTION chk_timeout(date_in,time_in,timeout)
    !PURPOSE = Check if timeout minutes has elapsed since date_in and time_in
    !STATUS = 
    !RETURNS = LOGICAL(KIND=4), .TRUE. if timeout has passed
    !NOTES = date_in is CHARACTER(LEN=8) in the format 'yyyymmdd'
    !NOTES = time_in is CHARACTER(LEN=6) in the format 'hhmmss'
    !NOTES = timeout is INTEGER(KIND=4) in the range [1,24*60]
    !CAUTION = seconds are ignored
    USE io
    IMPLICIT NONE
    CHARACTER(LEN=8),INTENT(IN)    :: date_in
    CHARACTER(LEN=6),INTENT(IN)    :: time_in
    INTEGER(KIND=4),INTENT(IN)     :: timeout
    LOGICAL(KIND=4)                :: chk_timeout
    LOGICAL(KIND=1)                :: carry,leapyr,last_day_of_month
    INTEGER(KIND=4)                :: i,j,minutes,cminutes
    CHARACTER(LEN=10)              :: tmps1,tmps2,curdate,curtime

    ! This functions returns as soon as it is known that timeout occured.
    chk_timeout = .TRUE.                          ! Assume timeout has passed until verified otherwise
    IF (timeout > 24*60) CALL printerror('timeout too large in chk_timeout',1)

    CALL DATE_AND_TIME(curdate,curtime)               ! Put current date into curdate and time into curtime

    tmps1 = date_in(1:4)
    READ(tmps1,'(I)') i                           ! Convert year string to integer
    tmps2 = curdate(1:4)
    READ(tmps2,'(I)') j                           ! Convert current year string to integer
    SELECT CASE (j-i)
    CASE (:-1)                                    ! negative (backward in time)
       CALL printerror('Invalid year value in chk_timeout.',1)
    CASE (0)
       carry = .FALSE.
    CASE (1)
       carry = .TRUE.
    CASE (2:)
       RETURN
    END SELECT
    ! Check for leap year
    IF (MOD(i,4) == 0) THEN
       leapyr = .TRUE.
    ELSE
       leapyr = .FALSE.
    END IF
    !print *,'different year=',carry,' leapyr=',leapyr

    tmps1 = date_in(5:6)
    READ(tmps1,'(I)') i                           ! Convert month string to integer
    tmps2 = curdate(5:6)
    READ(tmps2,'(I)') j                           ! Convert current month string to integer
    ! Carry tells if carry occured in year, if same year then carry = F
    IF (carry .AND. (j == 1) .AND. (i == 12)) THEN
       ! Let carry stay TRUE
    ELSE IF ((.NOT. carry) .AND. (j-i == 1)) THEN
       carry = .TRUE.
    ELSE IF ((.NOT. carry) .AND. (j-i == 0)) THEN
       ! Let carry stay FALSE
    ELSE IF (j-1 < 0) THEN
       CALL printerror('Invalid month value in chk_timeout.',1)
    ELSE
       RETURN
    END IF
    !print *,'different month=',carry

    tmps1 = date_in(7:8)
    READ(tmps1,'(I)') j                           ! Convert day string to integer
    ! Check for last day of the month
    SELECT CASE (i) ! i is the month, 1-12
    CASE (1,3,5,7,8,10,12)
       IF (j == 31) THEN
          last_day_of_month = .TRUE.
       ELSE
          last_day_of_month = .FALSE.
       END IF
    CASE (4,6,9,11)
       IF (j == 30) THEN
          last_day_of_month = .TRUE.
       ELSE
          last_day_of_month = .FALSE.
       END IF
    CASE (2)
       IF (leapyr .AND. (j == 29)) THEN
          last_day_of_month = .TRUE.
       ELSE IF ((.NOT. leapyr) .AND. (j == 28)) THEN
          last_day_of_month = .TRUE.
       ELSE
          last_day_of_month = .FALSE.
       END IF
    END SELECT
    tmps2 = curdate(7:8)
    READ(tmps2,'(I)') i                           ! Convert current day string to integer
    ! Carry tells if carry occured in month, if same month then carry = F
    IF (carry .AND. (i == 1) .AND. last_day_of_month) THEN
       ! Let carry stay TRUE
    ELSE IF ((.NOT. carry) .AND. (i-j == 1)) THEN
       carry = .TRUE.
    ELSE IF ((.NOT. carry) .AND. (i-j == 0)) THEN
       ! Let carry stay FALSE
    ELSE
       RETURN
    END IF
    !print *,'different day=',carry,' last_day_of_month=',last_day_of_month

    tmps1 = time_in(1:2)
    READ(tmps1,'(I)') i                           ! Convert hour string to integer
    tmps1 = time_in(3:4)
    READ(tmps1,'(I)') j                           ! Convert minute string to integer
    minutes = j + (i * 60)

    tmps2 = curtime(1:2)
    READ(tmps2,'(I)') i                           ! Convert current hour string to integer
    tmps2 = curtime(3:4)
    READ(tmps2,'(I)') j                           ! Convert current minute string to integer
    cminutes = j + (i * 60)
    IF (carry) cminutes = cminutes + 1440         ! 24 hours * 60 minutes

    IF (cminutes - minutes < timeout) chk_timeout = .FALSE.
    !print *,'min=',minutes,' cmin=',cminutes,' timeout=',timeout,chk_timeout

  END FUNCTION chk_timeout

  !---------------------------------------------------------------------
  !---------------------------------------------------------------------
END MODULE cinaadmin_core
