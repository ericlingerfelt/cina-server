MODULE io
!DESC = This module contains procedures and data types for general input/output using units.
!
! Procedures in this module are:
!
! io_ver                             Version of this file
! printerror(string,action)          Print error and do action (0=RETURN)
! send_email(message,subject,to,file,error) Send an email
! use_syslog(ident,log_level,message,bin)Log a message into syslog

  USE IFLPORT

! By default all procedures and global variables are public
  PUBLIC

CONTAINS
!---------------------------------------------------------------------
  FUNCTION io_ver
!PURPOSE = Return the cvs revision number for this file
!STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: io_ver
    CHARACTER(LEN=20),PARAMETER   :: IO_VERSION = '$Revision: 1.1.1.1 $'

    io_ver = IO_VERSION(12:LEN_TRIM(IO_VERSION)-2)

  END FUNCTION io_ver

!---------------------------------------------------------------------
  SUBROUTINE print_long_string(string,ADVANCE,unit,IOSTAT)
    IMPLICIT NONE
    INTEGER(KIND=4),PARAMETER     :: BUFFER_LENGTH = 1024
    INTEGER(KIND=4),INTENT(IN)    :: unit
    CHARACTER(LEN=*),INTENT(IN)   :: string,ADVANCE
    INTEGER(KIND=4),INTENT(OUT)   :: IOSTAT
    OPTIONAL                      :: ADVANCE,unit,IOSTAT
    INTEGER(KIND=4)               :: start_index,stop_index,length
    INTEGER(KIND=4)               :: print_unit,status
    LOGICAL(KIND=4)               :: newline

    IF (PRESENT(unit)) THEN
       print_unit = unit
    ELSE
       print_unit = 6
    END IF

    IF (PRESENT(ADVANCE)) THEN
       IF (ADVANCE == 'NO') THEN
          newline = .FALSE.
       ELSE
          newline = .TRUE.
       END IF
    ELSE
       newline = .TRUE.
    END IF

    length = LEN(string)
    start_index = 1

    DO WHILE (start_index < length)
! Set stop_index
       IF (start_index - 1 + BUFFER_LENGTH > length) THEN
          stop_index = length
       ELSE
          stop_index = start_index - 1 + BUFFER_LENGTH
       END IF
       WRITE(print_unit,'(A)',ADVANCE='NO',IOSTAT=status) &
            string(start_index:stop_index)
       IF (status /= 0 .AND. PRESENT(IOSTAT)) THEN
          IOSTAT=status
          RETURN
       END IF

       start_index = stop_index + 1
    END DO

    IF (newline) WRITE(print_unit,'(A)') ''
    IF (PRESENT(IOSTAT)) IOSTAT=status
  END SUBROUTINE print_long_string

!---------------------------------------------------------------------
  SUBROUTINE printerror(str,action)
!PURPOSE = This subroutine provides a standard way to print warnings and errors
!STATUS = Complete and tested
!DESC = To use it, set str to a string to be sent to the user
!DESC = and set action to a number.  The numbers defined are:
!DESC = action = 0    Print error and return to calling procedure
!DESC = action = 1    Print error and and quit program
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)      :: action
    CHARACTER(LEN=*),INTENT(IN)     :: str
    CHARACTER(LEN=10)               :: prefix
    INTEGER(KIND=1)                 :: l

    SELECT CASE (action)
    CASE (0)
       prefix = 'CAUTION: '
       l = 9
    CASE (1)
       prefix = 'ERROR: '
       l = 7
    END SELECT

    WRITE(0,'(A)') prefix(1:l) // str

    SELECT CASE (action)
    CASE (0)
       RETURN
    CASE (1)
       STOP
    END SELECT
  END SUBROUTINE printerror

!---------------------------------------------------------------------
  FUNCTION send_email(message,subject,to,file,error)
!PURPOSE = Send an email
!STATUS = Complete and tested
!DESC = file is pathname where temp file is stored.  error is optional string that holds any error messages
!RETURNS = INT(KIND=4) with 0 if successful, nonzero if an error occurred
!CAUTION = subject, to, and file should not contain single quotes
    USE IFLPORT
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: message,subject,to,file
    CHARACTER(LEN=*),INTENT(OUT)     :: error
    INTEGER(KIND=4)                  :: i,send_email
    OPTIONAL                         :: error

    send_email = -1
    IF (PRESENT(error)) error = ''

! Check input for single quotes
    IF (INDEX(subject,'''') > 0) THEN
       IF (PRESENT(error)) error = 'Subject can not contain a single quote'
       RETURN
    END IF
    IF (INDEX(to,'''') > 0) THEN
       IF (PRESENT(error)) error = 'To can not contain a single quote'
       RETURN
    END IF
    IF (INDEX(file,'''') > 0) THEN
       IF (PRESENT(error)) error = 'File can not contain a single quote'
       RETURN
    END IF

! Create file to serve as email message body
    OPEN(80,FILE=file,ACTION='WRITE',IOSTAT=i)
    IF (i /= 0) THEN
       IF (PRESENT(error)) error = 'Error opening temp file for email'
       RETURN
    END IF

    WRITE(80,'(A)',IOSTAT=i) TRIM(message)
    IF (i /= 0) THEN
       IF (PRESENT(error)) WRITE(error,'(A,I0,A)') 'Error (',i,') writing temp file for email'
       RETURN
    END IF
    CLOSE(80)

!WRITE(*,'(A)') '/bin/cat '''//TRIM(file)//''' | /bin/mail -s '''//TRIM(subject)//''' '''//TRIM(to)//''''
! Send the email
    i = SYSTEM('/bin/cat '''//TRIM(file)//''' | /bin/mail -s '''//TRIM(subject)//''' '''//TRIM(to)//'''')
    IF (i /= 0) THEN
       IF (PRESENT(error)) WRITE(error,'(A,I0)') 'Error sending email: ',i
       RETURN
    END IF

! Erase temp file
    i = DELFILESQQ(file)
    IF (i /= 1) THEN
       IF (PRESENT(error)) WRITE(error,'(A,I0)') 'Error deleting temp file for email: ',i
       RETURN
    END IF

    send_email = 0
  END FUNCTION send_email

!---------------------------------------------------------------------
  FUNCTION use_syslog(ident,log_level,message,bin)
!PURPOSE = Log a message into syslog using the use_syslog program
!STATUS =
    IMPLICIT NONE
    INTEGER(KIND=4),PARAMETER       :: OK=0,QUOTES=1
    CHARACTER(LEN=*),INTENT(IN)     :: bin,ident,log_level,message
    INTEGER(KIND=4)                 :: use_syslog

    use_syslog = OK

! Make sure no inputs have single quotes
    IF ((INDEX(bin,'''') > 0) .OR. (INDEX(ident,'''') > 0) .OR. &
         (INDEX(log_level,'''') > 0) .OR. (INDEX(message,'''') > 0)) THEN
       use_syslog = QUOTES
       RETURN
    END IF

!!$    print *,''''//TRIM(bin)//''' '''//TRIM(ident)//''' '''// &
!!$         TRIM(log_level)//''' '''//TRIM(message)//''''
    use_syslog = SYSTEM(''''//TRIM(bin)//''' '''//TRIM(ident)//''' '''// &
         TRIM(log_level)//''' '''//TRIM(message)//''' &> /dev/null')
    use_syslog = OK

  END FUNCTION use_syslog

!---------------------------------------------------------------------
!---------------------------------------------------------------------
END MODULE io
