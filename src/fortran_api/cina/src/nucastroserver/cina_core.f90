MODULE cina_core
  !DESC = This module contains procedures and data types for common CGI operations for the cina program
  !
  ! Current report_error categories for errors
  !   Improper usage     : improper use of cina, but no damage was done                        : NOTICE
  !   Authencation       : can not authencate user, bad password or too many active sessions,  : ERR
  !                        invalid or expired ID
  !   Improbable         : errors that should never happen under normal circumstances          : CRIT
  !   File input/output  : fortran fileio error followed by a value for IOSTAT                 : ERR
  !   Severe Authencation: one user tried to act as another user                               : CRIT
  !   Pilot system       : affects cina system and all cina sessions - very bad                : ALERT
  !   External fileio    : external file input/output error                                    : ERR
  !                        (rename, delete, search for files using INTEL LIBRARY or SHELL)
  !   Bad file format    : file is in wrong format                                             : ERR
  !   Developer Reminder : error to remind developer to do something                           : NOTICE
  !   External program   : error in inputread, inputcheck, rategen, or getparm                 : ERR
  !   Missing feature    : Feature is not available in this version                            : NOTICE
  !   External log       : Log message sent by Java program                                    : NOTICE
  !   Warning            : Warning message                                                     : NOTICE
  !   Debug              : Debug message                                                       : DEBUG
  !

  ! Use Intel Fortran compiler portability function interface module
  USE IFLPORT
  
  ! By default all procedures and global variables are private
  PRIVATE
  
  PUBLIC   :: cina_core_ver,chk_options,chk_cgi_env,get_content_len
  PUBLIC   :: get_cina_common,cina_encode,cina_decode,print_input
  PUBLIC   :: get_CGI_ID,verify_CGI_ID,report_error,authencate_user
  PUBLIC   :: chk_reaction,get_inv_reaction,CGI_logout,log_error2
  PUBLIC   :: safe_shell,rm_env_var,kill_bg_proc,bg_pid,register
  PUBLIC   :: get_rids,get_prop,get_file,get_data1,get_data2
  
  TYPE,PUBLIC                      :: cina_common
     INTEGER(KIND=1)               :: help = 0
     INTEGER(KIND=1)               :: version = 0
     CHARACTER(LEN=30)             :: IP = ''
     CHARACTER(LEN=5)              :: HEADER = ''
     CHARACTER(LEN=20)             :: ID = ''
     CHARACTER(LEN=50)             :: ACTION = ''
     CHARACTER(LEN=50)             :: USER = ''
     CHARACTER(LEN=64)             :: PW = ''
     INTEGER(KIND=4)               :: SIM_WORKFLOW_RUN_INDEX = -1
     CHARACTER(LEN=8192)           :: BODY = ''
     CHARACTER(LEN=80)             :: REACTION = ''
     CHARACTER(LEN=20)             :: REACTION_TYPE = ''
     CHARACTER(LEN=4096)           :: NOTES = ''
     CHARACTER(LEN=10)             :: TYPE = ''
     CHARACTER(LEN=20)             :: FORMAT = ''
     CHARACTER(LEN=5)              :: WIDTH = ''
     CHARACTER(LEN=5)              :: HEIGHT = ''
     LOGICAL(KIND=1)               :: POSITIVE_CHK = .TRUE.
     LOGICAL(KIND=1)               :: SINGLE_CHK = .TRUE.
     LOGICAL(KIND=1)               :: RANGE_CHK = .TRUE.
     LOGICAL(KIND=1)               :: CONTINUITY_CHK = .TRUE.
     LOGICAL(KIND=1)               :: ERROR_CHK = .TRUE.
     LOGICAL(KIND=1)               :: REACTION_CHK = .TRUE.
     LOGICAL(KIND=1)               :: CHK_TEMP = .FALSE.
     LOGICAL(KIND=1)               :: CHK_OVFL = .FALSE.
     LOGICAL(KIND=1)               :: CHK_INV = .FALSE.
     LOGICAL(KIND=1)               :: DEL = .FALSE.
     LOGICAL(KIND=1)               :: CONFIRM = .FALSE.
     LOGICAL(KIND=1)               :: MK_INV = .FALSE.
     LOGICAL(KIND=1)               :: FILENAME = .TRUE.
     LOGICAL(KIND=1)               :: WEAK = .FALSE.
     LOGICAL(KIND=1)               :: SCREENING = .FALSE.
     REAL(KIND=8)                  :: TMIN = -1D0
     REAL(KIND=8)                  :: TMAX = -1D0
     INTEGER(KIND=4)               :: len_left = 0
     INTEGER(KIND=4)               :: TIMEOUT = -1
     INTEGER(KIND=4)               :: INT = -1
     INTEGER(KIND=4)               :: SESSIONP = 0
  END TYPE cina_common
  ! Change these two lines around depending on which version 
  ! you are making, development or alpha
!DEC$ IF DEV .EQ. 1
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: CINA_NAME='cina_files_dev'
!DEC$ ELSE
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: CINA_NAME='cina_files'
!DEC$ ENDIF
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: CINA_PATH='../'//TRIM(CINA_NAME)//'/'
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: ADM_PATH=TRIM(CINA_PATH)//'adm/'
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: BIN_PATH=TRIM(CINA_PATH)//'bin/'
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: USER_PATH=TRIM(CINA_PATH)//'user/'
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: TEMP_PATH=TRIM(CINA_PATH)//'tmp/'
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: PUB_PATH=TRIM(CINA_PATH)//'public/'
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: SYSLOG_PATH=TRIM(BIN_PATH)//'use_syslog'
  CHARACTER(LEN=60),PARAMETER,PUBLIC :: WINVN_PATH=TRIM(PUB_PATH)//'winvn'
  CHARACTER(LEN=1),PARAMETER,PUBLIC  :: LF = ACHAR(10)
  CHARACTER(LEN=1),PARAMETER,PUBLIC  :: SEP = ACHAR(9)
  CHARACTER(LEN=26),PARAMETER,PUBLIC :: UCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  CHARACTER(LEN=26),PARAMETER,PUBLIC :: LCASE = 'abcdefghijklmnopqrstuvwxyz'
  CHARACTER(LEN=10),PARAMETER,PUBLIC :: NUM = '0123456789'
  CHARACTER(LEN=62),PARAMETER,PUBLIC :: ALPHANUM = UCASE//LCASE//NUM
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: array_len = 10000
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_LOGINS = 10  ! Maximum # of IDs a registered user can get at one time
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_GUEST_LOGINS = 200  ! Maximum # of IDs allowed for the guest account at one time
  INTEGER(KIND=4),PARAMETER,PUBLIC   :: MAX_CONTENT_LEN = 102399
  
  CHARACTER(LEN=32767),PRIVATE       :: file_storage = ''
  CHARACTER(LEN=32767),PRIVATE       :: prop_storage = ''
  CHARACTER(LEN=32767),PRIVATE       :: rids_storage = ''
  CHARACTER(LEN=32767),PRIVATE       :: data1_storage = ''
  CHARACTER(LEN=32767),PRIVATE       :: data2_storage = ''

CONTAINS
  !---------------------------------------------------------------------
  FUNCTION cina_core_ver()
    !PURPOSE = Return the cvs revision number for this file
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)                :: cina_core_ver
    CHARACTER(LEN=20),PARAMETER      :: CINA_C_VERSION = '$Revision: 1.1.1.1 $'
    
    cina_core_ver = CINA_C_VERSION(12:LEN_TRIM(CINA_C_VERSION)-2)
    
  END FUNCTION cina_core_ver
  
  !---------------------------------------------------------------------
  SUBROUTINE chk_options(pcom)
    !PURPOSE = Process any command line options
    !STATUS = Complete and tested
    !DESC = This subroutine does not return if showing help or version info
    USE convert
    USE io
    IMPLICIT NONE
    TYPE(cina_common),INTENT(INOUT) :: pcom
    CHARACTER(LEN=80)                :: tmps,tmps2
    INTEGER(KIND=4)                  :: n,i
    !EXTERNAL                         :: IARGC

    ! Record REMOTE_IP to pcom
    CALL GETENV('REMOTE_ADDR',tmps)
    pcom%IP = tmps
    
    ! Read in the number of command-line arguments
    n = IARGC()
    i = 1
    DO WHILE (i <= n)
       CALL GETARG(i,tmps)
       CALL GETARG(i+1,tmps2)
       SELECT CASE (TRIM(lowercase(tmps)))
       CASE ('-h ','--help ')
          pcom%help = 1
       CASE ('-v ','--version ')
          pcom%version = 1
       CASE DEFAULT
          CALL access_log(pcom,'Unknown option "'//TRIM(tmps)//'"')
          CALL report_error('Unknown option "'//TRIM(tmps)//'"','Improper usage',pcom,1)
       END SELECT
       i = i + 1
    END DO
    
  END SUBROUTINE chk_options
  
  !---------------------------------------------------------------------
  SUBROUTINE chk_cgi_env
    !PURPOSE = Check for correct CGI environment variables
    !STATUS = Complete and tested
    !DESC = This subroutine does not return if a problem if found
    USE convert
    IMPLICIT NONE
    CHARACTER(LEN=80)               :: tmps,error
    TYPE(cina_common)              :: pcom
    
    ! Record REMOTE_IP to pcom
    CALL GETENV('REMOTE_ADDR',tmps)
    pcom%IP = tmps
    
    error = ''
    
    CALL GETENV('REQUEST_METHOD',tmps)
    IF (LEN_TRIM(tmps) == 0) THEN
       ! Log cgi request
       CALL access_log(pcom,'Could not find required CGI environment variables.')
       CALL report_error('This program should be executed by the webserver not the command line.' &
            //LF//'Could not find required CGI environment variables.','Improper usage',pcom,1)
    END IF
    
    ! Make sure REQUEST_METHOD is POST
    IF (lowercase(tmps) /= 'post') error = 'Invalid REQUEST_METHOD '//TRIM(tmps)
    
    CALL GETENV('CONTENT_TYPE',tmps)
    ! Make sure CONTENT_TYPE is application/x-www-form-urlencoded
    IF (tmps /= 'application/x-www-form-urlencoded') error = 'Invalid CONTENT_TYPE '//TRIM(tmps)
    
    ! Make sure CONTENT_LENGTH is valid
    CALL GETENV('CONTENT_LENGTH',tmps)
    IF (get_content_len() < 0) error = 'Invalid CONTENT_LENGTH '//TRIM(tmps)
    IF (get_content_len() > MAX_CONTENT_LEN) error = 'CONTENT_LENGTH ('//TRIM(tmps)//') is too high'
    
    ! CONTENT_LENGTH an error occured, return it to the webserver
    IF (LEN_TRIM(error) > 0) THEN
       ! Log cgi request
       CALL access_log(pcom,TRIM(error))
       CALL report_error(error,'Improper usage',pcom,1)
    END IF
  END SUBROUTINE chk_cgi_env
  
  !---------------------------------------------------------------------
  FUNCTION get_content_len
    !PURPOSE = Return the CONTENT_LENGTH as an integer
    !STATUS = Complete and tested
    !RETURNS = INTEGER(KIND=4)
    !DESC = The return value is < 0 if CONTENT_LENGTH was not found or was not a number
    IMPLICIT NONE
    CHARACTER(LEN=10),PARAMETER   :: digit = '0123456789'
    INTEGER(KIND=4)               :: get_content_len,i
    CHARACTER(LEN=30)             :: str
    
    CALL GETENV('CONTENT_LENGTH',str)
    ! Make sure CONTENT_LENGTH contains only digits
    IF (VERIFY(TRIM(str),digit) > 0) THEN
       get_content_len = -1
       RETURN
    END IF
    
    get_content_len = 0
    DO i = 1, LEN_TRIM(str)
       get_content_len = get_content_len * 10 + IACHAR(str(i:i)) - 48
    END DO
  END FUNCTION get_content_len
  
  !---------------------------------------------------------------------
  FUNCTION get_cina_common
    !PURPOSE = Parse CGI input until FILE is found
    !STATUS = Complete and tested
    !RETURNS = cina_common derived type variable
    !DESC = Missing information is returned as '' or .TRUE.
    !DESC = After reading from stdin, the record is set to point to the contents of the file 
    USE convert
    IMPLICIT NONE
    CHARACTER(LEN=22),PARAMETER   :: HEX_CHR = '0123456789ABCDEFabcdef'
    TYPE(cina_common)            :: get_cina_common
    LOGICAL(KIND=1)               :: n,v,e,binary_file = .FALSE.
    CHARACTER(LEN=80)             :: name,tmps
    CHARACTER(LEN=32767)          :: value
    CHARACTER(LEN=1)              :: c
    CHARACTER(LEN=2)              :: h
    CHARACTER(LEN=MAX_CONTENT_LEN):: buffer
    INTEGER(KIND=4)               :: ci,i,j,k,l
    REAL(KIND=8)                  :: tmpr

    ! Record REMOTE_IP to get_cina_common
    CALL GETENV('REMOTE_ADDR',name)
    get_cina_common%IP = name
    
    ! c is index into stdin (the number of characters read in so far)
    ci = 0;
    buffer = ''
    
    ! l is the CONTENT_LENGTH
    l = get_content_len()
    
    IF (l == 0) THEN
       ! Log cgi request
       WRITE(*,*) 'Empty request'
       CALL access_log(get_cina_common,'')
       CALL report_error('Empty server request was sent','Improper usage',get_cina_common,1)
    END IF

!!$    DO ci = 1, l/6
!!$       i = GETC(c)
!!$       buffer(ci:ci) = c
!!$       i = PUTC(c)
!!$    END DO
!!$    STOP
    DO WHILE (ci < l)
       ! Read in name (part before the equal sign)
       n = .TRUE.     ! Becomes FALSE when the equal sign is found
       j = 0          ! Counts the number of characters in name
       e = .FALSE.    ! Becomes TRUE when a value was truncated
       name = ''
       DO WHILE (n .AND. (ci < l))
          ! Read in next character and return if there is an error
          READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') c
          IF (i == -2) THEN
             !WRITE(*,'(A,I0,3A)') '_i=-2 (1), ci=',ci,', c="',c,'"'
             READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') c
             IF (i == -2) THEN
                !WRITE(*,'(A,I0,3A)') '_i=-2 (2), ci=',ci,', c="',c,'"'
                c = ''
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Error reading CGI input (1)','Improbable',get_cina_common,1)
             END IF
          END IF
          ci = ci + 1
          get_cina_common%len_left = l - ci
          buffer(ci:ci) = c
          IF (i /= 0) THEN
             CALL access_log(get_cina_common,buffer)
             CALL report_error('Error reading from stdin in get_cina_common (1)','Improbable',get_cina_common,1)
          END IF
          IF (c == '=') THEN
             n = .FALSE.
          ELSEIF (j < LEN(name)) THEN
             ! Make sure the character is okay to add to name
             IF ((IACHAR(c) >= 32) .AND. (IACHAR(c) <= 126)) THEN
                j = j + 1
                name(j:j) = c
             END IF
          END IF
       END DO
       
       
       ! Read in value (part after the equal sign and before "&")
       v = .TRUE.     ! Becomes FALSE when "&" is found
       j = 0          ! Counts the number of characters in value
       value = ''
       DO WHILE (v .AND. (ci < l))
          ! Read in next character and return if there is an error
          c = ''
          READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') c
          IF (i == -2) THEN
             !WRITE(*,'(A,I0,3A)') 'i=-2 (1), ci=',ci,', c="',c,'"'
             READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') c
             IF (i == -2) THEN
                !WRITE(*,'(A,I0,3A)') 'i=-2 (2), ci=',ci,', c="',c,'"'
                c = ''
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Error reading CGI input (2)','Improbable',get_cina_common,1)
             END IF
          END IF
          ci = ci + 1
          get_cina_common%len_left = l - ci
          buffer(ci:ci) = c
          IF (i /= 0) THEN
             WRITE(*,'(A,3I)') 'ERROR=',i,ci,l
             CALL access_log(get_cina_common,buffer)
             CALL report_error('Error reading from stdin in get_cina_common (2)','Improbable',get_cina_common,1)
          END IF
          IF (i == 0) THEN
             IF ((IACHAR(c) >= 32) .AND. (IACHAR(c) <= 126)) THEN
                IF (c == '&') THEN
                   v = .FALSE.
                ELSEIF (c == '+') THEN
                   j = j + 1
                   value(j:j) = ' '
                ELSEIF (c == '%') THEN
                   ! Decode string like "%0A" to a linefeed character
                   h = ''
                   ! Read in h character by character
                   READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') h(1:1)
                   IF (i == -2) THEN
                      !WRITE(*,'(A,I0,3A)') 'i=-2 (1), ci=',ci,', (1)h="',h,'"'
                      READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') h(1:1)
                      IF (i == -2) THEN
                         !WRITE(*,'(A,I0,3A)') 'i=-2 (2), ci=',ci,', (1)h="',h,'"'
                         h = ''
                         CALL access_log(get_cina_common,buffer)
                         CALL report_error('Error reading CGI input (3)','Improbable',get_cina_common,1)
                      END IF
                   END IF
                   READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') h(2:2)
                   IF (i == -2) THEN
                      !WRITE(*,'(A,I0,3A)') 'i=-2 (1), ci=',ci,', (2)h="',h,'"'
                      READ(*,'(A1)',IOSTAT=i,ADVANCE='NO') h(2:2)
                      IF (i == -2) THEN
                         !WRITE(*,'(A,I0,3A)') 'i=-2 (2), ci=',ci,', (2)h="',h,'"'
                         h = ''
                         CALL access_log(get_cina_common,buffer)
                         CALL report_error('Error reading CGI input (4)','Improbable',get_cina_common,1)
                      END IF
                   END IF
                   ci = ci + 2
                   get_cina_common%len_left = l - ci
                   IF (i == -2) h = ''
                   buffer(ci-1:ci) = h
                   IF ((i /= 0) .AND. (i /= -2)) THEN
                      WRITE(*,'(A,3I)') 'ERROR=',i,ci,l
                      CALL access_log(get_cina_common,buffer)
                      CALL report_error('Error reading from stdin in get_cina_common (3)','Improbable',get_cina_common,1)
                   END IF
                   IF (i == 0) THEN
                      ! Check that character is a HEX value
                      IF (VERIFY(h,HEX_CHR) > 0) THEN
                         ! This should never happen, but if so 
                         ! ignore the 2 characters in h (otherwise, check to see what they are)
                         j = j + 1
                         value(j:j) = '%'
                         !WRITE(*,'(A,I0,6A)') 'CAUTION=Invalid hex characters at ci=',ci,' h="',h,'"', &
                         !   ' buffer="',buffer(ci-18:ci),'"'
                         CALL report_error('Server input is formatted incorrectly','Improbable',get_cina_common,0)
                      ELSE
                         k = 32     ! Default character (in case of an error below)
                         READ(h,'(Z2)',IOSTAT=i) k
                         IF (((k >= 32) .AND. (k <= 126)) .OR. ((k >= 9) .AND. (k <= 11)) &
                              .OR. (k == 13)) THEN
                            j = j + 1
                            value(j:j) = ACHAR(k)
                         ELSE IF (name == 'FILE') THEN
                            !print '(A,I0)','CAUTION=',k
                            binary_file = .TRUE.
                         END IF
                      END IF
                   END IF
                ELSEIF (j < LEN(value)) THEN
                   ! Make sure the character is okay to add to value
                   !IF ((IACHAR(c) >= 32) .AND. (IACHAR(c) <= 126)) THEN
                   j = j + 1
                   value(j:j) = c
                   !END IF
                END IF
                ! Check if value is full
                IF (v .AND. (j >= LEN(value))) e = .TRUE.
             END IF
          END IF
       END DO

       IF (e) CALL report_error('The '//TRIM(name)//' is too large and has been truncated.','Warning',get_cina_common,0)
       SELECT CASE (name)
       CASE ('HEADER ')
          get_cina_common%HEADER = value
       CASE ('ID ')
          get_cina_common%ID = value
       CASE ('ACTION ')
          get_cina_common%ACTION = value
       CASE ('USER ')
          get_cina_common%USER = value
       CASE ('PW ')
          get_cina_common%PW = value
       CASE ('SIM_WORKFLOW_RUN_INDEX ')
          READ(value(1:10),'(I)') get_cina_common%SIM_WORKFLOW_RUN_INDEX
       CASE ('WIDTH ')
          get_cina_common%WIDTH = value
       CASE ('HEIGHT ')
          get_cina_common%HEIGHT = value
       CASE ('BODY ')
          ! Only store something in BODY for certain actions
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GENERATE PARAMETER FORMAT ', 'LOG ', 'DEBUG ')
             get_cina_common%BODY = value
          END SELECT
       CASE ('REACTION ')
          get_cina_common%REACTION = value
       CASE ('REACTION_TYPE ')
          get_cina_common%REACTION_TYPE = value
       CASE ('NOTES ')
          get_cina_common%NOTES = value
       CASE ('TYPE ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET RATE LIST ','GET NUC DATA LIST')
             get_cina_common%REACTION = value
          CASE DEFAULT
             get_cina_common%TYPE = value
          END SELECT
       CASE ('FORMAT ')
          get_cina_common%FORMAT = value
       CASE ('FILENAME ')                       ! This is only for zeroth order
          IF (value /= '') THEN
             file_storage = value
             get_cina_common%FILENAME = .TRUE.
             !PRINT '(A)','ERROR=FILENAME='//TRIM(value)
          END IF
       CASE ('FILE ')
          IF (value == '') THEN
             get_cina_common%FILENAME = .TRUE.
             !PRINT '(A)','CAUTION=Blank FILE'
          ELSE
             !PRINT '(A)','CAUTION=FILE='//TRIM(value)
             file_storage = value
             !WRITE(*,'(2A)') 'ERROR=end=',file_storage(j-30:j)
             !WRITE(*,'(3(A,I0))') 'ERROR=len=',LEN_TRIM(file_storage),', j=',j,', l=',l
             get_cina_common%FILENAME = .FALSE.
          END IF
       CASE ('POSITIVE_CHK ')
          IF ((value(1:1) == 'N') .OR. (value(1:1) == 'n')) get_cina_common%POSITIVE_CHK = .FALSE.
       CASE ('SINGLE_CHK ')
          IF ((value(1:1) == 'N') .OR. (value(1:1) == 'n')) get_cina_common%SINGLE_CHK = .FALSE.
       CASE ('RANGE_CHK ')
          IF ((value(1:1) == 'N') .OR. (value(1:1) == 'n')) get_cina_common%RANGE_CHK = .FALSE.
       CASE ('CONTINUITY_CHK ')
          IF ((value(1:1) == 'N') .OR. (value(1:1) == 'n')) get_cina_common%CONTINUITY_CHK = .FALSE.
       CASE ('ERROR_CHK ')
          IF ((value(1:1) == 'N') .OR. (value(1:1) == 'n')) get_cina_common%ERROR_CHK = .FALSE.
       CASE ('REACTION_CHK ')
          IF ((value(1:1) == 'N') .OR. (value(1:1) == 'n')) get_cina_common%REACTION_CHK = .FALSE.
       CASE ('XUNITS ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('READ INPUT','PARSE NUC DATA FILE')
             XUC1: SELECT CASE (get_cina_common%TYPE)
             CASE ('S(E) ', 'CS(E) ') XUC1
                XUC2: SELECT CASE (value)
                CASE ('EV ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -eu eV'
                CASE ('KEV ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -eu KeV'
                CASE ('MEV ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -eu MeV'
                CASE ('GEV ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -eu GeV'
                CASE DEFAULT
                   CALL access_log(get_cina_common,buffer)
                   CALL report_error('Unknown XUNIT '//TRIM(value),'Improper usage',get_cina_common,1)
                END SELECT XUC2
             CASE ('R(T) ')
                XUC3: SELECT CASE (value)
                CASE ('T0 ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -tu T0'
                CASE ('T3 ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -tu T3'
                CASE ('T6 ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -tu T6'
                CASE ('T9 ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -tu T9'
                CASE DEFAULT
                   CALL access_log(get_cina_common,buffer)
                   CALL report_error('Unknown XUNIT '//TRIM(value),'Improper usage',get_cina_common,1)
                END SELECT XUC3
             END SELECT XUC1
          END SELECT
       CASE ('YUNITS ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('READ INPUT','PARSE NUC DATA FILE')
             YUC1: SELECT CASE (get_cina_common%TYPE)
             CASE ('S(E) ')
                YUC2: SELECT CASE (value)
                CASE ('EV-B ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -su eV-b'
                CASE ('KEV-B ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -su KeV-b'
                CASE ('MEV-B ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -su MeV-b'
                CASE ('GEV-B ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -su GeV-b'
                CASE DEFAULT
                   CALL access_log(get_cina_common,buffer)
                   CALL report_error('Unknown YUNIT '//TRIM(value),'Improper usage',get_cina_common,1)
                END SELECT YUC2
             CASE ('CS(E) ')
                YUC3: SELECT CASE (value)
                CASE ('b ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -csu b'
                CASE ('mb ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -csu mb'
                CASE ('ub ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -csu ub'
                CASE ('nb ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -csu nb'
                CASE ('pb ')
                   get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -csu pb'
                CASE DEFAULT
                   CALL access_log(get_cina_common,buffer)
                   CALL report_error('Unknown YUNIT '//TRIM(value),'Improper usage',get_cina_common,1)
                END SELECT YUC3
             CASE ('R(T) ')
                ! Do nothing for now
             END SELECT YUC1
          END SELECT
       CASE ('PLEVEL ')
          PLC1: SELECT CASE (get_cina_common%ACTION)
          CASE ('GENERATE RATE ', 'PARAMETERIZE RATE ')
             IF (VERIFY(value(1:1),'1234') == 0) THEN
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -p'//value(1:1)
             ELSE
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Invalid print level: '//value(1:1),'Improper usage',get_cina_common,1)
             END IF
          END SELECT PLC1
       CASE ('TMIN ')
          TMIN1: SELECT CASE (get_cina_common%ACTION)
          CASE ('GENERATE RATE ','DEBUG ')
             j = 1   ! Start looking in character position 1 in tmps2 for a number
             CALL nextnum(value,tmpr,j,i)
             TMIN2: SELECT CASE (i)
             CASE (0)
                ! No errors
                get_cina_common%TMIN = tmpr
             CASE (1)
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Could not represent "'//TRIM(value)//'" in TMIN', &
                     'Improper usage',get_cina_common,1)
             CASE (2)
                WRITE(value,'(G)') get_cina_common%TMIN
                CALL access_log(get_cina_common,buffer)
                CALL report_error('TMIN was truncated to '//TRIM(value),'Warning',get_cina_common,0)
             CASE DEFAULT
                WRITE(value,'(I0)') i
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Unknown status '//TRIM(value)//'reading TMIN ', &
                     'Developer Reminder',get_cina_common,0)
             END SELECT TMIN2
          END SELECT TMIN1
       CASE ('TMAX ')
          TMAX1: SELECT CASE (get_cina_common%ACTION)
          CASE ('GENERATE RATE ')
             j = 1   ! Start looking in character position 1 in tmps2 for a number
             CALL nextnum(value,tmpr,j,i)
             TMAX2: SELECT CASE (i)
             CASE (0)
                ! No errors
                get_cina_common%TMAX = tmpr
             CASE (1)
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Could not represent "'//TRIM(value)//'" in TMAX', &
                     'Improper usage',get_cina_common,1)
             CASE (2)
                WRITE(value,'(G)') get_cina_common%TMAX
                CALL access_log(get_cina_common,buffer)
                CALL report_error('TMAX was truncated to '//TRIM(value),'Warning',get_cina_common,0)
             CASE DEFAULT
                WRITE(value,'(I0)') i
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Unknown status '//TRIM(value)//'reading TMAX ', &
                     'Developer Reminder',get_cina_common,0)
             END SELECT TMAX2
          END SELECT TMAX1
       CASE ('METHOD ')
          MTH1: SELECT CASE (get_cina_common%ACTION)
          CASE ('PARAMETERIZE RATE')
             MTH2: SELECT CASE (value)
             CASE ('PARALLEL MARQUARDT ')
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -l'
             CASE ('MARQUARDT ')
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -l -p'
             CASE DEFAULT
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Invalid parameterization method: '//TRIM(value),  &
                     'Improper usage',get_cina_common,0)
             END SELECT MTH2
          END SELECT MTH1
       CASE ('ITERATIONS ')
          ITR1: SELECT CASE (get_cina_common%ACTION)
          CASE ('PARAMETERIZE RATE ')
             i = VERIFY(value,NUM)
             IF (i == 0) i = LEN_TRIM(value) + 1
             IF (i > 1) THEN
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -i '//value(1:i-1)
             END IF
          END SELECT ITR1
       CASE ('S_PARM_NUM ')
          SPN1: SELECT CASE (get_cina_common%ACTION)
          CASE ('PARAMETERIZE RATE ')
             SPN2: SELECT CASE (value)
             CASE ('7')
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -b 1'
             CASE ('14')
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -b 2'
             CASE ('21')
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -b 3'
             CASE ('28')
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -b 4'
             CASE DEFAULT
                CALL access_log(get_cina_common,buffer)
                CALL report_error('Invalid number of starting parameters: '//TRIM(value),  &
                     'Improper usage',get_cina_common,0)
             END SELECT SPN2
          END SELECT SPN1
       CASE ('PARM_CHECK ')
          PCK1: SELECT CASE (get_cina_common%ACTION)
          CASE ('PARAMETERIZE RATE ')
             IF (value(1:1) == 'N') THEN
                ! getparm does not support parameter checking yet
                !get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -p'
             END IF
          END SELECT PCK1
       CASE ('MAX_DIFF ')
          MDF1: SELECT CASE (get_cina_common%ACTION)
          CASE ('PARAMETERIZE RATE ')
             i = VERIFY(value,NUM//'. -eEdD+')
             IF (i == 0) i = LEN_TRIM(value) + 1
             IF (i > 1) THEN
                get_cina_common%BODY = TRIM(get_cina_common%BODY)//' -e '//value(1:i-1)
             END IF
          END SELECT MDF1
       CASE ('GROUP ')
          GRP1: SELECT CASE (get_cina_common%ACTION)
          CASE ('GET RATE LIBRARY LIST ','GET NUC DATA SET LIST')
             get_cina_common%BODY = value
          END SELECT GRP1
       CASE ('LIBRARY ')
          LIB1: SELECT CASE (get_cina_common%ACTION)
          CASE ('GET RATE LIBRARY INFO ','GET RATE LIBRARY ISOTOPES ','GET RATE LIST ', &
               'ADD MISSING INV RATES ','SHARE RATE LIBRARY ','ELEMENT SYNTHESIS SETUP ', &
               'EXPORT RATE LIBRARY')
             get_cina_common%BODY = value
          END SELECT LIB1
       CASE ('ISOTOPE ')
          ISO1: SELECT CASE (get_cina_common%ACTION)
          CASE ('GET RATE LIST ','GET NUC DATA LIST')
             get_cina_common%NOTES = value
          END SELECT ISO1
       CASE ('RATES ')
          rids_storage = value
       CASE ('PROPERTIES ')
          prop_storage = value
       CASE ('DEST_LIB ')
          DST1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATES ')
             get_cina_common%REACTION = value
          CASE ('MODIFY RATE LIBRARY ')
             get_cina_common%BODY = value
          END SELECT DST1
       CASE ('CHK_TEMP_BEHAVIOR ')
          CTB1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATES ','MODIFY RATE LIBRARY ')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%CHK_TEMP = .TRUE.
          END SELECT CTB1
       CASE ('CHK_OVERFLOW ')
          COV1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATES ','MODIFY RATE LIBRARY ','MODIFY NUC DATA','MODIFY NUC DATA SET')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%CHK_OVFL = .TRUE.
          END SELECT COV1
       CASE ('CHK_INVERSE ')
          CIN1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATES ','MODIFY RATE LIBRARY ')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%CHK_INV = .TRUE.
          END SELECT CIN1
       CASE ('DEL_SRC_RATE ')
          DSR1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATES ')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%DEL = .TRUE.
          END SELECT DSR1
       CASE ('DEL_SRC_LIB ')
          DSL1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATE LIBRARY ')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%DEL = .TRUE.
          END SELECT DSL1
       CASE ('CONFIRM ')
          CFM1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATES ','MODIFY RATE LIBRARY ')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%CONFIRM = .TRUE.
          END SELECT CFM1
       CASE ('MAKE_INVERSE ')
          MIV1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATES ','MODIFY RATE LIBRARY ')
             IF ((value(1:1) == 'N') .OR. (value(1:1) == 'n')) THEN
                get_cina_common%MK_INV = .FALSE.
             ELSE
                get_cina_common%MK_INV = .TRUE.
             END IF
          END SELECT MIV1
       CASE ('DEST_GROUP ')
          DGP1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATE LIBRARY ','MODIFY NUC DATA SET')
             get_cina_common%REACTION = value
          END SELECT DGP1
       CASE ('SRC_LIB ')
          SCL1: SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY RATE LIBRARY ')
             get_cina_common%NOTES = value
          END SELECT SCL1
       CASE ('FIRST_NAME ')
          FNM1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             get_cina_common%NOTES = value
          END SELECT FNM1
       CASE ('LAST_NAME ')
          LNM1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             get_cina_common%BODY = value
          END SELECT LNM1
       CASE ('EMAIL ')
          EML1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             get_cina_common%REACTION = value
          END SELECT EML1
       CASE ('INSTITUTION ')
          IST1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             file_storage = value
          END SELECT IST1
       CASE ('ADDRESS ')
          ADR1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             rids_storage = value
          END SELECT ADR1
       CASE ('RESEARCH ')
          RSH1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             prop_storage = value
          END SELECT RSH1
       CASE ('HEAR_OF_SUITE ')
          HST1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             data1_storage = value
          END SELECT HST1
       CASE ('INFO ')
          INF1: SELECT CASE (get_cina_common%ACTION)
          CASE ('REGISTER ')
             data2_storage = value
          END SELECT INF1
       CASE ('PATH ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET ELEMENT SYNTHESIS TIME MAPPING ','GET ELEMENT SYNTHESIS ISOTOPE MAPPING ', &
               'GET ELEMENT SYNTHESIS THERMO PROFILE ', 'GET ELEMENT SYNTHESIS ABUNDANCES ', &
               'SAVE ELEMENT SYNTHESIS RUN ','GET ELEMENT SYNTHESIS FLUX MAPPING ', &
               'GET ELEMENT SYNTHESIS FLUXES', 'GET ELEMENT SYNTHESIS WEIGHTED ABUNDANCES')
             get_cina_common%BODY = value
          END SELECT
       CASE ('ZONES ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET ELEMENT SYNTHESIS TIME MAPPING ', 'GET ELEMENT SYNTHESIS THERMO PROFILE ')
             get_cina_common%REACTION = value
          CASE ('SYNTHESIZE ELEMENTS ')
             get_cina_common%BODY = value
          END SELECT
       CASE ('ZONE ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET ELEMENT SYNTHESIS ABUNDANCES ','GET ELEMENT SYNTHESIS FLUXES ')
             get_cina_common%REACTION = value
          END SELECT
       CASE ('REACTIONS ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET ELEMENT SYNTHESIS FLUXES ')
             get_cina_common%NOTES = value
          END SELECT
       CASE ('ISOTOPES ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET ELEMENT SYNTHESIS ABUNDANCES ', 'GET ELEMENT SYNTHESIS WEIGHTED ABUNDANCES')
             get_cina_common%NOTES = value
          END SELECT
       CASE ('MIN_ISOTOPE ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('ELEMENT SYNTHESIS SETUP ')
             get_cina_common%REACTION = value
          END SELECT
       CASE ('MAX_ISOTOPE ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('ELEMENT SYNTHESIS SETUP ')
             get_cina_common%NOTES = value
          END SELECT
       CASE ('NAME ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SAVE ELEMENT SYNTHESIS RUN ')
             get_cina_common%REACTION = value
          END SELECT
       CASE ('OVERWRITE ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SAVE ELEMENT SYNTHESIS RUN ')
             IF (value(1:3) == 'YES') get_cina_common%DEL = .TRUE.
          END SELECT
       CASE ('INIT_ABUND_PATH ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SYNTHESIZE ELEMENTS ')
             data1_storage = value
          END SELECT
       CASE ('THERMO_PATH ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SYNTHESIZE ELEMENTS ')
             data2_storage = value
          END SELECT
       CASE ('MAX_TIMESTEPS ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SYNTHESIZE ELEMENTS ')
             READ(value,'(I)',IOSTAT=i) get_cina_common%INT
             IF (i /= 0) THEN
                WRITE(tmps,'(A,I0)') 'Error ',i
                CALL access_log(get_cina_common,buffer)
                CALL report_error(TRIM(tmps)//' interpreting "'//TRIM(value)//'" as an integer','',get_cina_common,1)
             END IF
          END SELECT
       CASE ('INCLUDE_WEAK ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SYNTHESIZE ELEMENTS ')
             IF (value == 'Y') get_cina_common%WEAK = .TRUE.
          END SELECT
       CASE ('INCLUDE_SCREENING ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SYNTHESIZE ELEMENTS ')
             IF (value == 'Y') get_cina_common%SCREENING = .TRUE.
          END SELECT
       CASE ('START_TIME ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SYNTHESIZE ELEMENTS ')
             READ(value,'(G)',IOSTAT=i) get_cina_common%TMIN
             IF (i /= 0) THEN
                WRITE(tmps,'(A,I0)') 'Error ',i
                CALL access_log(get_cina_common,buffer)
                CALL report_error(TRIM(tmps)//' interpreting "'//TRIM(value)//'" as a number','',get_cina_common,1)
             END IF
          END SELECT
       CASE ('STOP_TIME ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('SYNTHESIZE ELEMENTS ')
             READ(value,'(G)',IOSTAT=i) get_cina_common%TMAX
             IF (i /= 0) THEN
                WRITE(tmps,'(A,I0)') 'Error ',i
                CALL access_log(get_cina_common,buffer)
                CALL report_error(TRIM(tmps)//' interpreting "'//TRIM(value)//'" as a number','',get_cina_common,1)
             END IF
          END SELECT
       CASE ('SET')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET NUC DATA SET ISOTOPES','GET NUC DATA LIST','GET NUC DATA SET INFO', &
               'SHARE NUC DATA SET')
             get_cina_common%BODY = value
          END SELECT
       CASE('NUC_DATA')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET NUC DATA INFO','NUC DATA EXIST','MODIFY NUC DATA')
             rids_storage = value
          END SELECT
       CASE ('DEL_SRC_SET ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY NUC DATA SET ')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%DEL = .TRUE.
          END SELECT
       CASE ('DEL_NUC_DATA ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY NUC DATA')
             IF ((value(1:1) == 'Y') .OR. (value(1:1) == 'y')) get_cina_common%DEL = .TRUE.
          END SELECT
       CASE ('SRC_SET ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY NUC DATA SET ')
             get_cina_common%NOTES = value
          END SELECT 
       CASE ('DEST_SET ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MODIFY NUC DATA ')
             get_cina_common%REACTION = value
          CASE ('MODIFY NUC DATA SET')
             get_cina_common%BODY = value
          END SELECT
       CASE ('SUNET_PATH ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('ELEMENT SYNTHESIS SETUP ')
             file_storage = value
          END SELECT
       CASE ('ARGUMENTS ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MAKE ELEMENT SYNTHESIS MOVIE')
             get_cina_common%NOTES = value
          END SELECT
       CASE ('SIMULATION ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MAKE ELEMENT SYNTHESIS MOVIE')
             get_cina_common%BODY = value
          END SELECT
       CASE ('MOVIE_FPS ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MAKE ELEMENT SYNTHESIS MOVIE')
             i = 1
             CALL nextnum(value,get_cina_common%TMIN,i,j)
             !READ(value,'(G)',IOSTAT=i) get_cina_common%TMIN
             IF (i == 0) THEN
                WRITE(tmps,'(A,I0)') 'Error ',i
                CALL access_log(get_cina_common,buffer)
                CALL report_error(TRIM(tmps)//' interpreting "'//TRIM(value)//'" as a number','',get_cina_common,1)
             END IF
          END SELECT
       CASE ('FRAME_SKIP_INTERVAL ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MAKE ELEMENT SYNTHESIS MOVIE')
             READ(value,'(I)',IOSTAT=i) get_cina_common%INT
             IF (i /= 0) THEN
                WRITE(tmps,'(A,I0)') 'Error ',i
                CALL access_log(get_cina_common,buffer)
                CALL report_error(TRIM(tmps)//' interpreting "'//TRIM(value)//'" as an integer','',get_cina_common,1)
             END IF
          END SELECT
       CASE ('QUALITY ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('MAKE ELEMENT SYNTHESIS MOVIE')
             get_cina_common%REACTION = value
          END SELECT
       CASE ('SUM ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET ELEMENT SYNTHESIS FLUXES ')
             get_cina_common%FORMAT = value
          END SELECT
       CASE ('FINAL_STEP ')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('GET ELEMENT SYNTHESIS ABUNDANCES ')
             get_cina_common%FORMAT = value
          END SELECT
       CASE ('START_PARMS')
          SELECT CASE (get_cina_common%ACTION)
          CASE ('PARAMETERIZE RATE')
             get_cina_common%NOTES = value
          END SELECT
       END SELECT
       
    END DO

    CALL access_log(get_cina_common,buffer)

    ! Remove any characters that compromise security
    get_cina_common = filter_cina_common(get_cina_common)
    
  END FUNCTION get_cina_common
  
  !---------------------------------------------------------------------
  SUBROUTINE access_log(pcom,message)
    !PURPOSE = Log a message in the access log
    !STATUS = Complete and tested
    !CAUTION = This procedure does not return if an error occurred
    USE convert
    USE io
!    USE CINA
    IMPLICIT NONE
    TYPE(cina_common),INTENT(INOUT) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: message
    CHARACTER(LEN=200)            :: header
    CHARACTER(LEN=32765)          :: msg_line
    INTEGER(KIND=4)               :: i,n

    IF (pcom%ACTION == 'GET ID') THEN
       header = ''
       CALL new_session(pcom%USER,pcom%PW,pcom%ACTION,message,pcom%ID,pcom%SESSIONP,msg_line)
       IF (msg_line /= '') CALL log_error2(pcom%SESSIONP,'Username or password is invalid','Authencation',0,1,msg_line, &
            '$Id: cina_core.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $',1)
       WRITE(*,'(2A)') 'ID=',pcom%ID
       STOP
    ELSE
       CALL valid_session(pcom%USER,pcom%PW,pcom%ACTION,message,pcom%ID,pcom%SESSIONP,msg_line)
       IF (msg_line /= '') THEN
          IF (pcom%ACTION == 'LOGOUT') THEN
             WRITE(*,'(A)') 'LOGOUT=SUCCESS'
             STOP
          END IF
          IF (pcom%ACTION == 'MODIFY RATE LIBRARY' .OR. pcom%ACTION == 'GET RATE INFO' .OR. pcom%ACTION == 'MODIFY RATES' &
                .OR. pcom%ACTION == 'ELEMENT SYNTHESIS SETUP'.OR. pcom%ACTION == 'SYNTHESIZE ELEMENTS' .OR. pcom%ACTION == 'ELEMENT SYNTHESIS UPDATE') THEN
          ELSE
             CALL log_error2(pcom%SESSIONP,'Username or password is invalid','Authencation',0,1,msg_line, &
               '$Id: cina_core.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $',1)
          END IF
          
       END IF
       pcom%TIMEOUT = 180
    END IF
       
    ! Create message header
    WRITE(header,'(8A)') TRIM(str2line(pcom%USER)),'(',pcom%ID,',',TRIM(pcom%IP),') Access|', &
         TRIM(str2line(TRIM(pcom%ACTION))),'|'

    msg_line = str2line(TRIM(message))

    ! Log the message
    i = use_syslog(TRIM(CINA_NAME),'INFO',TRIM(header)//TRIM(msg_line),TRIM(SYSLOG_PATH))

    IF (i /= 0) THEN
       WRITE(header,'(I0)') i
       CALL report_error('Could not write to access log because of error '//TRIM(header), &
            'External program',pcom,1)
    END IF

  END SUBROUTINE access_log

  !---------------------------------------------------------------------
  FUNCTION filter_cina_common(in)
    !PURPOSE = Remove any characters that compromise security
    !STATUS = Complete and tested
    !DESC = BODY and NOTES are encoded.  Use cina_decode to retrieve string
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: in
    TYPE(cina_common)            :: filter_cina_common
    INTEGER(KIND=4)               :: i
    
    filter_cina_common = in

    ! Filter HEADER to only allow numbers or the period (not even spaces)
    i = VERIFY(TRIM(in%HEADER),NUM//'.')
    IF (i /= 0) CALL report_error('Invalid header: '//TRIM(in%HEADER), &
         'Improper usage',in,1)
    
    ! Filter ID to only allow alphanumeric characters
    i = VERIFY(TRIM(in%ID),ALPHANUM)
    IF (i /= 0) CALL report_error('Invalid ID: '//TRIM(in%ID), &
         'Improper usage',in,1)
    
    ! Filter ACTION to only allow alphanumeric characters and space
    i = VERIFY(TRIM(in%ACTION),ALPHANUM//' ')
    IF (i /= 0) CALL report_error('Invalid ACTION: '//TRIM(in%ACTION), &
         'Improper usage',in,1)
    
    ! Filter REACTION to only allow for Alphanumeric characters or the comma or space
    SELECT CASE (in%ACTION)
    CASE ('MODIFY RATES ')
       i = VERIFY(in%REACTION,ALPHANUM//'_.- '//ACHAR(9))
       IF (i > 0) CALL report_error('Invalid character in library name: "'//in%REACTION(i:i)//'"','Improper usage',in,1)
    CASE ('REGISTER ')
       i = INDEX(in%REACTION,'@')
       ! Check for @ sign, and that a period follows @
       IF ((i< 1) .OR. (INDEX(in%REACTION(i:),'.') < 1)) CALL report_error('The email address "'//TRIM(in%REACTION)// &
            '" is not valid.','Improper usage',in,1)
    CASE ('MAKE ELEMENT SYNTHESIS MOVIE')
       i = VERIFY(in%REACTION,ALPHANUM//' _-')
       IF (i /= 0) CALL report_error('Invalid quality indicator: "'//in%REACTION(i:i)//'"', &
            'Improper usage',in,1)
    CASE ('MODIFY NUC DATA ')
       i = VERIFY(in%BODY,ALPHANUM//'_.-+ '//ACHAR(9))
       IF (i > 0) CALL report_error('Invalid character in nuclear data set name: "'//in%BODY(i:i)//'"','Improper usage',in,1)
    CASE DEFAULT
       DO i = 1, LEN(in%REACTION)
          IF (VERIFY(in%REACTION(i:i),ALPHANUM//',+-> ') > 0) filter_cina_common%REACTION(i:i) = ' '
       END DO
    END SELECT
    
    ! Filter TYPE to only allow S(E), CS(E), R(T), or blank
    SELECT CASE (in%ACTION)
    CASE ('GET RATE LIST ','GET NUC DATA LIST')
       i = VERIFY(in%REACTION,NUM//', ')
       IF (i /= 0) CALL report_error('Invalid type: '//TRIM(in%REACTION),'Improper usage',in,1)
    CASE ('ELEMENT SYNTHESIS SETUP')
       ! Do nothing
    CASE DEFAULT
       SELECT CASE (in%TYPE)
       CASE ('S(E) ','CS(E) ','R(T) ',' ')
          ! Do nothing
       CASE DEFAULT
          CALL report_error('Unknown type: '//TRIM(in%TYPE),'Improper usage',in,1)
       END SELECT
    END SELECT
    
    ! Filter FORMAT to only allow numbers or a comma
    SELECT CASE (in%ACTION)
    CASE ('EXPORT RATE LIBRARY')

    CASE ('GET ELEMENT SYNTHESIS FLUXES ', 'GET ELEMENT SYNTHESIS ABUNDANCES ')
       IF (in%FORMAT /= 'Y' .AND. in%FORMAT /= 'N' .AND. in%FORMAT /= '') &
            CALL report_error('Invalid format: '//TRIM(in%FORMAT),'Improper usage',in,1)
    CASE DEFAULT
       DO i = 1, LEN(in%FORMAT)
          IF (VERIFY(in%FORMAT(i:i),NUM//', ') > 0) &
               CALL report_error('Invalid format: '//TRIM(in%FORMAT),'Improper usage',in,1)
       END DO
    END SELECT
    
    IF ((in%FILENAME) .AND. (in%ACTION == 'READ INPUT')) THEN
       ! Filter FILENAME to only allow alphanumeric characters, period, and slash
       i = VERIFY(TRIM(file_storage),ALPHANUM//'./')
       IF (i /= 0) CALL report_error('Invalid file: '//TRIM(file_storage), &
            'Improper usage',in,1)
    END IF

    ! Filter BODY according to the action
    SELECT CASE (in%ACTION)
    CASE ('GENERATE PARAMETER FORMAT ')
       i = VERIFY(in%BODY,ALPHANUM//',_ ')
       IF (i > 0) CALL report_error('Invalid character for generating parameter format: "'// &
            TRIM(in%BODY(i:i))//'"','Improper usage',in,1)
    CASE ('GET RATE LIBRARY LIST ')
       i = VERIFY(in%BODY,ALPHANUM//'_ '//ACHAR(9))
       IF (i /= 0) CALL report_error('Invalid character in library group: '//in%BODY(i:i),'Improper usage',in,1)
    CASE ('GET RATE LIBRARY INFO ','GET RATE LIBRARY ISOTOPES ','GET RATE LIST ','MODIFY RATE LIBRARY ')
       i = VERIFY(in%BODY,ALPHANUM//'_.- '//ACHAR(9))
       IF (i > 0) CALL report_error('Invalid character in library name: "'//in%BODY(i:i)//'"','Improper usage',in,1)
    CASE ('MODIFY NUC DATA SET')
       i = VERIFY(in%BODY,ALPHANUM//'_.-+ '//ACHAR(9))
       IF (i > 0) CALL report_error('Invalid character in nuclear data set name: "'//in%BODY(i:i)//'"','Improper usage',in,1)
    END SELECT

    ! Filter NOTES according to the action
    SELECT CASE (in%ACTION)
    CASE ('GET RATE LIST ')
       i = VERIFY(in%NOTES,NUM//', ')
       IF (i /= 0) CALL report_error('Invalid isotope list: '//TRIM(in%NOTES),'Invalid usage',in,1)
    CASE ('MODIFY LIBRARY ')
       i = VERIFY(in%NOTES,ALPHANUM//'_.- '//ACHAR(9))
       IF (i > 0) CALL report_error('Invalid character in library name: "'//in%BODY(i:i)//'"','Improper usage',in,1)
    END SELECT

    ! Filter rids_storage
    SELECT CASE (in%ACTION)
    CASE ('REGISTER ', 'GET NUC DATA INFO', 'NUC DATA EXIST')
       ! Don't filter
    CASE DEFAULT
       ! Allow () in rate id until REACLIB is parsed, then don't allow them
       i = VERIFY(rids_storage,ALPHANUM//'_+-,>~*(). '//ACHAR(9)//ACHAR(11)//ACHAR(10))
       IF (i > 0) CALL report_error('Invalid character in rate id: '//rids_storage(i:i),'Improper usage',in,1)
    END SELECT

    ! Filter prop_storage
    SELECT CASE (in%ACTION)
    CASE ('REGISTER ')
       ! Don't filter
    CASE DEFAULT
       i = INDEX(prop_storage,ACHAR(9)//ACHAR(10))
       IF (i /= 0) CALL report_error('Properties can not have tabs or linefeeds','Invalid usage',in,1)
    END SELECT

    ! Encode BODY so that it only contains HEX characters
    filter_cina_common%BODY = cina_encode(in%BODY)
    
    ! Encode NOTES so that it only contains HEX characters
    filter_cina_common%NOTES = cina_encode(in%NOTES)
    
  END FUNCTION filter_cina_common
  
  !---------------------------------------------------------------------
  FUNCTION cina_encode(in)
    !PURPOSE = Encode string to HEX characters
    !STATUS = Complete and tested
    !DESC = Encoding produces a string that is twice as long as the input string
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: in
    CHARACTER(LEN=2*LEN(in))      :: cina_encode
    INTEGER(KIND=4)               :: i
    
    cina_encode = ''
    
    DO i = 1, LEN_TRIM(in)
       ! This converts each character into 2 that contain the HEX digits for the ASCII value
       ! For example, 'M' has a ASCII value of 77 or 4D in HEX.  The encoded string is '4D'
       WRITE(cina_encode(2*i-1:2*i),'(Z2.2)') IACHAR(in(i:i))
    END DO
  END FUNCTION cina_encode
  
  !---------------------------------------------------------------------
  FUNCTION cina_decode(in)
    !PURPOSE = Decode HEX characters to string
    !STATUS = Complete and tested
    !DESC = Decoding produces a string that is half as long as the input string
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: in
    CHARACTER(LEN=LEN(in)/2)      :: cina_decode
    INTEGER(KIND=4)               :: i,n,e
    
    cina_decode = ''
    
    DO i = 1, LEN_TRIM(in)/2
       ! This converts a HEX string like '4D' to the decimal number 77 and stores it in n
       READ(in(2*i-1:2*i),'(Z2)',IOSTAT=e) n
       IF (e == 0) THEN
          ! Convert n into ASCII character
          cina_decode(i:i) = ACHAR(n)
       ELSE
          cina_decode(i:i) = '?'
       END IF
    END DO
  END FUNCTION cina_decode
  
  !---------------------------------------------------------------------
  SUBROUTINE print_input(pcom)
    !PURPOSE = Parse, decode, and print input.  Do not process it.
    !STATUS = Complete and tested.
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    
    WRITE(*,'(3A)') 'HEADER="',TRIM(pcom%HEADER),'"'
    WRITE(*,'(3A)') 'ID="',TRIM(pcom%ID),'"'
    WRITE(*,'(3A)') 'ACTION="',TRIM(pcom%ACTION),'"'
    WRITE(*,'(3A)') 'USER="',TRIM(pcom%USER),'"'
    WRITE(*,'(3A)') 'PW="',TRIM(pcom%PW),'"'
    WRITE(*,'(3A)') 'BODY="',TRIM(cina_decode(pcom%BODY)),'"'
    WRITE(*,'(3A)') 'REACTION="',TRIM(pcom%REACTION),'"'
    WRITE(*,'(3A)') 'NOTES="',TRIM(cina_decode(pcom%NOTES)),'"'
    WRITE(*,'(3A)') 'TYPE="',TRIM(pcom%TYPE),'"'
    WRITE(*,'(3A)') 'FORMAT="',TRIM(pcom%FORMAT),'"'
    !WRITE(*,'(3A)') 'FILE="',TRIM(pcom%FILE),'"'
    WRITE(*,'(A,L1)') 'POSITIVE_CHK=',pcom%POSITIVE_CHK
    WRITE(*,'(A,L1)') 'SINGLE_CHK=',pcom%SINGLE_CHK
    WRITE(*,'(A,L1)') 'RANGE_CHK=',pcom%RANGE_CHK
    WRITE(*,'(A,L1)') 'CONTINUITY_CHK=',pcom%CONTINUITY_CHK
    WRITE(*,'(A,L1)') 'ERROR_CHK=',pcom%ERROR_CHK
    WRITE(*,'(A,L1)') 'REACTION_CHK=',pcom%REACTION_CHK
    
  END SUBROUTINE print_input
  
  !---------------------------------------------------------------------
  FUNCTION get_CGI_ID(pcom)
    !PURPOSE = Perform the CGI Action "GET ID"
    !STATUS = Complete and tested
    !DESC = This subroutine does not return
!    USE CINA
    USE fileio
    IMPLICIT NONE
    TYPE(cina_common),INTENT(INOUT) :: pcom
    CHARACTER(LEN=2000)            :: tmps
    CHARACTER(LEN=20)             :: id,id_new,get_CGI_ID
    CHARACTER(LEN=8)              :: id_date
    CHARACTER(LEN=10)             :: id_time,iostat_str
    INTEGER(KIND=4)               :: i,c,ucount,maxlogins
    LOGICAL(KIND=1)               :: loop
    
    CALL new_session(pcom%USER,pcom%PW,pcom%ACTION,'Called get_CGI_ID',get_CGI_ID,pcom%SESSIONP,tmps)
    IF (tmps /= '') CALL log_error2(pcom%SESSIONP,'Username or password is invalid','Authencation',0,1,tmps, &
         '$Id: cina_core.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $',1)

  END FUNCTION get_CGI_ID
  
  
  !---------------------------------------------------------------------
  SUBROUTINE report_error(s,t,pcom,x,prgm_name)
    !PURPOSE = Report error to user and log error
    !STATUS = Complete and tested
    !DESC = s is a string to send to the user and log file
    !DESC = t is a string of the type of error
    !DESC = x is an INTEGER(KIND=4).  
    !DESC =   If 0, this function returns.
    !DESC =   If 1, this function terminates the cina program
    !DESC =   If 2, this function returns but does not log the message
    !DESC =   If 3, this function does not print the message but logs it (for the LOG action)
    !DESC =   If 4, same as 2 but prints ERROR instead of CAUTION
    USE convert
    USE io
!    USE CINA
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: s,t,prgm_name
    TYPE(cina_common),INTENT(IN) :: pcom
    INTEGER(KIND=4),INTENT(IN)    :: x
    INTEGER(KIND=4)               :: i,first,last
    CHARACTER(LEN=100)            :: log_level,ident
    CHARACTER(LEN=200)            :: header
    CHARACTER(LEN=32765)          :: message
    CHARACTER(LEN=10)             :: h
    CHARACTER(LEN=1)              :: blank
    OPTIONAL                      :: prgm_name
    
    IF ((x == 0) .OR. (x == 2)) THEN
       h = 'CAUTION='
    ELSE
       h = 'ERROR='
    END IF
    
    IF (x /= 3) THEN
       ! Print log message to user
       ! Look for carriage returns, and prefix them with 'ERROR: '
       i = LEN_TRIM(s)
       first = 1
       last = 1
       DO WHILE (last < i)
          last = SCAN(s(first:),ACHAR(10)) + first - 1
          IF (last == first-1) last = i + 1
          !print *,first,last,i
          WRITE(*,'(A,A)') TRIM(h),s(first:last-1)
          first = last + 1
       END DO
       IF ((x == 2) .OR. (x == 4)) RETURN
    END IF
    
    CALL log_error(pcom%SESSIONP,s,t,0,x,blank,blank)
    
    ! Create message header
    WRITE(header,'(10A)',IOSTAT=i) TRIM(str2line(TRIM(pcom%USER))),'(',pcom%ID,',',TRIM(pcom%IP),') ', &
         TRIM(t),'|',TRIM(str2line(TRIM(pcom%ACTION))),'|'

    !WRITE(*,'(A)') 'ERROR='//TRIM(header)

    ! Save program name
    IF (PRESENT(prgm_name)) THEN
       ident = prgm_name
    ELSE
       ident = CINA_NAME
    END IF

    ! Set the log_level
    SELECT CASE (t)
    CASE ('Authencation','File input/output','External fileio','Bad file format','External program')
       log_level = 'ERR'
    CASE ('Improper usage','Developer Reminder','Missing feature','External log','Warning')
       log_level = 'NOTICE'
!!$    CASE ('Warning')
!!$       log_level = 'WARNING'
    CASE ('Improbable','Severe Authencation')
       log_level = 'CRIT'
    CASE ('Pilot system')
       log_level = 'ALERT'
    CASE ('Debug')
       log_level = 'DEBUG'
    CASE DEFAULT
       log_level = 'ERR'
       ! Report that this is an unknown error type
       i = use_syslog(TRIM(ident),'NOTICE',TRIM(header)//'Unknown error type ('// &
            TRIM(t)//') in report_error',TRIM(SYSLOG_PATH))
    END SELECT

    !WRITE(*,'(A)') 'ERROR=type='//TRIM(t)//', log_level='//TRIM(log_level)
    message = str2line(TRIM(s))
    ! Log the message
    i = use_syslog(TRIM(ident),TRIM(log_level),TRIM(header)//TRIM(message),TRIM(SYSLOG_PATH))
    IF (x == 3) THEN
       IF (i == 0) THEN
          !WRITE(*,'(A)') 'LOG=SUCCESS'  !This is assumed
       ELSE
          WRITE(*,'(A)',IOSTAT=i) 'LOG=FAIL'
          WRITE(*,'(A,I0)',IOSTAT=i) 'REASON=Error number ',i
          STOP
       END IF
    ELSE !x /= 3
       IF (i == 0) THEN
          WRITE(*,'(A,2A)',IOSTAT=i) TRIM(h),'The above errors were reported to the administrator ', &
               'to help improve this software package.'
          WRITE(*,'(A,A)',IOSTAT=i) TRIM(h),'Please email coordinator@nucastrodata.org for more information.'
       ELSE
          WRITE(*,'(A,2A,I0)',IOSTAT=i) TRIM(h),'The above errors could NOT be reported ',&
               'to the administrator because of error number ',i
          WRITE(*,'(A,A)',IOSTAT=i) TRIM(h),'Please email coordinator@nucastrodata.org for more information.'
       END IF
    END IF

    
    IF (x == 1) STOP
    
  END SUBROUTINE report_error
  
  !---------------------------------------------------------------------
  !SUBROUTINE report_error(s,t,pcom,x,prgm_name)
  SUBROUTINE log_error2(sessionp,client_msg,debug_msg,type,val,str,cvsid,action)
    !PURPOSE = Report error to user and log error
    !STATUS = Complete and tested
    !DESC = s is a string to send to the user and log file
    !DESC = t is a string of the type of error
    !DESC = x is an INTEGER(KIND=4).  
    !DESC =   If 0, this function returns.
    !DESC =   If 1, this function terminates the cina program
    !DESC =   If 2, this function returns but does not log the message
    !DESC =   If 3, this function does not print the message but logs it (for the LOG action)
    !DESC =   If 4, same as 2 but prints ERROR instead of CAUTION
!    USE CINA
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: client_msg,debug_msg,str,cvsid
    INTEGER(KIND=4),INTENT(IN)    :: sessionp
    INTEGER(KIND=4),INTENT(IN)    :: type,val
    INTEGER(KIND=4),INTENT(IN)    :: action
    CHARACTER(LEN=10)             :: h
    INTEGER(KIND=4)               :: i,first,last,x
    OPTIONAL :: action

    IF (PRESENT(action)) THEN
       x = action
    ELSE
       x = 1
    END IF

    IF ((x == 0) .OR. (x == 2)) THEN
       h = 'CAUTION='
    ELSE
       h = 'ERROR='
    END IF

    IF (x /= 3) THEN
       ! Print log message to user
       ! Look for carriage returns, and prefix them with 'ERROR: '
       i = LEN_TRIM(client_msg)
       first = 1
       last = 1
       DO WHILE (last < i)
          last = SCAN(client_msg(first:),ACHAR(10)) + first - 1
          IF (last == first-1) last = i + 1
          WRITE(*,'(A,A)') TRIM(h),client_msg(first:last-1)
          first = last + 1
       END DO
       IF ((x == 2) .OR. (x == 4)) RETURN
    END IF
    
    CALL log_error(sessionp,client_msg,debug_msg,type,val,str,cvsid)

          WRITE(*,'(A,2A)',IOSTAT=i) TRIM(h),'The above errors were reported to the administrator ', &
               'to help improve this software package.'
          WRITE(*,'(A,A)',IOSTAT=i) TRIM(h),'Please email coordinator@nucastrodata.org for more information.'
    
    IF (x == 1) STOP
    
  END SUBROUTINE log_error2
  
  
  !---------------------------------------------------------------------
  SUBROUTINE chk_reaction(pcom)
    !PURPOSE = Check if a reaction is valid and return a parsed version
    !STATUS = Complete and tested
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    TYPE(reactionparticles)       :: r
    CHARACTER(LEN=100)            :: m
    INTEGER(KIND=4)               :: i
    
    ! Convert the pcom%REACTION string into r structure
    CALL read_reac_str(r,pcom%REACTION,m,pcom%REACTION_TYPE)

    IF (LEN_TRIM(m) > 0) THEN
       WRITE(*,'(A)') 'VALID=N'
       WRITE(*,'(2A)') 'REASON=',TRIM(m)
       STOP
    END IF
    
    ! Check if r is a valid reaction
    CALL validreaction(r,i,m)
    SELECT CASE (i)
    CASE (0)
       WRITE(*,'(A)') 'VALID=Y'
       m = getreac_str(r,1)
       WRITE(*,'(2A)') 'REACTION=',TRIM(m)
       m = getreac_str(r,3)
       WRITE(*,'(2A)') 'REACTION_INT=',TRIM(m)
    CASE (-1)
       WRITE(*,'(A)') 'VALID=N'
       WRITE(*,'(2A)') 'REASON=',TRIM(m)
    CASE (-2)
       CALL report_error(m,'Warning',pcom,0)
       WRITE(*,'(A)') 'VALID=Y'
       m = getreac_str(r,1)
       WRITE(*,'(2A)') 'REACTION=',TRIM(m)
       m = getreac_str(r,3)
       WRITE(*,'(2A)') 'REACTION_INT=',TRIM(m)
    CASE DEFAULT
       WRITE(m,'(A,I0)') 'Unknown case in chk_reaction: ',i
       CALL report_error(m,'Developer Reminder',pcom,1)
    END SELECT
    
    STOP
    
  END SUBROUTINE chk_reaction
  
  !---------------------------------------------------------------------
  SUBROUTINE get_inv_reaction(pcom)
    !PURPOSE = Get inverse reaction string
    !STATUS = Complete and tested
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    TYPE(reactionparticles)       :: r
    CHARACTER(LEN=100)            :: m
    INTEGER(KIND=4)               :: i
    
    ! Convert the pcom%REACTION string into r structure
    CALL read_reac_str(r,pcom%REACTION,m,pcom%REACTION_TYPE)

    IF (LEN_TRIM(m) > 0) THEN
       WRITE(*,'(A)') 'VALID=N'
       WRITE(*,'(2A)') 'REASON=',TRIM(m)
       STOP
    END IF
    
    ! Get inverse reaction
    r = getinverse(r)
    ! Get string
    m = getreac_str(r,1)

    WRITE(*,'(2A)') 'REACTION=',TRIM(m)
    STOP
    
  END SUBROUTINE get_inv_reaction
  
  !---------------------------------------------------------------------
  SUBROUTINE CGI_logout(pcom)
    !PURPOSE = Perform all logout operations
    !STATUS = Complete and tested
!    USE CINA
    USE fileio
    IMPLICIT NONE
    TYPE(cina_common),INTENT(INOUT) :: pcom
    CHARACTER(LEN=50)             :: user
    CHARACTER(LEN=20)             :: id
    CHARACTER(LEN=8)              :: id_date
    CHARACTER(LEN=10)             :: id_time,iostat_str
    CHARACTER(LEN=150)            :: tmps
    INTEGER(KIND=4)               :: i,c
    LOGICAL(KIND=4)               :: good,loop
    
    ! Kill any running programs for this ID
    CALL kill_bg_proc(pcom,TRIM(TEMP_PATH)//pcom%ID//'/rategen/')
    CALL kill_bg_proc(pcom,TRIM(TEMP_PATH)//pcom%ID//'/em_syn/')
    
    ! Delete all temp files
    i = safe_shell('/bin/rm -fR '''//TRIM(TEMP_PATH)//pcom%ID//'''')
    IF (i /= 0) THEN
       WRITE(id,'(I0)') i
       CALL report_error('Could not delete temp files. Error '//TRIM(id),'External fileio',pcom,0)
    END IF

    CALL close_session(pcom%SESSIONP,tmps)
    IF (tmps /= '') CALL log_error2(pcom%SESSIONP,'Error closing session','Authencation',0,3,tmps, &
         '$Id: cina_core.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $',3)

    
    WRITE(*,'(A)') 'LOGOUT=SUCCESS'
  END SUBROUTINE CGI_logout
  
  !---------------------------------------------------------------------
  FUNCTION safe_shell(str)
    !PURPOSE = Execute a shell command safer than using SHELL()
    !STATUS = Complete and tested
    !CAUTION = Make sure you remove environment variables before calling this function
    USE IFPORT
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: str
    CHARACTER(LEN=LEN_TRIM(str))  :: cmd
    INTEGER(KIND=4)               :: safe_shell,i
    
    cmd = str
    
    ! Remove unsafe characters from str
    i = 1
    DO WHILE (i > 0)
       ! Check for character not considered to be safe
       i = VERIFY(cmd,ALPHANUM//' +->,''().&/_$!')
       IF (i > 0) cmd(i:i) = ' '
    END DO
    
    !WRITE(*,'(2A)') 'CAUTION=shell=',cmd
    safe_shell=SYSTEM(cmd)
    
  END FUNCTION safe_shell
  
  !---------------------------------------------------------------------
  SUBROUTINE rm_env_var
    !PURPOSE = Remove environment variables
    !STATUS = Removes variables shown by the env command
    !CAUTION = The shell adds environment variables not displayed with the env command
    IMPLICIT NONE
    LOGICAL(KIND=4)                  :: s

    s = SETENVQQ('SERVER_SIGNATURE= ')
    s = SETENVQQ('UNIQUE_ID= ')
    s = SETENVQQ('HTTP_KEEP_ALIVE= ')
    s = SETENVQQ('HTTP_USER_AGENT= ')
    s = SETENVQQ('SERVER_PORT= ')
    s = SETENVQQ('HTTP_HOST= ')
    s = SETENVQQ('DOCUMENT_ROOT= ')
    s = SETENVQQ('HTTP_ACCEPT_CHARSET= ')
    s = SETENVQQ('SCRIPT_FILENAME= ')
    s = SETENVQQ('REQUEST_URI= ')
    s = SETENVQQ('SCRIPT_NAME= ')
    s = SETENVQQ('REMOTE_HOST= ')
    s = SETENVQQ('HTTP_CONNECTION= ')
    s = SETENVQQ('REMOTE_PORT= ')
    s = SETENVQQ('PATH= ')
    !s = SETENVQQ('PWD= ')          ! Could not change PWD on last try
    s = SETENVQQ('SERVER_ADMIN= ')
    s = SETENVQQ('HTTP_ACCEPT_LANGUAGE= ')
    s = SETENVQQ('HTTP_REFERER= ')
    s = SETENVQQ('HTTP_ACCEPT= ')
    s = SETENVQQ('REMOTE_ADDR= ')
    !s = SETENVQQ('SHLVL= ')        ! Could not change SHLVL on last try
    s = SETENVQQ('SERVER_NAME= ')
    s = SETENVQQ('CONTENT_LENGTH= ')
    s = SETENVQQ('SERVER_SOFTWARE= ')
    s = SETENVQQ('QUERY_STRING= ')
    s = SETENVQQ('SERVER_ADDR= ')
    s = SETENVQQ('GATEWAY_INTERFACE= ')
    s = SETENVQQ('SERVER_PROTOCOL= ')
    s = SETENVQQ('HTTP_ACCEPT_ENCODING= ')
    s = SETENVQQ('CONTENT_TYPE= ')
    s = SETENVQQ('REQUEST_METHOD= ')
    s = SETENVQQ('SHELL= ')

  END SUBROUTINE rm_env_var

  !---------------------------------------------------------------------
  SUBROUTINE kill_bg_proc(pcom,dir)
    !PURPOSE = Kill the process running in the background
    !STATUS = Complete and tested
    !DESC = Only one process (genrate or genparm) for each ID is allowed to run in the background
    !DESC = This routine will not return if the pid file can not be read
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: dir
    INTEGER(KIND=4)                  :: i,s,p
    CHARACTER(LEN=10)                :: tmps

    p = bg_pid(pcom,dir)
    IF (p > 0) THEN      ! If the pid was present in file
       ! Try to kill the old process
       s = KILL(p,15)  ! Send the TERM signal
       ! s == -1 if process does not exist
       ! s ==  0 if process was killed
       IF ((s /= 0) .AND. (s /= -1)) THEN
          WRITE(tmps,'(I0)') s
          CALL report_error('Error '//TRIM(tmps)//' while killing process.','Developer Reminder',pcom,1)
       END IF
    END IF
  END SUBROUTINE kill_bg_proc

  !---------------------------------------------------------------------
  FUNCTION bg_pid(pcom,dir)
    !PURPOSE = Get PID of background process
    !STATUS = Complete and tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: dir
    INTEGER(KIND=4)                  :: i,s,bg_pid
    CHARACTER(LEN=10)                :: tmps

    bg_pid = -1
    !dir = TRIM(TEMP_PATH)//pcom%ID//'/'
    s = safe_shell('/bin/ls '''//TRIM(dir)//'pid'' &> /dev/null')
    IF (s == 0) THEN     ! If the pid file exists
       ! Read in pid from file
       OPEN(4,FILE=TRIM(dir)//'pid',ACTION='READ',STATUS='OLD',IOSTAT=i)
       IF (i /= 0) THEN
          CLOSE(4)
          WRITE(tmps,'(I0)') i
          CALL report_error('1712: Can not open pid file.  Error '//TRIM(tmps),'File input/output',pcom,1)
       END IF
       READ(4,'(I)',IOSTAT=i) bg_pid
       IF (i /= 0) THEN
          CLOSE(4)
          WRITE(tmps,'(I0)') i
          CALL report_error('1718: Can not read pid file.  Error '//TRIM(tmps),'File input/output',pcom,1)
       END IF
       CLOSE(4)
    END IF
  END FUNCTION bg_pid

  !---------------------------------------------------------------------
  SUBROUTINE register(pcom)
    !PURPOSE = Save a registration submission
    !STATUS = Complete and tested
    USE io
    USE convert
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i
    CHARACTER(LEN=32767)             :: s
    CHARACTER(LEN=1024)              :: e
    
    ! Save registration submission
    OPEN(80,FILE=TRIM(ADM_PATH)//'Registrations',ACTION='WRITE',IOSTAT=i,POSITION='APPEND')
    IF (i /= 0) CALL report_error('Error opening file (1)','File input/output',pcom,1)
    
    WRITE(80,'(12A)',IOSTAT=i,ADVANCE='NO') &
         TRIM(str2line(TRIM(cina_decode(pcom%BODY)))),ACHAR(9), &     ! Last name
         TRIM(str2line(TRIM(cina_decode(pcom%NOTES)))),ACHAR(9), &    ! First name
         get_date(),ACHAR(9),get_time(),ACHAR(9),TRIM(pcom%IP),ACHAR(9), & ! Date, Time, IP
         TRIM(str2line(TRIM(pcom%REACTION))),ACHAR(9)                  ! Email
    IF (i /= 0) CALL report_error('Error saving registration information (1)','File input/output',pcom,1)
    
    s = str2line(TRIM(get_file()))                               ! Institution
    WRITE(80,'(2A)',IOSTAT=i,ADVANCE='NO')        TRIM(s),ACHAR(9)
    IF (i /= 0) CALL report_error('Error saving registration information (2)','File input/output',pcom,1)
    
    s = str2line(TRIM(get_rids()))                               ! Mailing Address
    WRITE(80,'(2A)',IOSTAT=i,ADVANCE='NO')        TRIM(s),ACHAR(9)
    IF (i /= 0) CALL report_error('Error saving registration information (3)','File input/output',pcom,1)
    
    s = str2line(TRIM(get_prop()))                               ! Research Type
    WRITE(80,'(2A)',IOSTAT=i,ADVANCE='NO')        TRIM(s),ACHAR(9)
    IF (i /= 0) CALL report_error('Error saving registration information (4)','File input/output',pcom,1)
    
    s = str2line(TRIM(get_data1()))                              ! Where did you hear of this suite?
    WRITE(80,'(2A)',IOSTAT=i,ADVANCE='NO')        TRIM(s),ACHAR(9)
    IF (i /= 0) CALL report_error('Error saving registration information (5)','File input/output',pcom,1)

    s = str2line(TRIM(get_data2()))                              ! Additional Information (supervisor / research mentor)
    WRITE(80,'(2A)',IOSTAT=i)        TRIM(s),ACHAR(9)
    IF (i /= 0) CALL report_error('Error saving registration information (6)','File input/output',pcom,1)
    CLOSE(80)
    
    s = 'ASTRODATA Computational Infrastructure Suite Registration'//ACHAR(10)// &
       'Date: '//get_date()//ACHAR(10)//'Time: '//get_time()//ACHAR(10)//'IP: '//TRIM(pcom%IP)//ACHAR(10)// &
       'ID: '//pcom%ID//ACHAR(10)//'Dir: '//TRIM(CINA_NAME)//ACHAR(10)//ACHAR(10)// &
       'Last Name: '//TRIM(cina_decode(pcom%BODY))//ACHAR(10)//'First Name: '//TRIM(cina_decode(pcom%NOTES))//ACHAR(10)// &
       'Email: '//TRIM(pcom%REACTION)//ACHAR(10)//ACHAR(10)// &
       'Institution: '//ACHAR(10)//TRIM(get_file())//ACHAR(10)//ACHAR(10)// &
       'Mailing Address: '//ACHAR(10)//TRIM(get_rids())//ACHAR(10)//ACHAR(10)// &
       'Research Type: '//ACHAR(10)//TRIM(get_prop())//ACHAR(10)//ACHAR(10)// &
       'Where did you hear of this suite?: '//ACHAR(10)//TRIM(get_data1())//ACHAR(10)//ACHAR(10)// &
       'Additional Information: '//ACHAR(10)//TRIM(get_data2())

    ! Send email
    i = send_email(TRIM(s),'Registration Submission for ASTRODATA suite','coordinator@nucastrodata.org',TRIM(TEMP_PATH)//pcom%ID//'.reg',e)
    IF (i /= 0) CALL report_error('Error sending registration email: '//TRIM(e),'Improbable',pcom,1)
    
    WRITE(*,'(A)') 'REGISTER=SUCCESS'
  END SUBROUTINE register

  !---------------------------------------------------------------------
  FUNCTION get_rids
    !PURPOSE = Return rids_storage
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=LEN(rids_storage)) :: get_rids

    get_rids = rids_storage
  END FUNCTION get_rids

  !---------------------------------------------------------------------
  FUNCTION get_prop
    !PURPOSE = Return prop_storage
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=LEN(rids_storage)) :: get_prop

    get_prop = prop_storage
  END FUNCTION get_prop

  !---------------------------------------------------------------------
  FUNCTION get_file
    !PURPOSE = Return file_storage
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=LEN(rids_storage)) :: get_file

    get_file = file_storage
  END FUNCTION get_file

  !---------------------------------------------------------------------
  FUNCTION get_data1
    !PURPOSE = Return data1_storage
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=LEN(data1_storage)) :: get_data1

    get_data1 = data1_storage
  END FUNCTION get_data1

  !---------------------------------------------------------------------
  FUNCTION get_data2
    !PURPOSE = Return data2_storage
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=LEN(data2_storage)) :: get_data2

    get_data2 = data2_storage
  END FUNCTION get_data2

  !---------------------------------------------------------------------
END MODULE cina_core
