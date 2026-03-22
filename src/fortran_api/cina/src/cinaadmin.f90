PROGRAM cinaadmin
  !PURPOSE = Perform administrative tasks for cina program
  !STATUS = In development
  !DESC = 
  USE IFLPORT
  USE cinaadmin_core
  USE cina_core
  USE convert
  USE io
  IMPLICIT NONE
  CHARACTER(LEN=9),PARAMETER       :: PROGRAM_VER = '0.1'
  TYPE(cinaadm)                   :: padm(10)
  TYPE(p_admin_options)            :: paopt
  CHARACTER(LEN=100)               :: prgm_name
  CHARACTER(LEN=2000)              :: user_list
  INTEGER(KIND=4)                  :: i,n

  ! Initialize random number generator
  CALL RANDOM_SEED

  ! Get this program's path
  CALL GETARG(0,prgm_name)
  ! Extract this program's name
  i = INDEX(prgm_name,'/',BACK=.TRUE.)
  IF (i > 0) prgm_name = prgm_name(i+1:)

  ! Get path
  OPEN(UNIT=10,FILE='/etc/'//TRIM(CINA_NAME)//'.conf',ACTION='READ')
  READ(10,'(A)',IOSTAT=n) paopt%dir
  CLOSE(10)
  ! Check for write access
  OPEN(UNIT=10,FILE='/etc/'//TRIM(CINA_NAME)//'.conf',ACTION='WRITE')
  WRITE(10,'(A)',IOSTAT=n) TRIM(paopt%dir)
  CLOSE(10)

  ! Check that root is running this program
  i = GETUID()
  IF (i /= 0) CALL printerror('Only root can run cinaadmin.',1)

  ! Change the current working directory so that paths in cina_core will work
  i = CHDIR(TRIM(paopt%dir)//'..')
  IF (i /= 0) CALL printerror('Could not change directory to '//TRIM(paopt%dir)//'..',1)

  ! Parse any command line options
  CALL chk_admoptions(paopt,padm)
  IF (paopt%help /= 0) CALL print_cinaadmin_help(PROGRAM_VER)
  IF (paopt%version /= 0) CALL print_cinaadmin_ver(PROGRAM_VER)

  ! Make user list for logging
  user_list = ''
  i = 1
  DO WHILE (LEN_TRIM(padm(i)%USER) > 0)
     user_list = TRIM(user_list) // ',' // padm(i)%USER
     i = i + 1
  END DO
  user_list = user_list(2:)    ! Remove initial comma

  SELECT CASE (lowercase(paopt%action))
  CASE ('adduser ', 'changeuser ')
     i = use_syslog(TRIM(prgm_name),'NOTICE',TRIM(lowercase(paopt%action))// &
          ' '//TRIM(user_list),TRIM(SYSLOG_PATH))
     CALL modify_account(paopt,padm)
  CASE ('removeusers ')
     i = use_syslog(TRIM(prgm_name),'NOTICE',TRIM(lowercase(paopt%action))// &
          ' '//TRIM(user_list),TRIM(SYSLOG_PATH))
     i = 1
     DO WHILE (LEN_TRIM(padm(i)%USER) > 0)
        CALL modify_account(paopt,padm(i))
        i = i + 1
     END DO
  CASE ('listusers ')
     i = use_syslog(TRIM(prgm_name),'NOTICE',TRIM(lowercase(paopt%action))// &
          ' '//TRIM(user_list),TRIM(SYSLOG_PATH))
     CALL view_accounts(paopt,padm)
  CASE ('autologout ')
     CALL auto_logout(paopt,prgm_name)
  END SELECT
END PROGRAM cinaadmin

!---------------------------------------------------------------------
FUNCTION cinaadmin_ver
  !PURPOSE = Return the CVS revision number of this file
  !STATUS = Complete and tested
  IMPLICIT NONE
  CHARACTER(LEN=20),PARAMETER     :: CINAADM_VERSION = '$Revision: 1.1.1.1 $'
  CHARACTER(LEN=10)               :: cinaadmin_ver

  cinaadmin_ver = CINAADM_VERSION(12:LEN_TRIM(CINAADM_VERSION)-2)

END FUNCTION cinaadmin_ver

!---------------------------------------------------------------------
SUBROUTINE print_cinaadmin_ver(PROGRAM_VER)
  !PURPOSE = Print the cinaadmin version information
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  USE io
  USE convert
  USE fileio
  USE cina_core
  USE cinaadmin_core
  USE rategen
  USE reactionstrings
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)      :: PROGRAM_VER
  CHARACTER(LEN=10)                :: cinaadmin_ver
  EXTERNAL                         :: cinaadmin_ver

  WRITE(*,'(2A)') 'cinaadmin version ',PROGRAM_VER
  WRITE(*,'( A)') 'cinaadmin dependencies:'
  WRITE(*,'(T5,2A)') 'cinaadmin.f90 version ',cinaadmin_ver()
  WRITE(*,'(T5,2A)') 'io.f90 version ',io_ver()
  WRITE(*,'(T5,2A)') 'fileio.f90 version ',fileio_ver()
  WRITE(*,'(T5,2A)') 'convert.f90 version ',convert_ver()
  !WRITE(*,'(T5,2A)') 'options.f90 version ',options_ver()
  WRITE(*,'(T5,5A)') 'cina_core.f90 version ',TRIM(cina_core_ver()),' (',TRIM(CINA_NAME),')'
  WRITE(*,'(T5,2A)') 'cinaadmin_core.f90 version ',cinaadmin_core_ver()
  WRITE(*,'(T5,2A)') 'rategen.f90 version ',rategen_ver()
  WRITE(*,'(T5,2A)') 'reactionstrings.f90 version ',reactionstrings_ver()
  STOP

END SUBROUTINE print_cinaadmin_ver

!---------------------------------------------------------------------
SUBROUTINE print_cinaadmin_help(PROGRAM_VER)
  !PURPOSE = Print help screen for cinaadmin
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)    :: PROGRAM_VER

  WRITE(*,'(3A)') 'cinaadmin ',PROGRAM_VER,' Help'
  WRITE(*,'(A)') 'This program is an administrative utility for the cina web program.'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Usage:'
  WRITE(*,'(A)') '  cinaadmin [-h | -v]'
  WRITE(*,'(A)') '  cinaadmin adduser username [options]'
  WRITE(*,'(A)') '  cinaadmin changeuser username [options]'
  WRITE(*,'(A)') '  cinaadmin listusers [usernames]'
  WRITE(*,'(A)') '  cinaadmin removeusers [usernames]'
  WRITE(*,'(A)') '  cinaadmin autologout'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Options for adding and changing user accoutns:'
  WRITE(*,'(A)') '  -p password      password for user account'
  WRITE(*,'(A)') '  -t num           num is the minimum number of minutes to wait to'
  WRITE(*,'(A)') '                   automatically log out this account, otherwise 50'
  WRITE(*,'(A)') ' '
  STOP
END SUBROUTINE print_cinaadmin_help

!---------------------------------------------------------------------
SUBROUTINE modify_account(paopt,padm)
  !PURPOSE = Modify or Add one entry to the user/password file for cina
  !STATUS = Complete and tested
  !CAUTION = This function was not put into cinaadmin_core module to prevent
  !          an unauthorized person from using this library and changing an account
  USE IFLPORT
  USE convert
  USE cina_core
  USE cinaadmin_core
  USE io
  IMPLICIT NONE
  TYPE(cinaadm),INTENT(IN)        :: padm
  TYPE(p_admin_options),INTENT(IN) :: paopt
  TYPE(cinaadm)                   :: padmtemp
  INTEGER(KIND=4)                  :: i,j,n
  CHARACTER(LEN=200)               :: tmps
  LOGICAL(KIND=4)                  :: good

  OPEN(3,FILE=TRIM(paopt%dir)//'Users',ACTION='READ',POSITION='REWIND')
  OPEN(4,FILE=TRIM(paopt%dir)//'UsersNew',ACTION='WRITE',POSITION='REWIND')

  n = 0
  !Copy user entries into new file until the entry for this user is found
  DO WHILE (n == 0)
     READ(3,'(A)',IOSTAT=n) tmps
     IF (n > 0) THEN
        WRITE(tmps,'(A,I0,A)') 'Error ',n,' while reading account file.'
        CALL printerror(tmps,1)
     END IF
     IF (n == 0) THEN
        ! Decode entry
        padmtemp = decode_user_entry(tmps)
        ! Check username
        IF (padmtemp%USER == padm%USER) THEN
           n = -333
        ELSE
           ! Copy to new file
           tmps = encode_user_entry(padmtemp)
           WRITE(4,'(A)') TRIM(tmps)
        END IF
     END IF
  END DO

  SELECT CASE (paopt%action)
  CASE ('adduser')
     IF (n == -333) CALL printerror('User account already exists.',1)

     ! Ask for any values not specified on the command line
     padmtemp = padm
     DO WHILE (LEN_TRIM(padmtemp%PW) <= 0)
        WRITE(*,'(A)',ADVANCE='NO') 'Password: '
        READ(*,'(A)') padmtemp%PW
        WRITE(*,'(A)',ADVANCE='NO') 'Confirm password: '
        READ(*,'(A)') tmps
        IF (tmps /= padmtemp%PW) THEN
           padmtemp%PW = ''
           WRITE(*,'(A)') 'Passwords do not match.'
        END IF
     END DO
     IF (padmtemp%TIMEOUT <= 0) THEN
        WRITE(*,'(A)',ADVANCE='NO') 'Timeout (180): '
        READ(*,'(I)',IOSTAT=n) tmps
        IF (n /= 0) padmtemp%TIMEOUT = 180
     END IF

     tmps = encode_user_entry(padmtemp)
     WRITE(4,'(A)') TRIM(tmps)
  CASE ('changeuser', 'removeusers ')
     !IF (n /= -333) CALL printerror('User account '//TRIM(padm%USER)//' not found.',1)
     IF (n /= -333) THEN
        print *,'n=',n
        CALL printerror('User account '//TRIM(padm%USER)//' not found.',1)
     END IF

     IF (paopt%action == 'changeuser') THEN
        ! Get values to change from command line
        IF (LEN_TRIM(padm%PW) > 0) padmtemp%PW = padm%PW
        IF (padm%TIMEOUT > 0) padmtemp%TIMEOUT = padm%TIMEOUT
        
        tmps = encode_user_entry(padmtemp)
        WRITE(4,'(A)') TRIM(tmps)
     END IF

     ! Copy the rest of the user entries
     n = 0
     DO WHILE (n == 0)
        READ(3,'(A)',IOSTAT=n) tmps
        IF (n > 0) THEN
           WRITE(tmps,'(A,I0,A)') 'Error ',n,' while reading account file (2).'
           CALL printerror(tmps,1)
        END IF
        IF (n == 0) THEN
           ! Copy to new file
           padmtemp = decode_user_entry(tmps)
           tmps = encode_user_entry(padmtemp)
           WRITE(4,'(A)') TRIM(tmps)
        END IF
     END DO
  CASE DEFAULT
     CALL printerror('Unknown action in modify_account',1)
  END SELECT

  CLOSE(3)
  CLOSE(4)

  ! Delete Users file
  i = DELFILESQQ(TRIM(paopt%dir)//'Users')
  IF (i /= 1) CALL printerror('Could not delete Users file.',1)

  ! Rename UsersNew to Users
  good = RENAMEFILEQQ(TRIM(paopt%dir)//'UsersNew',TRIM(paopt%dir)//'Users')
  IF (.NOT. good) CALL printerror('Could not create Users file.',1)

  i = safe_shell('/bin/chown root '//TRIM(paopt%dir)//'Users')
  IF (i /= 0) CALL printerror('Could not change ownership on new Users file.',0)
  i = safe_shell('/bin/chgrp apache '//TRIM(paopt%dir)//'Users')
  IF (i /= 0) CALL printerror('Could not change group on new Users file.',0)
  i = safe_shell('/bin/chmod 440 '//TRIM(paopt%dir)//'Users')
  IF (i /= 0) CALL printerror('Could not change permissions on new Users file.',0)

END SUBROUTINE modify_account

!---------------------------------------------------------------------
SUBROUTINE view_accounts(paopt,padm)
  !PURPOSE = View user account entries
  !STATUS = Complete and tested
  USE convert
  !USE cina_core
  USE cinaadmin_core
  USE io
  IMPLICIT NONE
  TYPE(cinaadm),INTENT(IN)        :: padm(10)
  TYPE(p_admin_options),INTENT(IN) :: paopt
  TYPE(cinaadm)                   :: padmtemp
  INTEGER(KIND=4)                  :: i,j,n,c
  CHARACTER(LEN=200)               :: tmps
  LOGICAL(KIND=1)                  :: match

  OPEN(3,FILE=TRIM(paopt%dir)//'Users',ACTION='READ',POSITION='REWIND')

  ! Find the number of users in padm
  j = 0
  DO i = 1, UBOUND(padm,1)
     IF (LEN_TRIM(padm(i)%USER) > 0) j = i
  END DO

  ! Print header
  WRITE(*,'(A,T48,A)') 'Username','Timeout'
  WRITE(*,'(A)') REPEAT('_',79)

  n = 0
  c = 0      ! c counts the total number of users
  DO WHILE (n == 0)
     READ(3,'(A)',IOSTAT=n) tmps
     IF (n > 0) THEN
        WRITE(tmps,'(A,I0,A)') 'Error ',n,' while reading account file.'
        CALL printerror(tmps,1)
     END IF
     IF (n == 0) THEN
        c = c + 1   ! Increment counter
        ! Decode entry
        padmtemp = decode_user_entry(tmps)
        ! Check username
        IF (j == 0) THEN
           match = .TRUE.
        ELSE
           match = .FALSE.
        END IF
        DO i = 1, j
           IF (padmtemp%USER == padm(i)%USER) match = .TRUE.
        END DO
        IF (match) WRITE(*,'(A,T52,I3)') padmtemp%USER,padmtemp%TIMEOUT

     END IF
  END DO

  WRITE(*,'(A,I0)') 'Number of user accounts: ',c
  
  CLOSE(3)
END SUBROUTINE view_accounts

!---------------------------------------------------------------------
SUBROUTINE auto_logout(paopt,prgm_name)
  !PURPOSE = Logout sessions that have been inactive for the timeout period
  !STATUS = 
  USE IFLPORT
  USE cina_core
  USE cinaadmin_core
  USE io
  IMPLICIT NONE
  TYPE(p_admin_options),INTENT(IN) :: paopt
  TYPE(cinaadm)                   :: padm(100)   !MAKE THIS TO MATCH THE NUMBER OF USER ACCOUNTS
  TYPE(cina_common)               :: pcom
  CHARACTER(LEN=*),INTENT(IN)      :: prgm_name
  INTEGER(KIND=4)                  :: i,j,n,t,user_number,badnum
  CHARACTER(LEN=1200)              :: tmps
  ! Make baduser and badid big enough to hold the max number of expected sessions (including the inactive ones)
  CHARACTER(LEN=50)                :: user,baduser(500)
  CHARACTER(LEN=20)                :: id,badid(500)
  CHARACTER(LEN=8)                 :: id_date
  CHARACTER(LEN=10)                :: id_time
  LOGICAL(KIND=4)                  :: loop,good

  ! Just in case report_error is called, provide an action so we know that this function started the error
  pcom%ACTION = 'AUTO-LOGOUT'

  ! Load Users file into padm array
  OPEN(3,FILE=TRIM(paopt%dir)//'Users',ACTION='READ',POSITION='REWIND')

  user_number = 0     ! Counter of the number of entries in padm
  n = 0
    DO WHILE (n == 0)
     READ(3,'(A)',IOSTAT=n) tmps
     IF (n > 0) THEN
        WRITE(tmps,'(A,I0,A)') 'Error ',n,' while reading account file.'
        CALL report_error(TRIM(tmps),'File input/output',pcom,1)
     END IF
     IF (n == 0) THEN
        user_number = user_number + 1   ! Increment counter
        IF (user_number > UBOUND(padm,1)) CALL report_error(  &
             'padm is too small in auto_logout','Developer Reminder',pcom,1)
        ! Decode entry
        padm(user_number) = decode_user_entry(tmps)
     END IF
  END DO
  CLOSE(3)

  ! Go through OpenIDs file and check for inactive sessions
  OPEN(23,FILE=TRIM(paopt%dir)//'OpenIDs',ACTION='READ',STATUS='UNKNOWN',IOSTAT=i)
  IF (i /= 0)  CALL report_error('Could not open ID file','File input/output',pcom,1)
    
  badnum = 0
  loop = .TRUE.
  DO WHILE (loop)
     ! Read in id, date, time, 
     READ(23,'(A20,A8,A6)',IOSTAT=i,ADVANCE='NO') id,id_date,id_time
     READ(23,'(A)',IOSTAT=i) user
     IF (i < 0) THEN
        ! End of file reached
        loop = .FALSE.
        id = ''
     ELSE IF (i > 0) THEN
        CLOSE(23)
        WRITE(user,'(A,I0,A)') 'Could not read ID file (',i,')'
        CALL report_error(TRIM(user),'File input/output',pcom,1)
     END IF

     ! Find the user's timeout
     i = 0
     good = .TRUE.
     DO WHILE (good .AND. loop)
        i = i + 1
        IF (i > user_number) THEN
           good = .FALSE.
           i = 0
        END IF
        IF (padm(i)%USER == user) good = .FALSE.
     END DO

     IF (i > 0) THEN
        ! Check if timeout has passed
        good = chk_timeout(id_date,id_time,padm(i)%TIMEOUT)
        IF (good) THEN
           badnum = badnum + 1
           baduser(badnum) = padm(i)%USER
           badid(badnum) = id
        END IF
     END IF
     
  END DO
  CLOSE(23)

  ! Logout inactive sessions
  DO i = 1, badnum
     pcom%ID = badid(i)
     pcom%USER = baduser(i)
     WRITE(*,'(4A)') 'Logging out user "',TRIM(baduser(i)),'" with ID ',badid(i)
     CALL CGI_logout(pcom)
  END DO

  WRITE(*,'(A,I0)') 'Number of inactive sessions ',badnum
  
  ! Log this information
  WRITE(tmps,'(A,I0,A)') 'Logging out ',badnum,' inactive sessions ('
  DO i = 1, badnum
     tmps = TRIM(tmps) // TRIM(baduser(i)) // ','
  END DO
  tmps = TRIM(tmps(:LEN_TRIM(tmps)-1)) // ')'

  i = use_syslog(TRIM(prgm_name),'NOTICE',TRIM(tmps),TRIM(SYSLOG_PATH))

  ! Find all tmp directories not listed in OpenIDs file and put list in baduser array
  ! First save list of directories
  i = safe_shell('/bin/ls -1 '//TRIM(TEMP_PATH)//' > dir')
  IF (i /= 0) THEN
     WRITE(*,'(A,I0)') 'Error ',i
     CALL report_error('Error getting directory listing.','External fileio',pcom,0)
  END IF

  ! Read dir file into baduser array
  OPEN(31,FILE='dir',ACTION='READ',IOSTAT=i)
  IF (i /= 0) THEN
     WRITE(*,'(A,I0)') 'Error ',i
     CALL report_error('Error opening directory listing.','File input/output',pcom,0)
  END IF
  n = 0
  badnum = 0
  DO WHILE (n == 0)
     READ(31,'(A)',IOSTAT=n) baduser(badnum+1)
     IF (n > 0) THEN
        WRITE(*,'(A,I0)') 'Error ',i
        CALL report_error('Error reading directory listing.','File input/output',pcom,1)
     END IF
     IF (n == 0) THEN
        badnum = badnum + 1
        IF (badnum+1 > UBOUND(baduser,1)) THEN
           n = -33         ! Force loop to stop
           CALL report_error('baduser is too small in auto_logout','Developer Reminder',pcom,0)
        END IF
     END IF
  END DO
  CLOSE(31)

  ! Del dir file just created
  i = safe_shell('/bin/rm -f dir')
  IF (i /= 0) THEN
     WRITE(*,'(A,I0)') 'Error ',i
     CALL report_error('Error removing temp file "dir"','External fileio',pcom,0)
  END IF

  ! Load active IDs into badid
  OPEN(23,FILE=TRIM(paopt%dir)//'OpenIDs',ACTION='READ',STATUS='UNKNOWN',IOSTAT=i)
  IF (i /= 0)  CALL report_error('Could not open ID file','File input/output',pcom,1)
  n = 0
  t = 0     ! t is the number of active IDs
  DO WHILE (n == 0)
     READ(23,'(A20)',IOSTAT=n) badid(t+1)
     IF (n > 0) THEN
        WRITE(*,'(A,I0)') 'Error ',i
        CALL report_error('Error reading active IDs','File input/output',pcom,0)
     END IF
     IF (n == 0) THEN
        t = t + 1
        IF (t > UBOUND(badid,1)) THEN
           n = -33
           CALL report_error('badid is too small in auto_logout','Developer Reminder',pcom,0)
        END IF
     END IF
  END DO
  CLOSE(23)

  ! Go through all tmp directories and search for any tmp directories not listed in OpenIDs file
  pcom%USER = ''
  user_number = 0   ! Counts the number of unaccounted directories
  DO i = 1, badnum
     n = -1      ! n == 0 if listed in OpenIDs file, nonzero otherwise
     j = 1       ! index to active tmp directory
     DO WHILE (j <= t)
        IF (badid(j) == baduser(i)) THEN
           n = 0
           j = t + 1
        END IF
        j = j + 1
     END DO
     IF (n == -1) THEN
        user_number = user_number + 1
        ! These directories are not listed in OpenIDs file and should be removed.
        WRITE(*,'(2A)') 'Removing unaccounted temp directory ',TRIM(baduser(i))
        pcom%ID = TRIM(baduser(i))

        ! Kill any running programs for this ID
	! this has to have t arguments or the ifort compiler barfs
	! so passing the current directory, don't know what else to do
        CALL kill_bg_proc(pcom,".")
    
        ! Delete all temp files
        n = safe_shell('/bin/rm -fR '//TRIM(TEMP_PATH)//pcom%ID)
        IF (n /= 0) THEN
           WRITE(*,'(A,I0)') 'Error ',n
           CALL report_error('Could not delete temp files. Error '//TRIM(pcom%ID), &
                'External fileio',pcom,0)
        END IF
     END IF
  END DO

  WRITE(*,'(A,I0)') 'Number of tmp directories not in OpenIDs file ',user_number

  IF (user_number > 0) THEN
     WRITE(tmps,'(A,I0)') 'Removing ',user_number,' unaccounted tmp directories'
     i = use_syslog(prgm_name,'NOTICE',TRIM(tmps),TRIM(SYSLOG_PATH))
  END IF
END SUBROUTINE auto_logout

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
