MODULE dir_list
  ! Module that serves as an interface to dirlist.c

  INTERFACE 
     FUNCTION opendir(path)
       INTEGER  :: opendir
       CHARACTER :: path
     END FUNCTION opendir

     FUNCTION read_dir_entry(name)
       INTEGER  :: read_dir_entry
       CHARACTER :: name
     END FUNCTION read_dir_entry

     FUNCTION closedir()
       INTEGER  :: closedir
     END FUNCTION closedir
  END INTERFACE

  ! Meanings of return values from dirlist.c
  INTEGER,PARAMETER :: SUCCESS =      0
  INTEGER,PARAMETER :: OPEN_ERROR =  -1
  INTEGER,PARAMETER :: READ_ERROR =  -2
  INTEGER,PARAMETER :: CLOSE_ERROR = -3
  INTEGER,PARAMETER :: UNKNOWN =      1
  INTEGER,PARAMETER :: FILE =         2
  INTEGER,PARAMETER :: DIR =          3

END MODULE 

MODULE foreachlib_core
  USE dir_list
  USE cina_core
  USE rate_man_core
  USE IFLPORT

  PUBLIC
  INTEGER,PARAMETER :: MAX_NAME_LEN = 300
  LOGICAL           :: movetotrash = .FALSE.
  CHARACTER(LEN=20),PRIVATE :: fel_group
  CHARACTER(LEN=20) :: fel_user = 'foreachlib'

CONTAINS
  SUBROUTINE fel_in_dir(path,group,trash_path)
    IMPLICIT NONE
    ! path should be directory containing libraries
    CHARACTER(LEN=*),INTENT(IN)  :: path,group,trash_path
    CHARACTER(LEN=MAX_NAME_LEN)  :: name
    INTEGER                      :: s,i
    LOGICAL                      :: insuite,pfl ! print first library
    TYPE(cina_common)           :: pcom

    fel_group = group
    pcom%IP = 'foreachlib'
    pcom%ACTION = 'foreachlib'
    pcom%USER = fel_user

    ! Load library list
    CALL clear_rlib_list()
    CALL read_rlib_list(group,pcom)

    ! Get list of libraries and call fel for each library
    s = opendir(path)
    IF (s /= SUCCESS) THEN
       !WRITE(0,'(A)') 'Error opening '//TRIM(path)//' to get library list'
       RETURN
    END IF

    IF (group == 'USER') THEN
       pfl = .FALSE.
    ELSE
       pfl = .TRUE.
    END IF

    DO WHILE (s >= SUCCESS)
       s = read_dir_entry(name)
       IF (s == DIR .AND. name /= '.' .AND. name /= '..') THEN
          CALL find_rlib_list_index(name,lib_index=i)
          IF (i < 1) THEN
             insuite = .FALSE.
          ELSE
             insuite = .TRUE.
          END IF
          IF (.NOT. pfl) THEN
             PRINT *,''
             pfl = .TRUE.
          END IF
          CALL fel(path,name,pcom,insuite,trash_path)
       END IF
    END DO

    s = closedir()
    IF (s /= SUCCESS) WRITE(0,'(A)') 'Warning: Error closing '//TRIM(path)
  END SUBROUTINE fel_in_dir

  SUBROUTINE fel(path,lib,pcom,insuite,trash_path)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)  :: path,lib,trash_path
    LOGICAL,INTENT(IN)           :: insuite
    INTEGER(KIND=4)              :: s
    TYPE(cina_common),INTENT(IN) :: pcom
 
    IF (fel_user == 'foreachlib') THEN
       WRITE(*,'(3A)') 'Processing library "',TRIM(lib),'"'
    ELSE
       WRITE(*,'(4A)') 'Processing library "',TRIM(lib),'" for user ',TRIM(fel_user)
    END IF
    
    IF (.NOT. insuite) THEN
       IF (movetotrash .AND. trash_path /= '') THEN
          WRITE(0,'(T3,3A)') 'CAUTION! ',TRIM(lib),' is not in rlib_list and was moved to trash'
          s = SYSTEM('rm -rf '''//TRIM(trash_path)//'/'//TRIM(lib)//'''')
          s = SYSTEM('mkdir -p '''//TRIM(trash_path)//'''')
          s = SYSTEM('mv -f '''//TRIM(path)//'/'//TRIM(lib)//''' '''//TRIM(trash_path)//'''')
       ELSE
          WRITE(0,'(T3,3A)') 'CAUTION! ',TRIM(lib),' is not in rlib_list and was skipped'
       END IF
       RETURN
    END IF

    ! Put code that operates on a library below here
    CALL find_links(lib,pcom)
  END SUBROUTINE fel

  SUBROUTINE find_links(lib,pcom)
    USE constants
    ! Prints each link from a library
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)  :: lib

    INTEGER(KIND=4)              :: z,a,a_index,num_rids,i
    INTEGER(KIND=2)              :: iso_list(0:MAX_Z,MAX_ISO)
    CHARACTER(LEN=MAX_RID_LEN)   :: rids(MAX_RATES)
    CHARACTER(LEN=MAX_RLIB_LEN)  :: orig_lib
    CHARACTER(LEN=2000)          :: lib_list
    LOGICAL(KIND=4)              :: link

    lib_list = ','
    CALL read_rlib_list('PUBLIC',pcom)
    CALL clear_riso_list()
    CALL read_rlib_iso_list(lib,pcom,iso_list)

    DO z = 0, MAX_Z
       a_index = 1
       a = iso_list(z,a_index)
       DO WHILE (a > 0)
          CALL read_rid_list(lib,pcom,z,a,rids,num_rids,buffer=.FALSE.)

          DO i = 1, num_rids
             CALL correct_rid(rids(i),lib,link)

             IF (link) THEN
                IF (.NOT. rid_exist(rids(i),pcom)) WRITE(*,'(A)') &
                     ' Unresolved link: "'//TRIM(rids(i))//'"'

                CALL decode_rid(rids(i),orig_lib)
                ! Print out rate id
                !WRITE(*,'(A)') TRIM(rids(i))

                ! Check if orig_lib is in lib_list
                IF (INDEX(lib_list,','//TRIM(orig_lib)//',') <= 0) THEN
                   ! Add orig_lib to lib_list
                   lib_list = TRIM(lib_list) // TRIM(orig_lib) // ','
                END IF
             END IF
          END DO
          a_index = a_index + 1
          IF (a_index <= MAX_ISO) THEN
             a = iso_list(z,a_index)
          ELSE
             a = -1
          END IF
       END DO
    END DO

    ! Remove initial and final commas in lib_list
    lib_list = lib_list(2:LEN_TRIM(lib_list)-1)

    IF (lib_list /= '') WRITE(*,'(A)') ' "'//TRIM(lib)//'" needs '//TRIM(lib_list)
  END SUBROUTINE find_links
END MODULE foreachlib_core


PROGRAM foreachlib
  USE IFLPORT
  USE foreachlib_core
  USE cina_core
  USE convert
  IMPLICIT NONE
  CHARACTER(LEN=300)  :: path
  CHARACTER(LEN=300)  :: user
  CHARACTER(LEN=10000):: user_list
  INTEGER             :: s
  LOGICAL             :: loop

  ! Move hidden libraries to trash?
  movetotrash = .FALSE.

  ! path should be set to /var/www/cina_files
  path = '/var/www/' // CINA_NAME
  PRINT '(A)', 'main path is "' // TRIM(path) // '"'

  ! Process all PUBLIC libraries
  CALL fel_in_dir(TRIM(path)//'/PUBLIC/rate_libs','PUBLIC','')

  ! Process all SHARED libraries
  CALL fel_in_dir(TRIM(path)//'/SHARED/rate_libs','SHARED','')

  ! Process all USER libraries
  ! Get list of users
  s = opendir(TRIM(path)//'/USER')
  IF (s /= SUCCESS) THEN
     WRITE(0,'(A)') 'Error opening '//TRIM(path)//'/USER to get USER list'
     STOP
  END IF

  user_list = ''
  DO WHILE (s >= SUCCESS)
     s = read_dir_entry(user)
     IF (s == DIR .AND. user /= '.' .AND. user /= '..') &
          user_list = TRIM(user_list) // ACHAR(9) // TRIM(user)
  END DO
  user_list = user_list(2:) ! Remove initial tab

  s = closedir()
  IF (s /= SUCCESS) WRITE(0,'(A)') 'Warning: Error closing '//TRIM(path)

  loop = .TRUE.
  DO WHILE (loop)
     loop = next_in_list(user, user_list, ACHAR(9))
     fel_user = user
     CALL fel_in_dir(TRIM(path)//'/USER/'//TRIM(user)//'/rate_libs','USER', &
          TRIM(path)//'/trash/'//TRIM(user)//'/rate_libs')
  END DO
END PROGRAM foreachlib
