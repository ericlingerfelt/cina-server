MODULE large_data

  USE constants

  USE fileio

  USE IFLPORT



  ! MODULE USAGE SUMMARY

  ! Step 1) Putting data into file

  ! Step 2) Compressing data

  ! Step 3) Sending compressed data

  ! Step 4) Deleting compressed data



  ! Putting data into a file

  ! Step 1.1   Use ldf_open

  ! Step 1.2   Write to file

  ! Step 1.3   Use ldf_close



  ! Compressing data

  ! Step 2.1   Use ldf_compress



  ! Sending compressed data as CGI response

  ! Step 3.1   Use ldf_send



  ! Sending compressed data to email

  ! Step 3.1   Use ldf_email



  ! Delete uncompressed and compressed files

  ! Step 4.1   Use ldf_delete



  ! Schedule deletion uncompressed and compressed files

  ! Step 4.1   Use ldf_schedule_del



  PUBLIC



  CHARACTER(LEN=MAX_PATH_LEN),PARAMETER :: DEFAULT_PATH_PREFIX = '/tmp/large_data/'

  INTEGER(KIND=4),PARAMETER        :: DEFAULT_UNIT = 77



  TYPE,PUBLIC                      :: large_data_file

     LOGICAL(KIND=4)               :: UfileOpen = .FALSE.

     LOGICAL(KIND=4)               :: Compressed = .FALSE.

     INTEGER(KIND=4)               :: status = 0

     INTEGER(KIND=4)               :: unit = -1

     INTEGER(KIND=4)               :: Usize = -1

     INTEGER(KIND=4)               :: Csize = -1

     CHARACTER(LEN=MAX_PATH_LEN)   :: Ufile = ''  ! Uncompressed filename

     CHARACTER(LEN=MAX_PATH_LEN)   :: Cfile = ''  ! Compressed filename

     CHARACTER(LEN=MAX_PATH_LEN)   :: path_prefix = ''

     CHARACTER(LEN=200)            :: err_str = ''

     CHARACTER(LEN=30)             :: err_sub = ''

  END TYPE large_data_file



  ! List of values for ldf%status

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_OK = 0

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_BAD_UNIT = 1

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_FIO_OPEN = 2

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_UFILE_LIMIT = 3

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_MAKE_PATH = 4

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_FIO_CLOSE = 5

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_UFILE_CLOSED = 6

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_UFILE_OPEN = 7

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_COMPRESS = 8

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_INVALID_STRING = 9

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_UFILE_EMPTY = 10

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_FIO_STAT = 11

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_IN_DEV = 12

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_FIO_DEL = 13

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_CFILE_EMPTY = 14

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_BAD_COMP_VALUE = 15

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_BAD_USIZE_VALUE = 16

  INTEGER(KIND=4),PARAMETER   :: LDF_ERR_SEND = 17



CONTAINS

  ! All procedures should check if ldf%status == 0 before doing anything else

  ! and call ldf_set_error before returning



  !======================================================================

  SUBROUTINE ldf_set_error(ldf,value,sub,str,int)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf

    INTEGER(KIND=4),INTENT(IN)          :: value,int

    CHARACTER(LEN=*),INTENT(IN)         :: sub,str

    OPTIONAL                            :: str,int



    ldf%status = value

    ldf%err_sub = sub



    SELECT CASE (value)

    CASE (LDF_ERR_OK)

       ldf%err_str = 'No error'

    CASE (LDF_ERR_BAD_UNIT)

       ldf%err_str = 'Unit number is invalid'

    CASE (LDF_ERR_FIO_OPEN)

       ldf%err_str = 'Error opening file'

    CASE (LDF_ERR_UFILE_LIMIT)

       ldf%err_str = 'Reached limit trying to find available Ufile'

    CASE (LDF_ERR_MAKE_PATH)

       ldf%err_str = 'Unable to make path for tmp file'

    CASE (LDF_ERR_FIO_CLOSE)

       ldf%err_str = 'Error closing file'

    CASE (LDF_ERR_UFILE_CLOSED)

       ldf%err_str = 'Subroutine requires Ufile to be open'

    CASE (LDF_ERR_UFILE_OPEN)

       ldf%err_str = 'Subroutine requires Ufile to be closed'

    CASE (LDF_ERR_COMPRESS)

       ldf%err_str = 'Error compressing file'

    CASE (LDF_ERR_INVALID_STRING)

       ldf%err_str = 'Invalid characters were found in string'

    CASE (LDF_ERR_UFILE_EMPTY)

       ldf%err_str = 'Subroutine requires non-empty Ufile'

    CASE (LDF_ERR_FIO_STAT)

       ldf%err_str = 'Error getting file size'

    CASE (LDF_ERR_IN_DEV)

       ldf%err_str = 'Subroutine in development'

    CASE (LDF_ERR_FIO_DEL)

       ldf%err_str = 'Error deleting file'

    CASE (LDF_ERR_CFILE_EMPTY)

       ldf%err_str = 'Subroutine requires non-empty Cfile'

    CASE (LDF_ERR_BAD_COMP_VALUE)

       ldf%err_str = 'ldf%Compressed flag is incorrect'

    CASE (LDF_ERR_BAD_USIZE_VALUE)

       ldf%err_str = 'ldf%Usize should not be negative'

    CASE (LDF_ERR_SEND)

       ldf%err_str = 'Error sending compressed file'

    CASE DEFAULT

       ldf%err_str = 'Undefined error'

    END SELECT



    ldf%err_str = TRIM(sub) // ': ' // ldf%err_str



    IF (PRESENT(str)) ldf%err_str = TRIM(ldf%err_str) // ': ' // TRIM(str)

    IF (PRESENT(int)) WRITE(ldf%err_str,'(A,A,I0,A)') TRIM(ldf%err_str),' (',int,')'

  END SUBROUTINE ldf_set_error



  SUBROUTINE ldf_open(ldf,unit,id)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf

    INTEGER(KIND=4),INTENT(IN)          :: unit

    CHARACTER(LEN=20),INTENT(IN)        :: id

    CHARACTER(LEN=MAX_PATH_LEN)         :: saved_path

    CHARACTER(LEN=100)                  :: tmps

    INTEGER(KIND=4)                     :: s,i,l,v

    REAL(KIND=8)                        :: r



    IF (ldf%status /= LDF_ERR_OK) RETURN

    IF (unit < 0 .OR. unit > 254) THEN

       CALL ldf_set_error(ldf,LDF_ERR_BAD_UNIT,'ldf_open',int=unit)

       RETURN

    END IF



    IF (ldf%UfileOpen) THEN

       CALL ldf_set_error(ldf,LDF_ERR_UFILE_OPEN,'ldf_open')

       RETURN

    END IF



    ! One can't compress an opened file so erase Cfile to cause errors if Cfile is used

    ldf%Cfile = ''

    ldf%Compressed = .FALSE.

    ldf%Usize = -1

    ldf%Csize = -1



    IF (ldf%Ufile == '') THEN

       ! Create a filename instead of using user-defined



       CALL RANDOM_SEED()



       ! Get default path prefix if necessary

       saved_path = ldf%path_prefix

       IF (saved_path == '') saved_path = DEFAULT_PATH_PREFIX



       ! Make sure it ends with a slash

       s = LEN_TRIM(ldf%path_prefix)

       IF (saved_path(s:s) /= '/') saved_path(s+1:s+1) = '/'



       ! Check saved_path for bad characters

       IF (ldf_chk_str(ldf,saved_path)) RETURN



       ! Make sure saved_path exists

       s = SYSTEM('/bin/mkdir -p ''' // TRIM(saved_path) // '''')

       IF (s /= 0) THEN

          CALL ldf_set_error(ldf,LDF_ERR_MAKE_PATH,'ldf_open',int=s)

          RETURN

       END IF

	   ! Add id to filename

	   saved_path = TRIM(saved_path) // id

       ! Add time to filename

       CALL DATE_AND_TIME(TIME=tmps)

       ! saved_path = TRIM(saved_path) // tmps(1:6) // tmps(8:10)

        saved_path = TRIM(saved_path) // tmps(1:6)

       ! Add date to filename

       CALL DATE_AND_TIME(tmps)

       saved_path = TRIM(saved_path) // tmps(1:8)

       s = 0



       DO WHILE (ldf%Ufile == '')

          ldf%Ufile = saved_path

          ! Add 10 random characters

          !l = LEN_TRIM(ldf%Ufile)

          !DO i = 1, 10

             !CALL RANDOM_NUMBER(r)

             !v = NINT(r * 61)   ! Number from 0 to 61

             !IF (v <= 9) THEN

                !v = v + 48  ! Turn into ASCII code of digit

             !ELSE IF (v <= 35) THEN

                !v = v + 55  ! Turn into ASCII code of uppercase letter

             !ELSE

                !v = v + 61  ! Turn into ASCII code of lowercase letter

             !END IF



             !ldf%Ufile(l+i : l+i) = ACHAR(v)

          !END DO



          ! Check that Ufile isn't used already

          !IF (file_exists(ldf%Ufile,tmps)) ldf%Ufile = ''



          !s = s + 1

          !IF (s > 1000) THEN

             !CALL ldf_set_error(ldf,LDF_ERR_UFILE_LIMIT,'ldf_open',int=s)

             !RETURN

          !END IF

       END DO

    END IF



    OPEN(UNIT=unit, FILE=ldf%Ufile, ACTION='WRITE', IOSTAT=s)



    IF (s == 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_OK,'ldf_open')

       ldf%UfileOpen = .TRUE.

       ldf%unit = unit

    ELSE

       CALL ldf_set_error(ldf,LDF_ERR_FIO_OPEN,'ldf_open',int=s)

    END IF

  END SUBROUTINE ldf_open



  !======================================================================

  SUBROUTINE ldf_close(ldf)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf

    INTEGER(KIND=4)                     :: s,stats(12)



    IF (ldf%status /= LDF_ERR_OK) RETURN

    IF (ldf%unit < 0 .OR. ldf%unit > 254) THEN

       CALL ldf_set_error(ldf,LDF_ERR_BAD_UNIT,'ldf_close',int=ldf%unit)

       RETURN

    END IF

    IF (.NOT. ldf%UfileOpen) THEN

       CALL ldf_set_error(ldf,LDF_ERR_UFILE_CLOSED,'ldf_close')

       RETURN

    END IF



    CLOSE(UNIT=ldf%unit, IOSTAT=s)

    IF (s /= 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_FIO_CLOSE,'ldf_close',int=s)

    END IF

    ldf%UfileOpen = .FALSE.

    ldf%Compressed = .FALSE.



    ! Get Ufile size

    s = STAT(ldf%Ufile,stats)

    IF (s /= 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_FIO_STAT,'ldf_close',int=s)

       RETURN

    END IF

    ldf%Usize = stats(8)

    ldf%Csize = -1



    CALL ldf_set_error(ldf,LDF_ERR_OK,'ldf_close')

  END SUBROUTINE ldf_close



  !======================================================================

  SUBROUTINE ldf_compress(ldf,whole_dir)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf

    LOGICAL(KIND=4),INTENT(IN)          :: whole_dir

    OPTIONAL                            :: whole_dir

    LOGICAL(KIND=4)                     :: dir

    INTEGER(KIND=4)                     :: s,stats(12)


    IF (ldf%status /= LDF_ERR_OK) RETURN

    IF (ldf%UfileOpen) THEN

       CALL ldf_set_error(ldf,LDF_ERR_UFILE_OPEN,'ldf_compress')

       RETURN

    END IF

    IF (ldf%Ufile == '') THEN

       CALL ldf_set_error(ldf,LDF_ERR_UFILE_EMPTY,'ldf_compress')

       RETURN

    END IF

    dir = .FALSE.

    IF (PRESENT(whole_dir)) THEN

       IF (whole_dir) dir = .TRUE.

    END IF

    IF (ldf%Cfile == '') ldf%Cfile = TRIM(ldf%Ufile) // '.zip'

    ! Check ldf%Ufile and ldf%Cfile for invalid strings

    IF (ldf_chk_str(ldf,ldf%Ufile) .OR. ldf_chk_str(ldf,ldf%Cfile)) RETURN

    ! Set ldf%Usize if necessary

    IF (ldf%Usize < 0 .AND. .NOT. dir) THEN

       OPEN(UNIT=DEFAULT_UNIT, FILE=ldf%Ufile, ACTION='READ', IOSTAT=s)

       IF (s /= 0) THEN

          CALL ldf_set_error(ldf,LDF_ERR_FIO_OPEN,'ldf_compress',ldf%Ufile,int=s)

          RETURN

       END IF



       CLOSE(UNIT=DEFAULT_UNIT, IOSTAT=s)

       IF (s /= 0) THEN

          CALL ldf_set_error(ldf,LDF_ERR_FIO_CLOSE,'ldf_compress',ldf%Ufile,int=s)

       END IF



       ! Get Ufile size

       s = STAT(ldf%Ufile,stats)

       IF (s /= 0) THEN

          CALL ldf_set_error(ldf,LDF_ERR_FIO_STAT,'ldf_compress',ldf%Ufile,int=s)

          RETURN

       END IF

       ldf%Usize = stats(8)

    END IF



    IF (dir) THEN

       s = SYSTEM('/usr/bin/zip -j ''' // TRIM(ldf%Cfile) // ''' ' // &

            TRIM(ldf%Ufile) // ' > /dev/null')

    ELSE

       s = SYSTEM('/usr/bin/zip -j ''' // TRIM(ldf%Cfile) // ''' ''' // &

            TRIM(ldf%Ufile) // ''' > /dev/null')

    END IF



    IF (s /= 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_COMPRESS,'ldf_compress',ldf%Cfile,int=s)

       RETURN

    END IF



    ldf%Compressed = .TRUE.



    OPEN(UNIT=DEFAULT_UNIT, FILE=ldf%Cfile, ACTION='READ', IOSTAT=s)

    IF (s /= 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_FIO_OPEN,'ldf_compress',ldf%Cfile,int=s)

       RETURN

    END IF



    ! Get Cfile size

    s = STAT(ldf%Cfile,stats)

    IF (s /= 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_FIO_STAT,'ldf_compress',ldf%Cfile,int=s)

       RETURN

    END IF

    ldf%Csize = stats(8)



    CLOSE(UNIT=DEFAULT_UNIT, IOSTAT=s)

    IF (s /= 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_FIO_CLOSE,'ldf_compress',ldf%Cfile,int=s)

    END IF



    CALL ldf_set_error(ldf,LDF_ERR_OK,'ldf_compress')

  END SUBROUTINE ldf_compress



  !======================================================================

  ! Return TRUE and set error status if string has a single quote

  FUNCTION ldf_chk_str(ldf,string)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf

    CHARACTER(LEN=*),INTENT(IN)         :: string

    LOGICAL(KIND=4)                     :: ldf_chk_str



    ldf_chk_str = .TRUE.

    IF (ldf%status /= LDF_ERR_OK) RETURN



    IF (INDEX(string,'''') > 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_INVALID_STRING,'ldf_chk_str', &

            ''' found in ' // TRIM(string))

    ELSE

       ldf_chk_str = .FALSE.

    END IF

  END FUNCTION ldf_chk_str



  !======================================================================

  SUBROUTINE ldf_send(ldf,del)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf

    LOGICAL(KIND=4),INTENT(IN)          :: del

    OPTIONAL                            :: del

    LOGICAL(KIND=4)                     :: l

    INTEGER(KIND=4)                     :: i,v

    CHARACTER(LEN=1)                    :: ldf_dec(0:3)



    IF (ldf%status /= LDF_ERR_OK) RETURN

    IF (ldf%UfileOpen) THEN

       CALL ldf_set_error(ldf,LDF_ERR_UFILE_OPEN,'ldf_send')

       RETURN

    END IF

    IF (ldf%Cfile == '') THEN

       CALL ldf_set_error(ldf,LDF_ERR_CFILE_EMPTY,'ldf_send')

       RETURN

    END IF

    IF (.NOT. ldf%Compressed) THEN

       CALL ldf_set_error(ldf,LDF_ERR_BAD_COMP_VALUE,'ldf_send')

       RETURN

    END IF

    IF (ldf%Usize < 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_BAD_USIZE_VALUE,'ldf_send')

       RETURN

    END IF



    ! Send 4 byte header

    DO i = 0, 3

       v = MOD(ldf%Usize / (2 ** (8 * i)), 256)

       ldf_dec(i) = ACHAR(v)

    END DO

    WRITE(6,'(4a)')(ldf_dec(i),i=0,3)



    ! Pretend to close stdout so that 4 byte header will be flushed to stdout

    ! The FLUSH subroutine and COMMITQQ function did not work

    CLOSE(6)



    ! Send compressed file

    ! Check ldf%Ufile and ldf%Cfile for invalid strings

    IF (ldf_chk_str(ldf,ldf%Cfile)) RETURN



    i = SYSTEM('/bin/cat ''' // TRIM(ldf%Cfile) // '''')

    IF (i /= 0) THEN

       CALL ldf_set_error(ldf,LDF_ERR_SEND,'ldf_send',int=i)

       RETURN

    END IF



    IF (PRESENT(del)) THEN

       IF (del) CALL ldf_delete(ldf)

    END IF



    IF (ldf%status == 0) CALL ldf_set_error(ldf,LDF_ERR_OK,'ldf_send')







  END SUBROUTINE ldf_send



  !======================================================================

  SUBROUTINE ldf_email(ldf)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf



    IF (ldf%status /= LDF_ERR_OK) RETURN

    CALL ldf_set_error(ldf,LDF_ERR_IN_DEV,'ldf_email')

  END SUBROUTINE ldf_email



  !======================================================================

  SUBROUTINE ldf_delete(ldf)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf

    INTEGER(KIND=4)                     :: i



    IF (ldf%status /= LDF_ERR_OK) RETURN

    IF (ldf%UfileOpen) THEN

       CALL ldf_set_error(ldf,LDF_ERR_UFILE_OPEN,'ldf_delete')

       RETURN

    END IF

    IF (ldf%Cfile == '' .AND. ldf%Compressed) THEN

       CALL ldf_set_error(ldf,LDF_ERR_CFILE_EMPTY,'ldf_delete')

       RETURN

    END IF



    ! Delete compressed files because Ufile may include an * that includes the compressed file

    IF (ldf%Cfile /= '') THEN

       ! Delete compressed file



       ! Check saved_path for bad characters

       IF (ldf_chk_str(ldf,ldf%Cfile)) RETURN



       i = DELFILESQQ(ldf%Cfile)

       IF (i /= 1) THEN

          CALL ldf_set_error(ldf,LDF_ERR_FIO_DEL,'ldf_delete',TRIM(ldf%Cfile))

          RETURN

       END IF

    END IF



    IF (ldf%Ufile /= '') THEN

       ! Delete uncompressed file



       ! Check saved_path for bad characters

       IF (ldf_chk_str(ldf,ldf%Ufile)) RETURN



       i = DELFILESQQ(ldf%Ufile)

       IF (i < 1) THEN

          CALL ldf_set_error(ldf,LDF_ERR_FIO_DEL,'ldf_delete',TRIM(ldf%Ufile))

          RETURN

       END IF

    END IF



    ldf%Compressed = .FALSE.

    ldf%unit = -1

    ldf%Usize = -1

    ldf%Csize = -1

    ldf%Ufile = ''

    ldf%Cfile = ''

    CALL ldf_set_error(ldf,LDF_ERR_OK,'ldf_delete')

  END SUBROUTINE ldf_delete



  !======================================================================

  SUBROUTINE ldf_schedule_del(ldf)

    IMPLICIT NONE

    TYPE(large_data_file),INTENT(INOUT) :: ldf



    IF (ldf%status /= LDF_ERR_OK) RETURN

    CALL ldf_set_error(ldf,LDF_ERR_IN_DEV,'ldf_schedule_del')

  END SUBROUTINE ldf_schedule_del



  !======================================================================

END MODULE large_data

