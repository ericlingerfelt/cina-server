MODULE rategen
  !DESC = This module contains procedures and data types for the Rate Generator CGI actions for the cina program
  !
  ! Procedures in this module are:
  !
  ! FUNCTION rategen_ver                        Version of this file
  ! SUBROUTINE read_input(pcom)                 Perform the READ INPUT action
  ! SUBROUTINE preprocess(pcom)                 Perform the PREPROCESSING action
  ! SUBROUTINE generate_rate(pcom)              Perform the GENERATE RATE action
  ! SUBROUTINE abort_rate_gen(pcom)             Perform the ABORT RATE GENERATION action
  ! SUBROUTINE gen_parm_fmt(pcom)               Perform the GENERATE PARAMETER FORMAT action
  ! SUBROUTINE rategen_update(pcom)             Perform the RATE GENERATION UPDATE action
  ! SUBROUTINE rategen_output(pcom)             Perform the RATE GENERATION OUTPUT action
  ! SUBROUTINE abort_rate_parm(pcom)            Perform the ABORT RATE PARAMETERIZATION action
  ! SUBROUTINE generate_parm(pcom)              Perform the PARAMETERIZE RATE action
  ! SUBROUTINE rateparm_update(pcom)            Perform the RATE PARAMETERIZATION UPDATE action
  ! SUBROUTINE rateparm_output(pcom)            Perform the RATE PARAMETERIZATION OUTPUT action
  ! SUBROUTINE create_tgrid(pcom)               Create temperature grid for rate generator
  ! SUBROUTINE read_trange(file,tmin,tmax)      Return tmin and tmax from trange file
  ! SUBROUTINE inverse_parm(pcom)               Perform the INVERSE PARAMETERS action

  USE constants
  USE cina_core
  ! Use Intel Fortran compiler portability function interface module
  USE IFLPORT
  
  ! By default all procedures and global variables are public
  PUBLIC
  
CONTAINS
  !---------------------------------------------------------------------
  FUNCTION rategen_ver()
    !PURPOSE = Return the cvs revision number for this file
    !STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)                :: rategen_ver
    CHARACTER(LEN=20),PARAMETER      :: RATEGEN_VERSION = '$Revision: 1.1.1.1 $'
    
    rategen_ver = RATEGEN_VERSION(12:LEN_TRIM(RATEGEN_VERSION)-2)
    
  END FUNCTION rategen_ver
  
  !---------------------------------------------------------------------
  FUNCTION rategen_temp_path(pcom)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=200)               :: rategen_temp_path

    rategen_temp_path = TRIM(TEMP_PATH)//pcom%ID//'/rategen/'
  END FUNCTION rategen_temp_path

  !---------------------------------------------------------------------
  SUBROUTINE read_input(pcom)
    !PURPOSE = Perform the READ INPUT action
    !STATUS = Complete and tested
    USE fileio
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    TYPE(fileiooptions)              :: fopt
    INTEGER(KIND=4)                  :: s = 0,i,n,points
    CHARACTER(LEN=200)               :: dir,tmps,tmps2
    CHARACTER(LEN=32767)             :: str
    CHARACTER(LEN=LEN(pcom%NOTES))   :: notes
    CHARACTER(LEN=4)                 :: fmt
    REAL(KIND=8)                     :: array(array_len,4),tmpr,tmpr2
    LOGICAL(KIND=4)                  :: e = .FALSE.
    
    dir = rategen_temp_path(pcom)

    ! Remove commas from FORMAT to use with inputread
    ! Note: no more than 9 columns can be used
    fmt = pcom%FORMAT(1:1)//pcom%FORMAT(3:3)//pcom%FORMAT(5:5)//pcom%FORMAT(7:7)

    ! Kill any background processes
    CALL kill_bg_proc(pcom,dir)
    
    ! Remove any files that exist and ignore any errors
    s = safe_shell('/bin/rm -fr '//TRIM(dir))
    IF (s /= 0) THEN
       WRITE(str,'(I0)') s
       CALL report_error('Could not remove old ID dir.  Error '//TRIM(str),'External fileio',pcom,1)
    END IF

    ! Make the directory for this ID
    s = safe_shell('/bin/mkdir -p '''//TRIM(dir)//'''')
    IF (s /= 0) THEN
       WRITE(str,'(I0)') s
       CALL report_error('Could not make ID dir.  Error '//TRIM(str),'External fileio',pcom,1)
    END IF
    
    tmps2 = ''
    IF (pcom%REACTION_TYPE /= '') tmps2 = '-rt ''' // TRIM(pcom%REACTION_TYPE) // ''''

    ! If no NOTES were given, add something so that inputread won't ask for them
    IF (LEN_TRIM(pcom%NOTES) == 0) THEN
       notes = 'blank'
    ELSE
       notes = pcom%NOTES
    END IF
    
    IF (pcom%FILENAME) THEN
       tmps = TRIM(PUB_PATH)//TRIM(get_file())
       !print '(A)','CAUTION=filename='//TRIM(tmps)
    ELSE
       tmps = TRIM(dir)//'user.txt'
       !PRINT '(A)','CAUTION=new file='//TRIM(tmps)
       ! Save filename to disk
       OPEN(3,FILE=tmps,IOSTAT=s,ACTION='WRITE')
       IF (s /= 0) CALL report_error('Error creating user data file on server','File input/output',pcom,1)

       ! Put file in str
       str = get_file()

       ! Replace all CR, LF sequences with linefeeds
       s = INDEX(str,ACHAR(13)//ACHAR(10))
       DO WHILE (s > 0)
          str = str(:s-1) // ACHAR(10) // str(s+2:)
          s = INDEX(str,ACHAR(13)//ACHAR(10))
       END DO

       ! Convert all remaining carriage returns to linefeeds
       s = INDEX(str,ACHAR(13))
       DO WHILE (s > 0)
          !print '(A,I0)','CAUTION=CR @ ',s
          str(s:s) = ACHAR(10)
          s = INDEX(str,ACHAR(13))
       END DO

       WRITE(3,'(A)',IOSTAT=s) TRIM(str)
       IF (s /= 0) CALL report_error('Error writing user data file on server','File input/output',pcom,1)
       CLOSE(3)
    END IF

!!$    print '(A)','CAUTION=command = '//TRIM(BIN_PATH)//'inputread '//TRIM(tmps)// &
!!$         ' '''//TRIM(pcom%TYPE)//''' -r '''//TRIM(pcom%REACTION)//''' -n '//       &
!!$         TRIM(notes)//' -o '//TRIM(dir)//'f.inrd '//'-f LOOSE_COLUMN -p 1 -fo '//  &
!!$         fmt//' '//TRIM(cina_decode(pcom%BODY))//' &> '//TRIM(dir)//'redir'

    ! Execute inputread program and redirect to a file.
    s = safe_shell(TRIM(BIN_PATH)//'inputread '//TRIM(tmps)// &
         ' '''//TRIM(pcom%TYPE)//''' -r '''//TRIM(pcom%REACTION)//''' -n '//       &
         TRIM(notes)//' -o '//TRIM(dir)//'f.inrd '//'-f LOOSE_COLUMN -p 1 -fo '//  &
         fmt//' '//TRIM(tmps2)//' '//TRIM(cina_decode(pcom%BODY))//' &> '//TRIM(dir)//'redir')
    IF (s /= 0) THEN
       WRITE(str,'(I0)') s
       CALL report_error('inputread exited with error '//TRIM(str),'External program',pcom,1)
    END IF
    
    ! Open inputread output that was redirected to a file
    e = .FALSE.
    OPEN(3,FILE=TRIM(dir)//'redir',ACTION='READ',STATUS='OLD',IOSTAT=i)
    IF (i == 0) THEN
       DO WHILE (i == 0)
          READ(3,'(A)',IOSTAT=i) str
          IF (i == 0) THEN
             ! Print out string if it is an error or caution
             IF (str == 'CAUTION: Assuming standard input rate units') THEN
                ! Don't pass this caution for now
             ELSE IF (str(1:9) == 'CAUTION: ') THEN
                CALL report_error(TRIM(str(10:)),'External program',pcom,2)
             ELSE IF (str(1:7) == 'ERROR: ') THEN
                CALL report_error(TRIM(str(8:)),'External program',pcom,4)
                e = .TRUE.
             END IF
          END IF
       END DO
       CLOSE(3)
       IF (i > 0) THEN
          WRITE(tmps,'(I0)') i
          CALL report_error('Could not read inputread screen output. Error '// &
               TRIM(tmps),'File input/output',pcom,0)
       END IF
    ELSE 
       WRITE(tmps,'(I0)') i
       CALL report_error('Could not open inputread screen output. Error '  &
            //TRIM(tmps),'File input/output',pcom,0)
    END IF

    IF (e) THEN
       CALL report_error('An error occurred while reading the input file from the server.','External program',pcom,1)
    ELSE
       !If no error
       ! Open .inrd file
       fopt%data_format = 'INPUTREAD'
       fopt%file_in = TRIM(dir)//'f.inrd'
       CALL file2array(array,points,fopt)
       
       ! Print out min and max values
       SELECT CASE (pcom%TYPE)
       CASE ('S(E) ')
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_MIN=',MINVAL(array(1:points,1))
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_MAX=',MAXVAL(array(1:points,1))
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_ERROR_MIN=',MINVAL(array(1:points,2))
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_ERROR_MAX=',MAXVAL(array(1:points,2))
          WRITE(*,'(1P,A,E15.7E3)') 'S_FACTOR_MIN=',MINVAL(array(1:points,3))
          WRITE(*,'(1P,A,E15.7E3)') 'S_FACTOR_MAX=',MAXVAL(array(1:points,3))
          WRITE(*,'(1P,A,E15.7E3)') 'S_FACTOR_ERROR_MIN=',MINVAL(array(1:points,4))
          WRITE(*,'(1P,A,E15.7E3)') 'S_FACTOR_ERROR_MAX=',MAXVAL(array(1:points,4))
       CASE ('CS(E) ')
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_MIN=',MINVAL(array(1:points,1))
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_MAX=',MAXVAL(array(1:points,1))
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_ERROR_MIN=',MINVAL(array(1:points,2))
          WRITE(*,'(1P,A,E15.7E3)') 'ENERGY_ERROR_MAX=',MAXVAL(array(1:points,2))
          WRITE(*,'(1P,A,E15.7E3)') 'CROSS_SECTION_MIN=',MINVAL(array(1:points,3))
          WRITE(*,'(1P,A,E15.7E3)') 'CROSS_SECTION_MAX=',MAXVAL(array(1:points,3))
          WRITE(*,'(1P,A,E15.7E3)') 'CROSS_SECTION_ERROR_MIN=',MINVAL(array(1:points,4))
          WRITE(*,'(1P,A,E15.7E3)') 'CROSS_SECTION_ERROR_MAX=',MAXVAL(array(1:points,4))
       CASE ('R(T) ')
          tmpr = MINVAL(array(1:points,1))
          tmpr2 = MAXVAL(array(1:points,1))
          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_MIN=',tmpr
          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_MAX=',tmpr2
          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_ERROR_MIN=',MINVAL(array(1:points,2))
          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_ERROR_MAX=',MAXVAL(array(1:points,2))
          WRITE(*,'(1P,A,E15.7E3)') 'RATE_MIN=',MINVAL(array(1:points,3))
          WRITE(*,'(1P,A,E15.7E3)') 'RATE_MAX=',MAXVAL(array(1:points,3))
          WRITE(*,'(1P,A,E15.7E3)') 'RATE_ERROR_MIN=',MINVAL(array(1:points,4))
          WRITE(*,'(1P,A,E15.7E3)') 'RATE_ERROR_MAX=',MAXVAL(array(1:points,4))

          ! Save TMIN and TMAX into f.trange file
          OPEN(52,FILE=TRIM(dir)//'f.trange',IOSTAT=i,ACTION='WRITE')
          IF (i /= 0) THEN
             WRITE(tmps,'(A,I0)') 'Can not open temperature range file.  IOSTAT=',i
             CALL report_error(TRIM(tmps),'File input/output',pcom,1)
          END IF

          WRITE(52,*,IOSTAT=i) tmpr,tmpr2
          IF (i /= 0) THEN
             WRITE(tmps,'(A,I0)') 'Can not write to temperature range file.  IOSTAT=',i
             CALL report_error(TRIM(tmps),'File input/output',pcom,1)
          END IF
          CLOSE(52)

          ! Save array to f.getparm.in file
          OPEN(52,FILE=TRIM(dir)//'f.getparm.in',IOSTAT=i,ACTION='WRITE')
          IF (i /= 0) THEN
             WRITE(tmps,'(A,I0)') 'Can not open f.getparm.in file.  IOSTAT=',i
             CALL report_error(TRIM(tmps),'File input/output',pcom,1)
          END IF

          DO i = 1, points
             WRITE(52,'(1P,2E18.10E3)',IOSTAT=n) array(i,1),array(i,3)
             IF (n /= 0) THEN
                WRITE(tmps,'(A,I0)') 'Could not save f.getparm.in.  IOSTAT=',n
                CALL report_error(TRIM(tmps),'File input/output',pcom,0)
                STOP
             END IF
          END DO
          CLOSE(52)
       CASE DEFAULT
          CALL report_error('Unknown case '//TRIM(pcom%TYPE)//' in read_input', &
               'Developer Reminder',pcom,1)
       END SELECT

       WRITE(*,'(A,I0)') 'POINTS=',points
       
       ! Print array to screen
       WRITE(*,'(A,1P,4E15.7E3)',ADVANCE='NO') 'DATA=',array(1,:)
       DO i = 2, points
          WRITE(*,'(A,1P,4E15.7E3)',ADVANCE='NO') ACHAR(9),array(i,:)
       END DO
       WRITE(*,'(A)') ''  ! Print newline after parameters

       ! Save reaction string to f.reaction file
       OPEN(14,FILE=TRIM(dir)//'f.reaction',IOSTAT=i,ACTION='WRITE')
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Can not open reaction file.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF

       tmps = getreac_str(fopt%r,1)
       WRITE(14,'(A)',IOSTAT=i) TRIM(tmps)
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Can not write to reaction file.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF
       CLOSE(14)

       ! Save reaction notes to f.notes file
       OPEN(15,FILE=TRIM(dir)//'f.notes',IOSTAT=i,ACTION='WRITE')
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Can not open notes file.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF

       WRITE(15,'(A)',IOSTAT=i) TRIM(cina_decode(pcom%NOTES))
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Can not write to notes file.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF
       CLOSE(15)

    END IF
  END SUBROUTINE read_input
  
  !---------------------------------------------------------------------
  SUBROUTINE preprocess(pcom)
    !PURPOSE = Perform the PREPROCESSING action
    !STATUS = Complete and tested
    USE convert
    USE fileio
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    TYPE(fileiooptions)              :: fopt
    TYPE(inputcheckresult)           :: results(10)
    INTEGER(KIND=4)                  :: s,i
    REAL(KIND=8)                     :: tmin,tmax
    CHARACTER(LEN=200)               :: dir,tmps
    
    ! Kill any background processes
    CALL kill_bg_proc(pcom,dir)
    
    dir = rategen_temp_path(pcom)

    s = safe_shell('/bin/ls '//TRIM(dir)//'f.inrd &> /dev/null')
    IF (s /= 0) THEN
       CALL report_error('Preprocessing can not be performed because the read input'// &
            ' stage was not completed successfully. ','Improper usage',pcom,1)
    END IF

    ! Delete f.inck and f.innr files if they exist, ignore errors
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.inck '//TRIM(dir)//'f.innr '//TRIM(dir)//'trange')
    
    ! Make string of options to pass to inputcheck
    tmps = ''
    IF (.NOT. pcom%POSITIVE_CHK) tmps = '-pos'
    IF (.NOT. pcom%SINGLE_CHK) tmps = TRIM(tmps) // ' -sv'
    IF (.NOT. pcom%RANGE_CHK) tmps = TRIM(tmps) // ' -r'
    IF (.NOT. pcom%CONTINUITY_CHK) tmps = TRIM(tmps) // ' -c'
    IF (.NOT. pcom%ERROR_CHK) tmps = TRIM(tmps) // ' -ev'
    IF (.NOT. pcom%REACTION_CHK) tmps = TRIM(tmps) // ' -rc'
    
    ! Execute inputcheck program and redirect to a file.
    s = safe_shell(TRIM(BIN_PATH)//'inputcheck '//TRIM(dir)//'f.inrd '//TRIM(tmps)// &
         ' &> '//TRIM(dir)//'redir')
    IF (s /= 0) THEN
       WRITE(tmps,'(I0)') s
       CALL report_error('inputcheck exited with error '//TRIM(tmps),'External program',pcom,1)
    END IF
    
    ! Check for .inck file
    s = safe_shell('/bin/ls '//TRIM(dir)//'f.inck &> /dev/null')
    IF (s /= 0) THEN
       CALL report_error('Preprocessing output file was not found. ','External fileio',pcom,1)
    END IF
    
    ! Open the .inck file
    fopt%data_format = 'INPUTCHECK'
    fopt%file_in = TRIM(dir)//'f.inck'
    CALL file2icresults(results,s,fopt)        ! s is the number of results (tests) 
    
    DO i = 1, s
       SELECT CASE (results(i)%test)
       CASE ('Positive ')
          WRITE(*,'(2A)') 'POSITIVE_CHK=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'POSITIVE_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Single valued ')
          WRITE(*,'(2A)') 'SINGLE_CHK=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'SINGLE_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Energy range ')
          WRITE(*,'(2A)') 'RANGE_CHK_ENERGY=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'RANGE_ENERGY_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('S-factor range ')
          WRITE(*,'(2A)') 'RANGE_CHK_S_FACTOR=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'RANGE_S_FACTOR_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Cross section range ')
          WRITE(*,'(2A)') 'RANGE_CHK_CROSS_SECTION=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'RANGE_CROSS_SECTION_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Temperature range ')
          WRITE(*,'(2A)') 'RANGE_CHK_TEMP=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'RANGE_TEMP_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Rate range ')
          WRITE(*,'(2A)') 'RANGE_CHK_RATE=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'RANGE_RATE_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Continuity ')
          WRITE(*,'(2A)') 'CONTINUITY_CHK=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'CONTINUITY_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Error / Value ')
          WRITE(*,'(2A)') 'ERROR_CHK=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'ERROR_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Reaction ')
          WRITE(*,'(2A)') 'REACTION_CHK=',TRIM(results(i)%result)
          IF (results(i)%result == 'FAILED') THEN
             WRITE(*,'(2A)') 'REACTION_REASON=',TRIM(results(i)%reason)
          END IF
       CASE ('Narrow resonance ')
          WRITE(*,'(2A)') 'NARROW_RESONANCE_CHK=',TRIM(results(i)%result)
          WRITE(*,'(2A)') 'NARROW_RESONANCE_REASON=',TRIM(results(i)%reason)
       END SELECT
    END DO

    SELECT CASE (lowercase(fopt%data_type))
    CASE ('s(e) ','cs(e) ')
       ! Execute temperature program and redirect to a file
       s = safe_shell(TRIM(BIN_PATH)//'temp_range '//TRIM(dir)//'f.inrd &> '//TRIM(dir)//'trange')
       IF (s /= 0) THEN
          WRITE(tmps,'(I0)') s
          CALL report_error('temp_range exited with error '//TRIM(tmps),'External program',pcom,1)
       END IF
       
       ! Get tmin and tmax from trange file
       CALL read_trange(pcom,tmin,tmax)
    CASE ('r(t) ')
       CALL read_trange(pcom,tmin,tmax,'f.trange')
    END SELECT

    WRITE(*,'(A,1P,E15.7E3)') 'TMIN=',tmin
    WRITE(*,'(A,1P,E15.7E3)') 'TMAX=',tmax

  END SUBROUTINE preprocess
  
  !---------------------------------------------------------------------
  SUBROUTINE generate_rate(pcom)
    !PURPOSE = Perform the generate rate action
    !STATUS = Complete and tested but user defined temperature grids are not implemented
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i,s,p
    CHARACTER(LEN=200)               :: dir
    CHARACTER(LEN=10)                :: tmps

    dir = rategen_temp_path(pcom)
    CALL kill_bg_proc(pcom,dir)

    ! Exit if the f.inrd file does not exist
    s = safe_shell('/bin/ls '//TRIM(dir)//'f.inrd &> /dev/null')
    IF (s /= 0) THEN
       WRITE(*,'(A)') 'START=FAIL'
       WRITE(*,'(A)') 'REASON=The rate can not be generated until input has been read.'
       STOP
    END IF

    ! Remove f.rategen if it exists and ignore any errors
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.rategen '//TRIM(dir)//'tgrid.txt')

    ! Remove f.getparm.in if it exists and ignore any errors
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.getparm.in')

    ! Create temperature grid
    CALL create_tgrid(pcom)

    i = safe_shell(TRIM(BIN_PATH)//'rategen '//TRIM(dir)//'f.inrd '//   &
         TRIM(dir)//'tgrid.txt '//TRIM(dir)//'f.rategen &> '//          &
         TRIM(dir)//'redir & echo $! &> '//TRIM(dir)//'pid')
    IF (i /= 0) THEN
       WRITE(tmps,'(I0)') i
       CALL report_error('Error '//TRIM(tmps)//' while starting rate generator.','External program',pcom,1)
    END IF

    ! Create file that stores the cursor for the update process
    OPEN(UNIT=11,FILE=TRIM(dir)//'cur',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not create update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF

    ! The cursor starts at 0, meaning 0 characters have been read in so far
    WRITE(11,'(I0)',IOSTAT=i) 0
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write to update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF
    CLOSE(11)

    WRITE(*,'(A)') 'START=SUCCESS'
  END SUBROUTINE generate_rate

  !---------------------------------------------------------------------
  SUBROUTINE abort_rate_gen(pcom)
    !PURPOSE = Perform the ABORT RATE GENERATION action
    !STATUS = Complete and tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i
    CHARACTER(LEN=200)               :: dir

    dir = rategen_temp_path(pcom)

    ! Kill any programs running in the background
    CALL kill_bg_proc(pcom,dir)

    ! Delete rate_gen screen output
    i = safe_shell('/bin/rm -f '//TRIM(dir)//'redir')

    WRITE(*,'(A)') 'STOP=SUCCESS'

  END SUBROUTINE abort_rate_gen

  !---------------------------------------------------------------------
  SUBROUTINE gen_parm_fmt(pcom)
    !PURPOSE = Perform the GENERATE PARAMETER FORMAT action
    !STATUS = Complete but not fully tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i,s
    CHARACTER(LEN=200)               :: dir,tmps
    REAL(KIND=8)                     :: qvalue
    LOGICAL(KIND=4)                  :: e

    dir = rategen_temp_path(pcom)

    ! Remove f.fmtoutput if it exists and ignore any errors
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.fmtoutput')
    
    ! Exit if no format was specified
    IF (LEN_TRIM(pcom%BODY) == 0) THEN
       WRITE(*,'(A)') 'TEXT='
       STOP
    END IF

    ! Get the PID of the rate parameterizer
    s = bg_pid(pcom,dir)
    IF (s <= 0) CALL report_error('The rate parameterizer was not started.', &
         'Improper usage',pcom,1)
    ! See if it is running
    i = KILL(s,18)  ! Send the process the SIGCONT signal (Continue process if stopped)
    SELECT CASE (i)
    CASE (0)
       ! process is running
       CALL report_error('Rate parameterizer has not completed yet.','Improper usage',pcom,1)
    CASE (-1)
       ! process is not running
       ! do nothing but continue
    CASE DEFAULT
       WRITE(tmps,'(I0)') i
       CALL report_error('Unknown response '//TRIM(tmps)//' to signal','Developer Reminder',pcom,1)
    END SELECT

    ! Try to read in f.qvalue
    tmps = ''
    OPEN(44,FILE=TRIM(dir)//'f.qvalue',IOSTAT=i,ACTION='READ')
    IF (i == 0) THEN
       ! If the file was opened, read in qvalue
       READ(44,'(G)',IOSTAT=i) qvalue
       IF (i == 0) THEN
          WRITE(tmps,'(A,G21.13)') '-q ',qvalue
       END IF
    END IF

    ! Put extra command line options in tmps
    tmps = TRIM(tmps) // ' -desc ''Generated by user ' // TRIM(pcom%USER) // &
         ' with astrodata suite from http://www.nucastrodata.org/'' '
    tmps = TRIM(tmps) // ' -b ' // TRIM(pcom%USER)

    ! Check if f.invparm file exists
    s = safe_shell('/bin/ls '//TRIM(dir)//'f.invparm &> /dev/null')
    IF (s == 0) tmps = TRIM(tmps)//' -inv '//TRIM(dir)//'f.invparm'

    ! Execute fmtoutput program and redirect to a file
    s = safe_shell(TRIM(BIN_PATH)//'fmtoutput '''//TRIM(cina_decode(pcom%BODY))//  &
         ''' '//TRIM(dir)//'f.fmtoutput '//TRIM(dir)//'f.rateparm -ir '//                &
         TRIM(dir)//'f.inrd '//TRIM(tmps)//' &> '//TRIM(dir)//'redir')
    IF (s /= 0) THEN
       WRITE(tmps,'(I0)') s
       CALL report_error('fmtoutput exited with error '//TRIM(tmps),'External program',pcom,1)
    END IF

    ! Open fmtoutput output that was redirected to a file
    e = .FALSE.
    OPEN(3,FILE=TRIM(dir)//'redir',ACTION='READ',STATUS='OLD',IOSTAT=i)
    IF (i == 0) THEN
       DO WHILE (i == 0)
          READ(3,'(A)',IOSTAT=i) tmps
          IF (i == 0) THEN
             ! Print out string if it is an error or caution
             IF (tmps(1:9) == 'CAUTION: ') THEN
                CALL report_error(TRIM(tmps(10:)),'External program',pcom,2)
             ELSE IF (tmps(1:7) == 'ERROR: ') THEN
                CALL report_error(TRIM(tmps(8:)),'External program',pcom,4)
                e = .TRUE.
             END IF
          END IF
       END DO
       CLOSE(3)
       IF (i > 0) THEN
          WRITE(tmps,'(I0)') i
          CALL report_error('Could not read fmtoutput screen output. Error '// &
               TRIM(tmps),'File input/output',pcom,0)
       END IF
    ELSE 
       WRITE(tmps,'(I0)') i
       CALL report_error('Could not open fmtoutput screen output. Error '  &
            //TRIM(tmps),'File input/output',pcom,0)
    END IF
    
    IF (.NOT. e) THEN 
       ! If no error
       ! Open f.fmtoutput file
       OPEN(3,FILE=TRIM(dir)//'f.fmtoutput',ACTION='READ',STATUS='OLD',IOSTAT=i)
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not open fmtoutput file.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF
       
       WRITE(*,'(A)',ADVANCE='NO') 'TEXT='
       ! Loop until the end of the file
       DO WHILE (i == 0)
          READ(3,'(A)',IOSTAT=i) tmps
          IF (i == 0) THEN
             WRITE(*,'(2A)',ADVANCE='NO') TRIM(tmps),ACHAR(8)
          ELSE IF (i > 0) THEN
             WRITE(tmps,'(A,I0)') 'Could not read fmtoutput file.  IOSTAT=',i
             CALL report_error(TRIM(tmps),'File input/output',pcom,1)
          END IF
       END DO

       CLOSE(3)
    END IF

    ! Put newline on end of TEXT= line
    WRITE(*,'(A)') ''

  END SUBROUTINE gen_parm_fmt

  !---------------------------------------------------------------------
  SUBROUTINE rategen_update(pcom)
    !PURPOSE = Perform the RATE GENERATION UPDATE action
    !STATUS = Complete and tested
    IMPLICIT NONE
    ! Use print_long_string if MAXCHAR is above 1024
    INTEGER(KIND=4),PARAMETER        :: MAXCHAR = 1000
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i,p,c,e
    CHARACTER(LEN=1)                 :: chr
    CHARACTER(LEN=200)               :: dir,tmps
    CHARACTER(LEN=MAXCHAR)           :: buffer,save_buffer
    LOGICAL(KIND=1)                  :: loop

    dir = rategen_temp_path(pcom)

    ! Get the PID of the rate generator
    p = bg_pid(pcom,dir)
    IF (p <= 0) CALL report_error('The rate generator was not started.','Improper usage',pcom,1)
    ! See if it is running
    i = KILL(p,18)  ! Send the process the SIGCONT signal (Continue process if stopped)
    SELECT CASE (i)
    CASE (0)
       ! process is running
       WRITE(*,'(A)') 'GENERATION=RUNNING'
    CASE (-1)
       ! process is not running
       WRITE(*,'(A)') 'GENERATION=COMPLETE'
    CASE DEFAULT
       WRITE(tmps,'(I0)') i
       CALL report_error('Unknown response '//TRIM(tmps)//' to signal','Developer Reminder',pcom,1)
    END SELECT

    ! Read in the number of characters already sent
    OPEN(UNIT=11,FILE=TRIM(dir)//'cur',IOSTAT=i,ACTION='READ')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    READ(11,'(I)',IOSTAT=i) c
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    CLOSE(11)

    ! Read in rate generation screen output
    OPEN(12,FILE=TRIM(dir)//'redir',IOSTAT=i,ACTION='READ',POSITION='APPEND')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open rate generation screen output.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF

    e = FTELL(12)                ! The total number of characters in the file (at this time)

    IF ((e == 0) .OR. (c == e)) THEN
       WRITE(*,'(A)') 'TEXT_SKIPPED=N'
       WRITE(*,'(A)') 'TEXT='
       RETURN
    END IF

    ! Find the last new line character before position e (because lines that aren't completed won't be sent)
    p = c
    !IF (p < 1) p = 1
    i = FSEEK(12,p,0)                ! Position file to previous cursor
    IF (i /= 0) CALL report_error('Could not seek in rate generation screen output (1).', &
         'File input/output',pcom,1)
    loop = .TRUE.
    ! Store the position of the last newline in p
    DO WHILE (loop)
       READ(12,'(A)',IOSTAT=i) chr
       IF (i > 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read rate generation screen output.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF
       IF (i == 0) THEN
          i = FTELL(12)
          IF (i <= e) THEN
             p = i
          ELSE
             loop = .FALSE.
          END IF
       ELSE
          loop = .FALSE.
       END IF
       !print *,i,p,e,c
    END DO

    ! Treat p as the last position to use
    IF (p > 0) e = p

    IF (e - c <= MAXCHAR) THEN
       WRITE(*,'(A)') 'TEXT_SKIPPED=N'
    ELSE
       WRITE(*,'(A)') 'TEXT_SKIPPED=Y'
       ! Find position to start printing text by looking in the last MAXCHAR characters for the first newline
       p = e - MAXCHAR
       !IF (p < 1) p = 1
       i = FSEEK(12,p,0)                ! Position file to just before previous cursor
       IF (i /= 0) CALL report_error('Could not seek in rate generation screen output (2).', &
            'File input/output',pcom,1)
       ! Find the position after the first newline character
       READ(12,'(A1)',IOSTAT=i) chr
       c = FTELL(12) - 1
    END IF
    !print *,c
    i = FSEEK(12,c,0)                ! Position file to just after previous cursor
    IF (i /= 0) CALL report_error('Could not seek in rate generation screen output (3).', &
         'File input/output',pcom,1)

    save_buffer = ''
    DO WHILE (c < e)
       READ(12,'(A)',IOSTAT=i) buffer
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read rate generation screen output.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF

       c = FTELL(12)
       save_buffer = TRIM(save_buffer) // TRIM(buffer) // ACHAR(8)
    END DO
    CLOSE(12)

    ! Use print_long_string if MAXCHAR is above 1024
    WRITE(*,'(2A)') 'TEXT=',TRIM(save_buffer)

    ! Record new cursor
    OPEN(11,FILE=TRIM(dir)//'cur',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Can not open update file (2).  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    WRITE(11,'(I0)',IOSTAT=i) e

    CLOSE(11)
  END SUBROUTINE rategen_update

  !---------------------------------------------------------------------
  SUBROUTINE rategen_output(pcom)
    !PURPOSE = Perform the RATE GENERATION OUTPUT action
    !STATUS = Complete and tested
    USE fileio
    USE math
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    TYPE(fileiooptions)              :: opt
    CHARACTER(LEN=200)               :: dir,tmps
    REAL(KIND=8)                     :: array(array_len,4)
    INTEGER(KIND=4)                  :: i,points,n

    dir = rategen_temp_path(pcom)

    ! Get the PID of the rate generator
    n = bg_pid(pcom,dir)
    IF (n <= 0) CALL report_error('The rate generator was not started.','Improper usage',pcom,1)
    ! See if it is running
    i = KILL(n,18)  ! Send the process the SIGCONT signal (Continue process if stopped)
    SELECT CASE (i)
    CASE (0)
       ! process is running
       CALL report_error('Rate generator has not completed yet.','Improper usage',pcom,1)
    CASE (-1)
       ! process is not running
       ! do nothing but continue
    CASE DEFAULT
       WRITE(tmps,'(I0)') i
       CALL report_error('Unknown response '//TRIM(tmps)//' to signal','Developer Reminder',pcom,1)
    END SELECT
    
    ! Check for f.rategen file
    i = safe_shell('/bin/ls '//TRIM(dir)//'f.rategen &> /dev/null')
    IF (i /= 0) THEN
       CALL report_error('Rate generator output file was not found. ','Developer Reminder',pcom,1)
    END IF
    
    ! Set options to use fileio module to read in rate generation output file
    opt%file_in = TRIM(dir)//'f.rategen'
    opt%data_format = 'RATEGEN'
    opt%data_type = 'R(T)'
    opt%t_units_in = 1D0
    opt%r_units_in = 1D0
    opt%fmt_options = '1230'   ! Temp is column 1, Rate in col 2, E-gam in col 3
    CALL file2array(array,points,opt)
    ! The number of interpolated points is in array(1,4)

    WRITE(*,'(A,1P,E15.7E3)') 'T_MIN=',MINVAL(array(1:points,1))
    WRITE(*,'(A,1P,E15.7E3)') 'T_MAX=',MAXVAL(array(1:points,1))
    WRITE(*,'(A,1P,E15.7E3)') 'R_MIN=',MINVAL(array(1:points,2))
    WRITE(*,'(A,1P,E15.7E3)') 'R_MAX=',MAXVAL(array(1:points,2))
    WRITE(*,'(A,I0)') 'NUM_INTERPOLATED_POINTS=',INT(array(1,4))
    ! Put dR/dT in array(:,4)
    array(1:points-1,4) = numderiv(array(1:points,1),array(1:points,2))
    WRITE(*,'(A,1P,E15.7E3)') 'DR_DT_MIN=',MINVAL(array(1:points-1,4))
    WRITE(*,'(A,1P,E15.7E3)') 'DR_DT_MAX=',MAXVAL(array(1:points-1,4))
    WRITE(*,'(A)') 'INT_TECHNIQUE=Riemann sum and trapezoidal rule'
    WRITE(*,'(A,I0)') 'POINTS=',points

    WRITE(*,'(A,1P,2E15.7E3)',ADVANCE='NO') 'DATA=',array(1,1:2)
    DO i = 2, points
       WRITE(*,'(A,1P,2E15.7E3)',ADVANCE='NO') ACHAR(9),array(i,1:2)
    END DO
    WRITE(*,'(A)') '' ! Print newline

    ! Save array to new file that getparm can read
    OPEN(UNIT=11,FILE=TRIM(dir)//'f.getparm.in',IOSTAT=n,ACTION='WRITE')
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not create f.getparm.in.  IOSTAT=',n
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF

    DO i = 1, points
       WRITE(11,'(1P,2E18.10E3)',IOSTAT=n) array(i,1:2)
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not save f.getparm.in.  IOSTAT=',n
          CALL report_error(TRIM(tmps),'File input/output',pcom,0)
          STOP
       END IF
    END DO
    CLOSE(11)

    ! Extract Q-value from rategen output
    OPEN(UNIT=11,FILE=TRIM(dir)//'redir',IOSTAT=n,ACTION='READ')
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open rategen screen output.  IOSTAT=',n
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF
    
    READ(11,'(A)',IOSTAT=n) tmps
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read rategen screen output.  IOSTAT=',n
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF
    CLOSE(11)

    ! Check that tmps has Q-value
    IF (tmps(1:19) == ' q value (in MeV) =') THEN
       ! Save q value in tmps
       tmps = tmps(20:)
       ! Save q value to f.qvalue
       OPEN(11,FILE=TRIM(dir)//'f.qvalue',IOSTAT=n,ACTION='WRITE')
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not open q-value file.  IOSTAT=',n
          CALL report_error(TRIM(tmps),'File input/output',pcom,0)
          STOP
       END IF
    
       WRITE(11,'(A)',IOSTAT=n) TRIM(tmps)
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not write q-value.  IOSTAT=',n
          CALL report_error(TRIM(tmps),'File input/output',pcom,0)
          STOP
       END IF
       CLOSE(11)
    END IF
  END SUBROUTINE rategen_output

  !---------------------------------------------------------------------
  SUBROUTINE abort_rate_parm(pcom)
    !PURPOSE = Perform the ABORT RATE PARAMETERIZATION action
    !STATUS = Complete and tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i
    CHARACTER(LEN=200)               :: dir

    dir = rategen_temp_path(pcom)

    ! Kill any programs running in the background
    CALL kill_bg_proc(pcom,dir)

    ! Delete rate_gen screen output
    i = safe_shell('/bin/rm -f '//TRIM(dir)//'redir.rateparm')

    WRITE(*,'(A)') 'STOP=SUCCESS'

  END SUBROUTINE abort_rate_parm

  !---------------------------------------------------------------------
  SUBROUTINE generate_parm(pcom)
    !PURPOSE = Perform the PARAMETERIZE RATE action
    !STATUS = Complete and tested except user defined temperature grids are not implemented
    USE constants
    USE convert
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i,s,p
    REAL(KIND=8)                     :: tmin,tmax,params(MAX_A)
    CHARACTER(LEN=200)               :: dir,opt
    CHARACTER(LEN=500)               :: tmps
    CHARACTER(LEN=30)                :: num
    LOGICAL(KIND=4)                  :: loop

    dir = rategen_temp_path(pcom)
    CALL kill_bg_proc(pcom,dir)
    opt = ''

    ! Remove output files
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.rateparm')
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.inverse '//TRIM(dir)//'f.invparm')

    ! Exit if the temp dir does not exist
    s = safe_shell('/bin/ls '//TRIM(dir)//'f.getparm.in &> /dev/null')
    IF (s /= 0) THEN
       WRITE(*,'(A)') 'START=FAIL'
       WRITE(*,'(A)') 'REASON=Parameters can not be produced until rate is generated.'
       STOP
    END IF

    ! Remove f.rategen if it exists and ignore any errors
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.rateparm')

    ! Check that TMIN and TMAX are valid
    CALL read_trange(pcom,tmin,tmax,'f.trange')

    ! Put starting parameters into params()
    tmps = cina_decode(pcom%NOTES)
    loop = .TRUE.
    IF (tmps == '') loop = .FALSE.
    p = 0
    DO WHILE (loop .AND. p < MAX_A)
       loop = next_in_list(num,tmps,',')
       p = p + 1
       i = 1
       CALL nextnum(num,params(p),i,s)
       ! Check status
    END DO

    IF (p > 6) THEN
       OPEN(14,FILE=TRIM(dir)//'f.startparm',IOSTAT=i,ACTION='WRITE')
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Can not open starting parameters file.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF
       WRITE(14,'(I0)',IOSTAT=i) p
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Can not write starting parameter count.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF

       DO s = 1, p
          WRITE(14,'(E20.10)',IOSTAT=i) params(s)
          IF (i /= 0) THEN
             WRITE(tmps,'(A,I0)') 'Can not write starting parameters.  IOSTAT=',i
             CALL report_error(TRIM(tmps),'File input/output',pcom,1)
          END IF
       END DO
       CLOSE(14)

       opt = TRIM(opt) // ' -sf ''' // TRIM(dir) // 'f.startparm'''
    END IF

    ! Get reaction string to use as ID
    OPEN(14,FILE=TRIM(dir)//'f.reaction',IOSTAT=i,ACTION='READ')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Can not open reaction file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    READ(14,'(A)',IOSTAT=i) tmps
    CLOSE(14)
    !tmps = 'Reaction for '//TRIM(pcom%USER)

    i = safe_shell(TRIM(BIN_PATH)//'getparm '//TRIM(dir)//'f.getparm.in '//   &
         TRIM(dir)//'f.rateparm -bf '//TRIM(dir)//'getparm.backup -ef '//     & 
         TRIM(dir)//'getparm.exit -pi 5000 -si 50000 -f 500 -ov -id '''//     &
         TRIM(tmps)//''' '//TRIM(cina_decode(pcom%BODY))//' '//TRIM(opt)//   &
         ' &> '//TRIM(dir)//'redir & echo $! &> '//TRIM(dir)//'pid')
    IF (i /= 0) THEN
       WRITE(tmps,'(I0)') i
       CALL report_error('Error '//TRIM(tmps)//' while starting rate generator.', &
            'External program',pcom,1)
    END IF

    ! Create file that stores the cursor for the update process
    OPEN(UNIT=11,FILE=TRIM(dir)//'cur',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not create update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF

    ! The cursor starts at 0, meaning 0 characters have been read in so far
    WRITE(11,'(I0)',IOSTAT=i) 0
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write to update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF
    CLOSE(11)

    WRITE(*,'(A)') 'START=SUCCESS'
  END SUBROUTINE generate_parm

  !---------------------------------------------------------------------
  SUBROUTINE rateparm_update(pcom)
    !PURPOSE = Perform the RATE PARAMETERIZATION UPDATE action
    !STATUS = Complete and tested
    IMPLICIT NONE
    ! Use print_long_string if MAXCHAR is above 1024
    INTEGER(KIND=4),PARAMETER        :: MAXCHAR = 1000
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i,p,c,e
    CHARACTER(LEN=1)                 :: chr
    CHARACTER(LEN=200)               :: dir,tmps
    CHARACTER(LEN=MAXCHAR)           :: buffer,save_buffer
    LOGICAL(KIND=1)                  :: loop

    dir = rategen_temp_path(pcom)

    ! Get the PID of the rate parameterizer
    p = bg_pid(pcom,dir)
    IF (p <= 0) CALL report_error('The rate parameterizer was not started.', &
         'Improper usage',pcom,1)
    ! See if it is running
    i = KILL(p,18)  ! Send the process the SIGCONT signal (Continue process if stopped)
    SELECT CASE (i)
    CASE (0)
       ! process is running
       WRITE(*,'(A)') 'PARAMETERIZATION=RUNNING'
    CASE (-1)
       ! process is not running
       WRITE(*,'(A)') 'PARAMETERIZATION=COMPLETE'
    CASE DEFAULT
       WRITE(tmps,'(I0)') i
       CALL report_error('Unknown response '//TRIM(tmps)//' to signal','Developer Reminder',pcom,1)
    END SELECT

    ! Read in the number of characters already sent
    OPEN(UNIT=11,FILE=TRIM(dir)//'cur',IOSTAT=i,ACTION='READ')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    READ(11,'(I)',IOSTAT=i) c
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read update file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    CLOSE(11)

    ! Read in rate generation screen output
    OPEN(12,FILE=TRIM(dir)//'redir',IOSTAT=i,ACTION='READ',POSITION='APPEND')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open rate parameterization screen output.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF

    e = FTELL(12)                ! The total number of characters in the file (at this time)

    IF ((e == 0) .OR. (c == e)) THEN
       WRITE(*,'(A)') 'TEXT_SKIPPED=N'
       WRITE(*,'(A)') 'TEXT='
       RETURN
    END IF

    ! Find the last new line character before position e (because lines that aren't completed won't be sent)
    p = c
    i = FSEEK(12,p,0)                ! Position file to previous cursor
    IF (i /= 0) CALL report_error('Could not seek in rate parameterization screen output (1).', &
         'File input/output',pcom,1)
    loop = .TRUE.
    ! Store the position of the last newline in p
    DO WHILE (loop)
       READ(12,'(A)',IOSTAT=i) chr
       IF (i > 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read rate parameterization screen output.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF
       IF (i == 0) THEN
          i = FTELL(12)
          IF (i <= e) THEN
             p = i
          ELSE
             loop = .FALSE.
          END IF
       ELSE
          loop = .FALSE.
       END IF
       !print *,i,p,e,c
    END DO

    ! Treat p as the last position to use
    IF (p > 0) e = p

    IF (e - c <= MAXCHAR) THEN
       WRITE(*,'(A)') 'TEXT_SKIPPED=N'
    ELSE
       WRITE(*,'(A)') 'TEXT_SKIPPED=Y'
       ! Find position to start printing text by looking in the last MAXCHAR characters for the first newline
       p = e - MAXCHAR
       !IF (p < 1) p = 1
       i = FSEEK(12,p,0)                ! Position file to just before previous cursor
       IF (i /= 0) CALL report_error('Could not seek in rate parameterization screen output (2).', &
            'File input/output',pcom,1)
       ! Find the position after the first newline character
       READ(12,'(A1)',IOSTAT=i) chr
       c = FTELL(12) - 1
    END IF
    !print *,c
    i = FSEEK(12,c,0)                ! Position file to just after previous cursor
    IF (i /= 0) CALL report_error('Could not seek in rate parameterization screen output (3).', &
         'File input/output',pcom,1)

    save_buffer = ''
    DO WHILE (c < e)
       READ(12,'(A)',IOSTAT=i) buffer
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read rate parameterization screen output.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF

       c = FTELL(12)
       save_buffer = TRIM(save_buffer) // TRIM(buffer) // ACHAR(8)
    END DO
    CLOSE(12)

    ! Use print_long_string if MAXCHAR is above 1024
    WRITE(*,'(2A)') 'TEXT=',TRIM(save_buffer)

    ! Record new cursor
    OPEN(11,FILE=TRIM(dir)//'cur',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Can not open update file (2).  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    WRITE(11,'(I0)',IOSTAT=i) e

    CLOSE(11)
  END SUBROUTINE rateparm_update

  !---------------------------------------------------------------------
  SUBROUTINE rateparm_output(pcom)
    !PURPOSE = Perform the RATE PARAMETERIZATION OUTPUT action
    !STATUS = Complete and tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=200)               :: dir,tmps
    INTEGER(KIND=4)                  :: i,n,a_num
    REAL(KIND=8)                     :: tmpr,tmin,tmax,a(MAX_A)
    LOGICAL(KIND=1)                  :: low_prob,high_prob

    dir = rategen_temp_path(pcom)

    ! Get the PID of the rate parameterizer
    n = bg_pid(pcom,dir)
    IF (n <= 0) CALL report_error('The rate parameterizer was not started.', &
         'Improper usage',pcom,1)
    ! See if it is running
    i = KILL(n,18)  ! Send the process the SIGCONT signal (Continue process if stopped)
    SELECT CASE (i)
    CASE (0)
       ! process is running
       CALL report_error('Rate parameterizer has not completed yet.','Improper usage',pcom,1)
    CASE (-1)
       ! process is not running
       ! do nothing but continue
    CASE DEFAULT
       WRITE(tmps,'(I0)') i
       CALL report_error('Unknown response '//TRIM(tmps)//' to signal','Developer Reminder',pcom,1)
    END SELECT
    
    ! Check for f.rateparm file
    i = safe_shell('/bin/ls '//TRIM(dir)//'f.rateparm &> /dev/null')
    IF (i /= 0) THEN
       CALL report_error('Rate parameterizer output file was not found. ','External fileio',pcom,1)
    END IF
    
    ! Open rate parameterization output file
    OPEN(12,FILE=TRIM(dir)//'f.rateparm',IOSTAT=n,ACTION='READ')
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open rate parameterization output file.  IOSTAT=',n
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF

    ! Read in number of parameters
    READ(12,'(I)',IOSTAT=n) a_num
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read in number of parameters.  IOSTAT=',n
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    IF (a_num > MAX_A) THEN
       WRITE(tmps,'(A,I0,A,I0)') 'The maximum number of parameters allowed is ',MAX_A,' instead of ',a_num
       CALL report_error(TRIM(tmps),'Iproper usage',pcom,1)
    END IF

    WRITE(*,'(A,I0)') 'PARAMETERS=',a_num
    
    ! Print out parameters
    WRITE(*,'(A)',ADVANCE='NO') 'DATA='
    DO i = 1, a_num
       READ(12,'(G)',IOSTAT=n) a(i)
       tmps = ACHAR(9)
       IF (i < 2) tmps = ''
       WRITE(*,'(A,E15.7E3)',ADVANCE='NO') TRIM(tmps),a(i)
    END DO
    WRITE(*,'(A)') ''  ! Print new line

    ! Find and print out max percent difference
    n = 0
    DO WHILE (n == 0)
       READ(12,'(A)',IOSTAT=n) tmps
       IF (n > 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read rate parameterization output file.  IOSTAT=',n
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       ELSE IF (n == 0) THEN
          IF (tmps(1:21) == 'Maximum difference is') THEN
             WRITE(*,'(2A)') 'MAX_PERCENT_DIFF=',TRIM(tmps(28:37))
             n = -1              ! Force loop to terminate
          END IF
       ELSE
          CALL report_error('No maximum percent difference in output file.','Bad file format',pcom,1)
       END IF
    END DO

    ! Ignore ID string in file
    READ(12,'(A)',IOSTAT=n) tmps

    ! Print out chisquared
    READ(12,'(A29,G)',IOSTAT=n) tmps,tmpr
    IF (tmps(1:11) /= 'Chisquared:') CALL report_error('Chisquared not found in output file.', &
         'Bad file format',pcom,1)
    WRITE(*,'(A,1PG12.4E3)') 'CHISQUARED=',tmpr   !TRIM(ADJUSTL(tmps(30:)))

    CLOSE(12)

    ! Load tmin and tmax used in rate generator (CHANGE LATER TO RANGE USED FOR PARAMETERIZER)
    CALL read_trange(pcom,tmin,tmax,'f.trange')

    ! Compare tmin and tmax with REACLIB range
    low_prob = .TRUE.
    high_prob = .TRUE.
    IF (tmin <= 1D-2) low_prob = .FALSE.
    IF (tmax >= 10D0) high_prob = .FALSE.

    ! Check for low temp problem if necessary
    IF (low_prob) THEN
       low_prob = .FALSE.
       DO i = 1, a_num/7
          IF (a(2+7*(i-1)) > 0D0) low_prob = .TRUE.
       END DO
    END IF

    ! Check for high temp problem if necessary
    IF (high_prob) THEN
       high_prob = .FALSE.
       DO i = 1, a_num/7
          IF (a(6+7*(i-1)) > 0D0) high_prob = .TRUE.
       END DO
    END IF

    ! Now low_prob and high_prob are true if a problem exists

    ! Print warning message header
    IF (low_prob .OR. high_prob) WRITE(*,'(2A)',ADVANCE='NO') 'REASON=All reaction rates must be well ', &
         'behaved over temperatures from T9 = 0.01 to 10 to be entered into a rate library.  '

    ! Print warning message
    IF (low_prob) WRITE(*,'(2A,G9.3,A)',ADVANCE='NO') 'This rate may be unrealistic when extrapolated ', &
         'from the current minimum fit temperature of T9 = ',tmin,' to 0.01.  '
    IF (high_prob) WRITE(*,'(2A,G9.3,A)',ADVANCE='NO') 'This rate may be unrealistic when extrapolated ', &
         'from the current maximum fit temperature of T9 = ',tmax,' to 10.  '

    ! Print newline at end of warning message
    IF (low_prob .OR. high_prob) WRITE(*,'(3A)') 'Use the "Plot Fit Over Full Range" feature to see how this ', &
         'rate behaves over the full temperature range.  Potential solutions to this problem will be in ', &
         'a future version of our code.'
  END SUBROUTINE rateparm_output

  !---------------------------------------------------------------------
  SUBROUTINE create_tgrid(pcom)
    !PURPOSE = Create temperature grid for rate generator
    !STATUS = Complete and tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    REAL(KIND=8),PARAMETER           :: GRID(23) = (/     &
         1.0, 1.1, 1.2, 1.3, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8,  &
         3.0, 3.25, 3.5, 3.75, 4.0, 4.5, 5.0, 6.0, 7.0, 8.0, 9.0  /)
    INTEGER(KIND=1),PARAMETER        :: EMIN = -3    ! Smallest decade is 10^EMIN
    INTEGER(KIND=1),PARAMETER        :: EMAX = 0     ! Largest decade is 10^EMAX
    INTEGER(KIND=4)                  :: i,s,g,e
    CHARACTER(LEN=200)               :: dir
    CHARACTER(LEN=10)                :: tmps
    REAL(KIND=8)                     :: tmin, tmax, value

    dir = rategen_temp_path(pcom)

    ! Get tmin and tmax from trange file
    CALL read_trange(pcom,tmin,tmax)

    ! Check that TMIN and TMAX are inside gamov window
    IF (pcom%TMIN < tmin) THEN
       WRITE(*,'(A)') 'START=FAIL'
       WRITE(*,'(A)') 'REASON=Tmin is not in gamov window'
       STOP
    END IF
    IF (pcom%TMAX > tmax) THEN
       WRITE(*,'(A)') 'START=FAIL'
       WRITE(*,'(A)') 'REASON=Tmax is not in gamov window'
       STOP
    END IF

    ! Check that TMIN < TMAX
    IF (pcom%TMAX <= pcom%TMIN) THEN
       WRITE(*,'(A)') 'START=FAIL'
       WRITE(*,'(A)') 'REASON=Tmax must be greater than Tmin'
       STOP
    END IF

    ! Save new TMIN and TMAX into trange file, this will prevent the parameterizer from fitting 
    ! outside the range used by the rate generator
    OPEN(UNIT=17,FILE=TRIM(dir)//'f.trange',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open trange file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF

    ! Add TMIN and TMAX
    WRITE(17,*,IOSTAT=i) pcom%TMIN, pcom%TMAX
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write to trange file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF
    CLOSE(17)

    ! Create temperature grid file
    OPEN(UNIT=13,FILE=TRIM(dir)//'tgrid.txt',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not temperature grid file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF

    ! Add TMIN
    WRITE(13,'(1P,E14.4)',IOSTAT=i) pcom%TMIN
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not add TMIN to temperature grid file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF

    DO e = EMIN, EMAX
       ! e is the decade number.  If e is -2 then points are ??E-2
       DO g = 1, UBOUND(GRID,1)
          ! g is the index into the GRID
          value = GRID(g) * (10D0 ** e)
          ! Consider rounding number to 3 significant figures
          IF ((value > pcom%TMIN) .AND. (value < pcom%TMAX)) THEN
             WRITE(13,'(1P,E14.4)',IOSTAT=i) value
             IF (i /= 0) THEN
                WRITE(tmps,'(A,I0)') 'Could not add point to temperature grid file.  IOSTAT=',i
                CALL report_error(TRIM(tmps),'File input/output',pcom,0)
                STOP
             END IF
          END IF
       END DO
    END DO

    ! Add TMAX
    WRITE(13,'(1P,E14.4)',IOSTAT=i) pcom%TMAX
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not add TMAX to temperature grid file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF
    CLOSE(13)

  END SUBROUTINE create_tgrid

  !---------------------------------------------------------------------
  SUBROUTINE read_trange(pcom,tmin,tmax,file)
    !PURPOSE = Return tmin and tmax from trange file
    !STATUS = Complete and tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    REAL(KIND=8),INTENT(OUT)         :: tmin, tmax
    CHARACTER(LEN=*),INTENT(IN)      :: file
    CHARACTER(LEN=200)               :: dir,tmps
    INTEGER(KIND=4)                  :: i
    OPTIONAL                         :: file

    dir = rategen_temp_path(pcom)

    IF (PRESENT(file)) THEN
       tmps = file
    ELSE
       tmps = 'trange'
    END IF

    ! Open trange file and read in two read numbers
    OPEN(31,FILE=TRIM(dir)//TRIM(tmps),ACTION='READ',STATUS='UNKNOWN',IOSTAT=i)
    IF (i /= 0) THEN
       CALL report_error('Could not open temp_range output','File input/output',pcom,1)
    END IF

    READ(31,'(2G)',IOSTAT=i) tmin,tmax
    IF (i /= 0) THEN
       IF (pcom%USER == 'msmith' .OR. pcom%USER == 'jps' .OR. pcom%USER == 'a') THEN
          !CALL report_error('Could not read temp_range output','File input/output',pcom,0)
          tmin = 0.001D0
          tmax = 10.01D0
          CLOSE(31)
          CALL write_trange(pcom,tmin,tmax,tmps)
          RETURN
       ELSE
          CALL report_error('Could not read temp_range output','File input/output',pcom,1)
       END IF
    END IF
    CLOSE(31)

  END SUBROUTINE read_trange

  !---------------------------------------------------------------------
  SUBROUTINE write_trange(pcom,tmin,tmax,file)
    !PURPOSE = Save tmin and tmax to trange file
    !STATUS = Complete and tested
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    REAL(KIND=8),INTENT(IN)          :: tmin, tmax
    CHARACTER(LEN=*),INTENT(IN)      :: file
    CHARACTER(LEN=200)               :: dir,tmps
    INTEGER(KIND=4)                  :: i
    OPTIONAL                         :: file

    dir = rategen_temp_path(pcom)

    IF (PRESENT(file)) THEN
       tmps = file
    ELSE
       tmps = 'trange'
    END IF

    ! Open trange file and write in two real numbers
    OPEN(31,FILE=TRIM(dir)//TRIM(tmps),ACTION='WRITE',STATUS='UNKNOWN',IOSTAT=i)
    IF (i /= 0) THEN
       CALL report_error('Could not open temp_range output','File input/output',pcom,1)
    END IF

    WRITE(31,'(2G)',IOSTAT=i) tmin,tmax
    IF (i /= 0) THEN
       CALL report_error('Could not write temp_range output','File input/output',pcom,1)
    END IF
    CLOSE(31)

  END SUBROUTINE write_trange

  !---------------------------------------------------------------------
  SUBROUTINE inverse_parm(pcom)
    !PURPOSE = Perform the INVERSE PARAMETERS action
    !STATUS = Incomplete
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=1024)              :: dir,tmps
    INTEGER(KIND=4)                  :: s,i,j,n
    REAL(KIND=8)                     :: tmpr
    
    ! Kill any background processes
    CALL kill_bg_proc(pcom,dir)
    
    dir = rategen_temp_path(pcom)
    s = safe_shell('/bin/ls '//TRIM(dir)//'f.rateparm &> /dev/null')
    IF (s /= 0) THEN
       CALL report_error('Inverse parameters can not be calculated before '// &
            ' the rate has been parameterized. ','Improper usage',pcom,1)
    END IF

    ! Delete f.inverse file if it exists, ignore errors
    s = safe_shell('/bin/rm -f '//TRIM(dir)//'f.inverse ')

    ! Execute inverse program and redirect screen output to a file.
    s = safe_shell(TRIM(BIN_PATH)//'inverse '//TRIM(dir)//'f.inrd '//TRIM(dir)// &
         'f.rateparm '//TRIM(dir)//'f.inverse &> '//TRIM(dir)//'redir')
    IF (s /= 0) THEN
       WRITE(tmps,'(I0)') s
       CALL report_error('inverse exited with error '//TRIM(tmps),'External program',pcom,1)
    END IF

    OPEN(14,FILE=TRIM(dir)//'redir',IOSTAT=i,ACTION='READ')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Can not open inverse screen output.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF

    i = 0
    s = 0   ! Set s = 1 when an error occurred
    DO WHILE (i == 0)
       READ(14,'(A)',IOSTAT=i) tmps
       IF (i == 0) THEN
          IF (tmps(1:7) == 'ERROR: ') THEN
             CALL report_error(TRIM(tmps(8:)),'External program',pcom,4)
             s = 1
          END IF
       END IF
    END DO
    CLOSE(14)
    ! Print and log general error, stop cina
    IF (s == 1) CALL report_error('Inverse parameters can be obtained for '// &
         '(p,g) and (p,a) reactions currently.  Other reaction types will be added soon.', &
         'External program',pcom,1)

    ! Open f.inverse
    OPEN(14,FILE=TRIM(dir)//'f.inverse',IOSTAT=i,ACTION='READ')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Can not open inverse output file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF

    ! Open f.invparm that will contain the inverse parameters in an easy to read format
    OPEN(44,FILE=TRIM(dir)//'f.invparm',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Can not open inverse parameter file.  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF

    i = 0
    s = 0

    DO WHILE ((i == 0) .AND. (s < 2))
       READ(14,'(A)',IOSTAT=i) tmps
       IF (i == 0) THEN
          s = s + 1
          IF (s == 2) THEN
             READ(tmps,'(I)',IOSTAT=i) n
             IF (i /= 0) CALL report_error('Error reading number of parameters.', &
                  'Bad file format',pcom,1)
             WRITE(*,'(A,I0)') 'PARAMETERS=',n
          END IF
       END IF
    END DO

    ! Save number of parameters to f.invparm file
    WRITE(44,'(I0)',IOSTAT=i) n
    IF (i /= 0) CALL report_error('Error writing number of parameters.', &
         'File input/output',pcom,0)

    s = 0
    DO WHILE ((i == 0) .AND. (s == 0))
       READ(14,'(A)',IOSTAT=i) tmps
       IF (i == 0) THEN
          IF (tmps(1:37) == '* Parameters for the inverse reaction') THEN
             ! Read in and print parameters to tmps
             tmps = ''
             DO j = 1, n
                READ(14,'(G)',IOSTAT=i) tmpr
                IF (i /= 0) CALL report_error('Error reading inverse parameters.', &
                     'Bad file format',pcom,1)
                WRITE(tmps,'(2A,1P,E15.7E3)') TRIM(tmps),ACHAR(9),tmpr
                WRITE(44,'(1P,E15.7)',IOSTAT=i) tmpr
                IF (i /= 0) CALL report_error('Error writing parameters.', &
                     'File input/output',pcom,0)
             END DO
             ! Remove initial tab character
             tmps = tmps(2:)
             WRITE(*,'(2A)') 'DATA=',TRIM(tmps)
             ! Read in a blank line
             READ(tmps,'(A)',IOSTAT=i) tmps
             IF (i /= 0) CALL report_error('Error reading inverse output file.', &
                  'Bad file format',pcom,1)
             s = -1
          END IF
       END IF
    END DO
    CLOSE(44)

    ! Send the rest of the file as part of the formula
    WRITE(*,'(A)',ADVANCE='NO') 'FORMULA='
    DO WHILE (i == 0)
       READ(14,'(A)',IOSTAT=i) tmps
       IF (i == 0) THEN
          WRITE(*,'(2A)',ADVANCE='NO') TRIM(tmps),ACHAR(8)
       END IF
    END DO
    CLOSE(14)

    ! The statement below is used to print a new line character so that the 
    ! previous WRITE statements that didn't advance will now advance to the next line
    WRITE(*,'(A)') ' '
  END SUBROUTINE inverse_parm

  !---------------------------------------------------------------------
END MODULE rategen
