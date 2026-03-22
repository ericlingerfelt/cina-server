MODULE element_sim
  USE IFLPORT
  USE cina_core
  USE constants
  USE read_em_sim
  USE convert

  PUBLIC

  INTEGER(KIND=4),PUBLIC,PARAMETER :: MAX_NZ_ABUNDANCES = 6000
  INTEGER(KIND=4),PUBLIC,PARAMETER :: MAX_NZ_FLUXES = 62000
  INTEGER(KIND=4),PUBLIC,PARAMETER :: MAX_ZONES = 1000
  REAL(KIND=8),PUBLIC,PARAMETER    :: ABUNDANCE_FLOOR = 1.0E-25
  REAL(KIND=8),PUBLIC,PARAMETER    :: FLUX_FLOOR = 1.0E-20

CONTAINS

  !---------------------------------------------------------------------
  FUNCTION em_path_to_dir(path,pcom,zone)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)   :: pcom
    CHARACTER(LEN=*),INTENT(IN)    :: path
    INTEGER(KIND=4),INTENT(IN)     :: zone
    CHARACTER(LEN=200)             :: em_path_to_dir
    CHARACTER(LEN=40)              :: group
    INTEGER(KIND=4)                :: end_of_group,i
    OPTIONAL                       :: zone

    em_path_to_dir = ''

    ! Check for invalid characters
    i = VERIFY(path,ALPHANUM//' _-/.+')  ! Take away + after demo
    IF (i > 0) CALL report_error('Invalid character "'//path(i:i)//'" in path '//TRIM(path),'Improper usage',pcom,1)

    ! Check for '..' in path
    IF (INDEX(path,'..') > 0) CALL report_error('Path "'//TRIM(path)//'" may not contain ..','Improbable',pcom,1)

    ! Extract group dir
    end_of_group = INDEX(path(1:),'/')   ! Find position of second slash

    IF (path == '/') THEN
       group = ''
       em_path_to_dir = TRIM(CINA_PATH)
    ELSE
       ! Make sure there is a second slash
       IF (end_of_group < 1) CALL report_error('Invalid path "'//TRIM(path)//'"','Improper usage',pcom,1)
       group = path(1:end_of_group-1)

       ! Check for valid group
       SELECT CASE (group)
       CASE ('USER')
          group = 'USER/' // TRIM(pcom%USER) // '/em_sims'
       CASE ('SHARED','PUBLIC')
          group = TRIM(group) // '/em_sims'
       CASE DEFAULT
          CALL report_error('Invalid group "'//TRIM(group)//'" in path "'//TRIM(path)//'"','Improper usage',pcom,1)
       END SELECT
!       i=LEN(TRIM(path(end_of_group:)))
!       if(i < 2) CALL report_error('No valid save path specified:"'//TRIM(path)//'"','Improper usage',pcom,1)

       em_path_to_dir = TRIM(CINA_PATH)//TRIM(group)//TRIM(path(end_of_group:))
    END IF

    IF (PRESENT(zone)) THEN
       WRITE(em_path_to_dir,'(2A,I0)') TRIM(em_path_to_dir),'/',zone
    END IF
    ! CALL report_error('Path is now '//TRIM(em_path),'Improper usage',pcom,1)
  END FUNCTION em_path_to_dir

  FUNCTION get_sunet_path(sunet_path)
    IMPLICIT NONE
    CHARACTER(LEN=MAX_PATH_LEN)   :: get_sunet_path
    CHARACTER(LEN=200), INTENT(IN)   :: sunet_path
    CHARACTER(LEN=200)   :: sunet_name
    sunet_name = get_basename(sunet_path)
    get_sunet_path = TRIM(CINA_PATH) // 'PUBLIC/sunet/' // TRIM(sunet_name)
  END FUNCTION get_sunet_path

  FUNCTION get_init_abund_path(init_abund_path)
    IMPLICIT NONE
    CHARACTER(LEN=MAX_PATH_LEN)   :: get_init_abund_path
    CHARACTER(LEN=200), INTENT(IN)   :: init_abund_path
    CHARACTER(LEN=200)   :: init_abund_name
    init_abund_name = get_basename(init_abund_path)
    get_init_abund_path = TRIM(CINA_PATH) // 'PUBLIC/init_abund/' // TRIM(init_abund_name)
  END FUNCTION get_init_abund_path

  FUNCTION get_basename(path)
    CHARACTER(*), INTENT(IN) :: path
    CHARACTER(:), ALLOCATABLE :: get_basename
    index = SCAN(path,'/')
    get_basename = path(index+1:)
  END FUNCTION get_basename

  !---------------------------------------------------------------------
  FUNCTION thermo_path_to_dir(path,pcom,zone)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)   :: pcom
    CHARACTER(LEN=*),INTENT(IN)    :: path
    INTEGER(KIND=4),INTENT(IN)     :: zone
    CHARACTER(LEN=200)             :: thermo_path_to_dir
    CHARACTER(LEN=40)              :: group
    INTEGER(KIND=4)                :: end_of_group,i
    OPTIONAL                       :: zone

    thermo_path_to_dir = ''

    ! Check for invalid characters
    i = VERIFY(path,ALPHANUM//' _-/.+')  ! Take away + after demo
    IF (i > 0) CALL report_error('Invalid character "'//path(i:i)//'" in path '//TRIM(path),'Improper usage',pcom,1)

    ! Check for '..' in path
    IF (INDEX(path,'..') > 0) CALL report_error('Path "'//TRIM(path)//'" may not contain ..','Improbable',pcom,1)

    ! Extract group dir
    end_of_group = INDEX(path(1:),'/')   ! Find position of second slash

    IF (path == '/') THEN
       group = ''
       thermo_path_to_dir = TRIM(CINA_PATH)
    ELSE
       ! Make sure there is a second slash
       IF (end_of_group < 1) CALL report_error('Invalid path "'//TRIM(path)//'"','Improper usage',pcom,1)
       group = path(1:end_of_group-1)

       ! Check for valid group
       SELECT CASE (group)
       CASE ('USER')
          group = 'USER/' // TRIM(pcom%USER) // '/thermo'
       CASE ('SHARED','PUBLIC')
          group = TRIM(group) // '/thermo'
       CASE DEFAULT
          CALL report_error('Invalid group "'//TRIM(group)//'" in path "'//TRIM(path)//'"','Improper usage',pcom,1)
       END SELECT

       thermo_path_to_dir = TRIM(CINA_PATH)//TRIM(group)//TRIM(path(end_of_group:))
    END IF

    IF (PRESENT(zone)) THEN
       WRITE(thermo_path_to_dir,'(2A,I0)') TRIM(thermo_path_to_dir),'/',zone
    END IF
    ! CALL report_error('Path is now '//TRIM(em_path),'Improper usage',pcom,1)
  END FUNCTION thermo_path_to_dir

  !---------------------------------------------------------------------
  SUBROUTINE get_em_timemap(pcom)
    IMPLICIT NONE
    TYPE(cina_common)              :: pcom
    CHARACTER(LEN=200)             :: path,zones,file
    CHARACTER(LEN=10)              :: zone
    INTEGER(KIND=4)                :: zone_num,i,step_num,counter,kstep
    INTEGER(KIND=4)                :: zonemap(MAX_ZONES),zone_cnt,file_num
    REAL(KIND=8)                   :: t
    LOGICAL                        :: loop = .TRUE.

    path = cina_decode(pcom%BODY)
    path = em_path_to_dir(path,pcom)
    zones = pcom%REACTION
    CALL get_zonemap(pcom,TRIM(path)//'/zonemap',zonemap,zone_cnt)

    DO WHILE (loop)
       loop = next_in_list(zone,zones,',')
       ! Convert zone to integer
       READ(zone,'(I)',IOSTAT=i) zone_num
       IF (i /= 0) THEN
          WRITE(file,'(3A,I0)') 'Problem interpreting zone number "',TRIM(zone),'" ',i
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF

       ! Make filename for number of steps
       file_num = get_zonemap_inv(zonemap,zone_cnt,zone_num)
       IF (file_num < 0) THEN
          WRITE(file,'(3A,I0)') 'Simulation ',TRIM(cina_decode(pcom%BODY)), &
               ' does not have zone ',zone_num
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF

       file = pad_name(TRIM(path)//'/step_num',file_num,zone_cnt)

       ! Get number of data points
       CALL get_em_step_num(20,file,step_num,i)
       IF (i /= 0) THEN
       !   WRITE(file,'(A,I0,A,I0)') 'Error ',i,' getting number of steps for zone ',zone_num
          WRITE(file,'(A,I0,A,I0,A)') 'Error ',i,' line 211 getting number of steps for zone ',zone_num,file
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF

       ! Make filename for timemap
       file = pad_name(TRIM(path)//'/timemap',file_num,zone_cnt)

       ! Open timemap file
       CALL open_em_timemap(20,file,i)
       IF (i /= 0) THEN
          WRITE(path,'(A,I0,A)') 'Error ',i,' opening timemap file'
          CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
       END IF

       ! step_num is number of last step.  Add 1 because steps start at 0
       WRITE(*,'(I0,A,I0,A)',ADVANCE='NO') zone_num,',',step_num+1,'='

       counter = -1
       DO WHILE (i == 0)
          CALL read_em_timemap(20,kstep,t,i)
          IF (i == 0) THEN
             counter = counter + 1

             ! Make sure counter and kstep are equal
             IF (counter /= kstep) THEN
                WRITE(*,'(A)') ''  ! Advance line
                WRITE(path,'(A,I0,A,I0,A)') 'Invalid timemap file. counter=',counter,' kstep=',kstep,' :'
                CALL report_error(TRIM(path)//TRIM(file),'Bad file format',pcom,1)
             END IF

             ! Print out time value
             IF (counter == 0) THEN
                WRITE(*,'(1P,E20.12)',ADVANCE='NO') t
             ELSE
                WRITE(*,'(A,1P,E20.12)',ADVANCE='NO') ',',t
             END IF
          ELSE IF (i > 0) THEN
             WRITE(*,'(A)') ''  ! Advance line
             WRITE(path,'(A,I0,A)') 'Error ',i,' reading timemap file:'
             CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
          END IF
       END DO

       CLOSE(20)
       WRITE(*,'(A)') ''  ! Advance line

       IF (counter /= step_num) THEN
          WRITE(path,'(A,I0,A,I0,A)') 'Invalid timemap file. counter=',counter,' step_num=',step_num,' :'
          CALL report_error(TRIM(path)//TRIM(file),'Bad file format',pcom,1)
       END IF
    END DO
  END SUBROUTINE get_em_timemap

  !---------------------------------------------------------------------
  SUBROUTINE get_em_isomap(pcom)
    IMPLICIT NONE
    TYPE(cina_common)             :: pcom
    CHARACTER(LEN=200)             :: path,err_str
    TYPE(em_isomap)                :: isomap
    INTEGER(KIND=4)                :: i

    path = cina_decode(pcom%BODY)
    path = em_path_to_dir(path,pcom)

    CALL load_em_isomap(20,TRIM(path)//'/isomap',isomap,i,err_str)
    IF (i /= 0) CALL report_error(TRIM(err_str),'Improper usage',pcom,1)

    WRITE(*,'(I0,A)',ADVANCE='NO') isomap%last_index + 1,'='

    DO i = 0,isomap%last_index
       IF (i == 0) THEN
          WRITE(*,'(I0,A,I0,2A)',ADVANCE='NO') isomap%z(0),',',isomap%a(0),',',TRIM(isomap%label(0))
       ELSE
          WRITE(*,'(A,I0,A,I0,2A)',ADVANCE='NO') ACHAR(9),isomap%z(i),',',isomap%a(i),',',TRIM(isomap%label(i))
       END IF
    END DO
    WRITE(*,'(A)') ''  ! Advance line

  END SUBROUTINE get_em_isomap

  !---------------------------------------------------------------------
  SUBROUTINE get_em_reacmap(pcom)
    IMPLICIT NONE
    TYPE(cina_common)             :: pcom
    CHARACTER(LEN=200)             :: path,err_str,name
    TYPE(em_reacmap)               :: reacmap
    TYPE(em_isomap)                :: isomap
    INTEGER(KIND=4)                :: i,in,out

    name = cina_decode(pcom%BODY)
    path = em_path_to_dir(name,pcom)

    CALL load_em_isomap(20,TRIM(path)//'/isomap',isomap,i,err_str)
    IF (i /= 0) CALL report_error(TRIM(err_str),'Improper usage',pcom,1)
    CALL load_em_reacmap(20,TRIM(path)//'/reacmap',reacmap,i,err_str)
    IF (i == 152) THEN
       WRITE(*,'(A)') '0='
       RETURN

       CALL report_error('Flux data is not available for "'//TRIM(name)//'"', &
            'Warning',pcom,0)
    ELSE IF (i /= 0) THEN
       CALL report_error(TRIM(err_str),'Improper usage',pcom,1)
    END IF

    WRITE(*,'(I0,A)',ADVANCE='NO') reacmap%last_index + 1,'='

    DO i = 0,reacmap%last_index
       CALL get_channel_heavyweights(reacmap, i, isomap, in, out)

       IF (in < 0 .OR. out < 0) THEN
          WRITE(*,'(A)') ''  ! Advance line
          CALL report_error('Invalid reaction mapping', &
               'Bad file format',pcom,1)
       END IF

       IF (i == 0) THEN
          WRITE(*,'(I0,A,I0,2A)',ADVANCE='NO') in,',',out,',',TRIM(ADJUSTL(reacmap%biblio(i)))
       ELSE
          WRITE(*,'(A,I0,A,I0,2A)',ADVANCE='NO') ACHAR(9),in,',',out,',',TRIM(ADJUSTL(reacmap%biblio(i)))
       END IF
    END DO
    WRITE(*,'(A)') ''  ! Advance line

  END SUBROUTINE get_em_reacmap

  !---------------------------------------------------------------------
  SUBROUTINE get_em_thermo(pcom)
    IMPLICIT NONE
    TYPE(cina_common)             :: pcom
    CHARACTER(LEN=200)             :: path,zones,file
    CHARACTER(LEN=10)              :: zone
    INTEGER(KIND=4)                :: zone_num,i,step_num,counter,kstep
    INTEGER(KIND=4)                :: zonemap(MAX_ZONES),zone_cnt,file_num
    REAL(KIND=8)                   :: t9,rhot
    LOGICAL                        :: loop = .TRUE.

    path = cina_decode(pcom%BODY)
    path = em_path_to_dir(path,pcom)
    zones = pcom%REACTION
    CALL get_zonemap(pcom,TRIM(path)//'/zonemap',zonemap,zone_cnt)

    DO WHILE (loop)
       loop = next_in_list(zone,zones,',')
       ! Convert zone to integer
       READ(zone,'(I)',IOSTAT=i) zone_num
       IF (i /= 0) THEN
          WRITE(file,'(3A,I0)') 'Problem interpreting zone number "',TRIM(zone),'" ',i
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF

       ! Make filename for number of steps
       file_num = get_zonemap_inv(zonemap,zone_cnt,zone_num)
       IF (file_num < 0) THEN
          WRITE(file,'(3A,I0)') 'Simulation ',TRIM(cina_decode(pcom%BODY)), &
               ' does not have zone ',zone_num
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF

       file = pad_name(TRIM(path)//'/step_num',file_num,zone_cnt)

       ! Get number of data points
       CALL get_em_step_num(20,file,step_num,i)
       IF (i /= 0) THEN
       !   WRITE(file,'(A,I0,A,I0)') 'Error ',i,'line 375  getting number of steps for zone ',zone_num
          WRITE(file,'(A,I0,A,I0,A)') 'Error ',i,'line 375  getting number of steps for zone ',zone_num,file
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF

       ! Make filename for thermo profile
       file = pad_name(TRIM(path)//'/thermo',file_num,zone_cnt)

       ! Open thermo file
       CALL open_em_thermo(20,file,i)
       IF (i /= 0) THEN
          WRITE(path,'(A,I0,A)') 'Error ',i,' opening thermo file'
          CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
       END IF

       ! step_num is number of last step.  Add 1 because steps start at 0
       WRITE(*,'(I0,A,I0,A)',ADVANCE='NO') zone_num,',',step_num+1,'='

       counter = -1
       DO WHILE (i == 0)
          CALL read_em_thermo(20,kstep,t9,rhot,i)
          IF (i == 0) THEN
             counter = counter + 1

             ! Make sure counter and kstep are equal
             IF (counter /= kstep) THEN
                WRITE(*,'(A)') ''  ! Advance line
                WRITE(path,'(A,I0,A,I0,A)') 'Invalid thermo file. counter=',counter,' kstep=',kstep,' :'
                CALL report_error(TRIM(path)//TRIM(file),'Bad file format',pcom,1)
             END IF

             ! Print out temperature and density
             IF (counter == 0) THEN
                WRITE(*,'(1P,E20.12E3,A,E20.12E3)',ADVANCE='NO') t9,',',rhot
             ELSE
                WRITE(*,'(A,1P,E20.12E3,A,E20.12E3)',ADVANCE='NO') ACHAR(9),t9,',',rhot
             END IF
          ELSE IF (i > 0) THEN
             WRITE(*,'(A)') ''  ! Advance line
             WRITE(path,'(A,I0,A)') 'Error ',i,' reading thermo file:'
             CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
          END IF
       END DO

       CLOSE(20)
       WRITE(*,'(A)') ''  ! Advance line

       IF (counter /= step_num) THEN
          WRITE(path,'(A,I0,A,I0,A)') 'Invalid thermo file. counter=',counter,' step_num=',step_num,' :'
          CALL report_error(TRIM(path)//TRIM(file),'Bad file format',pcom,1)
       END IF
    END DO
  END SUBROUTINE get_em_thermo

  !---------------------------------------------------------------------
  SUBROUTINE get_em_abund(pcom)
    USE io
    USE large_data
    IMPLICIT NONE
    TYPE(cina_common)             :: pcom
    TYPE(large_data_file)          :: ldf
    CHARACTER(LEN=200)             :: path,file
    CHARACTER(LEN=10)              :: tmps,zone
    CHARACTER(LEN=4096)            :: isotopes
    INTEGER(KIND=4)                :: sel_iso(MAX_NZ_ABUNDANCES),sel_iso_num,num_nz_sel
    INTEGER(KIND=4)                :: zone_num,i,z,a,step_num,counter,kstep,num_nz,s
    INTEGER(KIND=4)                :: iso_ind(MAX_NZ_ABUNDANCES),sel_iso_ind(MAX_NZ_ABUNDANCES)
    INTEGER(KIND=4)                :: alli(5)
    INTEGER(KIND=4)                :: iostatus
    INTEGER(KIND=4)                :: zonemap(MAX_ZONES),zone_cnt,file_num
    REAL(KIND=8)                   :: abund(MAX_NZ_ABUNDANCES),sel_abund(MAX_NZ_ABUNDANCES)
    LOGICAL                        :: loop = .TRUE.,found_an_isotope
    TYPE(em_isomap)                :: isomap

    path = cina_decode(pcom%BODY)
    path = em_path_to_dir(path,pcom)
    zone = pcom%REACTION
    CALL get_zonemap(pcom,TRIM(path)//'/zonemap',zonemap,zone_cnt)

! Convert zone to integer
    READ(zone,'(I)',IOSTAT=i) zone_num
    IF (i /= 0) THEN
       WRITE(file,'(3A,I0)') 'Problem interpreting zone number "',TRIM(zone),'" ',i
       CALL report_error(TRIM(file),'Improper usage',pcom,1)
    END IF

! Make filename for number of steps
    file_num = get_zonemap_inv(zonemap,zone_cnt,zone_num)
!print *,zone_num,zone_cnt,file_num

    IF (file_num < 0) THEN
       WRITE(file,'(3A,I0)') 'Simulation ',TRIM(cina_decode(pcom%BODY)), &
            ' does not have zone ',zone_num
       CALL report_error(TRIM(file),'Improper usage',pcom,1)
    END IF

    file = pad_name(TRIM(path)//'/step_num',file_num,zone_cnt)

! Get number of timesteps
    CALL get_em_step_num(20,file,step_num,i)
    IF (i /= 0) THEN
!   WRITE(file,'(A,I0,A,I0)') 'Error ',i,' line 474 getting number of steps for zone ',zone_num
       WRITE(file,'(A,I0,A,I0,A)') 'Error ',i,' line 474 getting number of steps for zone ',zone_num,file
       CALL report_error(TRIM(file),'Improper usage',pcom,1)
    END IF

! Load isomap
    CALL load_em_isomap(20,TRIM(path)//'/isomap',isomap,i,isotopes)
    IF (i /= 0) CALL report_error(TRIM(isotopes),'Improper usage',pcom,1)
    CLOSE(20)

! Get list of isotopes
    isotopes = cina_decode(pcom%NOTES)

! Record if sel_iso_num should be > 0
    IF (isotopes == '') THEN
       found_an_isotope = .TRUE.
    ELSE
       found_an_isotope = .FALSE.
    END IF

    sel_iso_num = 0
    IF (isotopes /= '') THEN
       ! Parse isotope string into indicies
       DO WHILE (loop)
          loop = next_in_list(tmps,isotopes,ACHAR(9))
          READ(tmps,'(2I)',IOSTAT=i) z,a
          IF (i /= 0) THEN
             WRITE(file,'(A,I0,3A)') 'Error ',i,' interpreting isotope "',TRIM(tmps),'"'
             CALL report_error(file,'Improper usage',pcom,1)
          END IF

          ! Get the isotope index in isomap for this isotope
          s = get_index_to_isotope(isomap,z,a,alli=alli)
          i = 1
          DO WHILE (alli(i) >= 0)
             IF (alli(i) >= 0) THEN
                sel_iso_num = sel_iso_num + 1
                IF (sel_iso_num <= UBOUND(sel_iso,1)) THEN
                   sel_iso(sel_iso_num) = alli(i)
                ELSE
                   WRITE(path,'(A,I0)') 'Too many isotopes were selected.  The maximum is currently ',MAX_NZ_ABUNDANCES
                   CALL report_error(path,'Developer Reminder',pcom,1)
                END IF
             END IF
             i = i + 1
          END DO
       END DO
    END IF

    IF (sel_iso_num > 0) found_an_isotope = .TRUE.

! Make filename for abundances
    file = pad_name(TRIM(path)//'/abund',file_num,zone_cnt)

    ldf%path_prefix = TRIM(TEMP_PATH) // 'large_data/'
    CALL ldf_open(ldf,33,pcom%ID)
    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str),'File input/output -- line 529',pcom,1)

    IF (found_an_isotope) THEN
! Open abundances file
       CALL open_em_abund(20,21,file,i)
       IF (i /= 0) THEN
          WRITE(path,'(A,I0,A)') 'Error ',i,' opening abund file'
          CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
       END IF

!    OPEN (UNIT=99, FILE="/tmp/cina_err", STATUS='REPLACE', ACTION='WRITE', IOSTAT= iostatus)
!    if( iostatus /= 0) WRITE(*,'(A)') 'Could NOT OPEN ERROR FILE'
       counter = -1
       s = 0
       DO WHILE (i == 0)
!
          CALL read_em_abund(20,21,kstep,num_nz,iso_ind,abund,i)
!          write(99,'(A,I0,A,I0,A,I0,A,I0,A)') '550: counter,error,kstep,num_nz:',counter,': ',i,': ',kstep,': ',num_nz,': '
!          write(99,'(A,I0)') 'ISO_IND = ',iso_ind(1:num_nz)
!          write(99,'(A,E15.10)') 'ABUND = ', abund(1:num_nz)

          IF (i == 0) THEN
             counter = counter + 1
!             write(99,'(A,I0,A,I0,A,I0)') 'counter,kstep,i : ',counter,': ',kstep,': ',i
             ! Make sure counter and kstep are equal
             IF (counter /= kstep) THEN
                WRITE(33,'(A)',IOSTAT=s) ''  ! Advance line
                IF (s /= 0) CALL report_error('Error writing to ldf file -- line 547','File input/output',pcom,1)
                WRITE(path,'(A,I0,A,I0,A)') 'Invalid abundances file. counter=',counter,' kstep=',kstep,' :'
                CALL report_error(TRIM(path)//TRIM(file),'Bad file format -- line 547',pcom,1)
             END IF

!             write(99,'(A,A,A,I0,A,I0)') '565: FORMAT,step_num,sel_iso : ',pcom%FORMAT,': ',step_num,': ',sel_iso_num
! This next line makes no sense.  Need to find a "functional" file and see what is says I changed the
! first logical operator from /= to == to see what happens.
! This line is supposed to determine whether all the abundances are wanted or only the LAST abundance.  If FORMAT
! is Y then only the last time step is wanted.  Problem is there nothing handling that case.  BTW step_num is set
! when the time steps are received and is usuall something like 2042.  kstep is set in read_em_abund and is usually
! 0.  I do not know when they might be the same.
             IF (pcom%FORMAT /= 'Y' .OR. kstep == step_num) THEN
                IF (sel_iso_num == 0) THEN
!
! Return all non-zero abundances
!
                   num_nz_sel = 0
                   sel_abund = 0D0
                   DO a = 1, num_nz
                      IF (ABS(abund(a)) >= ABUNDANCE_FLOOR) THEN
                         num_nz_sel = num_nz_sel + 1
                         sel_iso_ind(num_nz_sel) = iso_ind(a)
                         sel_abund(num_nz_sel) = abund(a)
                      END IF
                   END DO
!          write(99,'(A,I0,A,I0,A,I0,A,I0,A)') '588:counter,kstep,num_nz_sel',counter,': ',kstep,': ',num_nz_sel,': '
                ELSE
!
! Find the abundances of the selected isotopes
!
                   num_nz_sel = 0
                   sel_abund = 0D0
                   DO a = 1, num_nz
                      ! Loop through nonzero abundances
                      DO z = 1, sel_iso_num
                         IF (iso_ind(a) == sel_iso(z)) THEN
                            IF (ABS(abund(a)) >= ABUNDANCE_FLOOR) THEN
                               num_nz_sel = num_nz_sel + 1
                               sel_iso_ind(num_nz_sel) = iso_ind(a)
                               sel_abund(num_nz_sel) = abund(a)
                            END IF
                         END IF
                      END DO
                   END DO
                 END IF
!
! the end of the selection of the abundances if-then-else construct
!
                WRITE(33,'(I0,A,I0,A)',ADVANCE='NO',IOSTAT=s) kstep,',',num_nz_sel,'='
!                WRITE(99,'(A,I0,A,I0,A)',ADVANCE='NO',IOSTAT=s) '608:kstep and num_nz_sel: ',kstep,',',num_nz_sel,'='
                IF (s /= 0) CALL report_error('Error writing to ldf file -- line 587','File input/output',pcom,1)

                DO a = 1, num_nz_sel
                   IF (a == 1) THEN
                      WRITE(33,'(I0,A,1P,E20.12E3)',ADVANCE='NO',IOSTAT=s) sel_iso_ind(1),',',sel_abund(1)
!                      WRITE(99,'(A,I0,A,1P,E12.4E3)',ADVANCE='NO',IOSTAT=s) 'a == 1 ',sel_iso_ind(1),',',sel_abund(1)
                   ELSE
                      WRITE(33,'(A,I0,A,1P,E20.12E3)',ADVANCE='NO',IOSTAT=s) ACHAR(9),sel_iso_ind(a),',',sel_abund(a)
!                      WRITE(99,'(A,A,I0,A,1P,E12.4E3)',ADVANCE='NO',IOSTAT=s) 'a/=1 ',ACHAR(9),sel_iso_ind(a),',',sel_abund(a)
                   END IF
                   IF (s /= 0) CALL report_error('Error writing to ldf file -- line 596','File input/output',pcom,1)
                END DO
                WRITE(33,'(A)',IOSTAT=s) ''  ! Advance line
             END IF
          ELSE IF (i == -334) THEN
! associated with if i==0
             CALL report_error('MAX_NZ_ISOTOPES is too small to load isotopes for '//TRIM(file),'Developer Reminder',pcom,1)
             WRITE(33,'(A)',IOSTAT=s) ''  ! Advance line
             ELSE IF (i > 0) THEN
! associated with if i==0
                WRITE(33,'(A)',IOSTAT=s) ''  ! Advance line
                IF (s /= 0) CALL report_error('Error writing to ldf file -- line 605','File input/output',pcom,1)
                WRITE(path,'(A,I0,A)') 'Error ',i,' reading abundances file:'
                CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
              END IF
       END DO
!
!end of do while i==0, this ends the selection of the isotopes.
!and their abundances.
!

       CLOSE(20)
       CLOSE(21)

       IF (counter /= step_num) THEN
          WRITE(path,'(A,I0,A,I0,A)') 'Invalid abundances file. counter=',counter,' step_num=',step_num,' :'
          CALL report_error(TRIM(path)//TRIM(file),'Bad file format',pcom,1)
       END IF
    ELSE
! None of the selected isotopes were found in simulation
! Associated with the IF found_an_isotope

!      WRITE(99,'(A,A)') 'FORMAT line 654 - ', pcom%FORMAT
       IF (pcom%FORMAT == 'Y') step_num = 0
       DO i = 0, step_num
          WRITE(33,'(I0,A)',IOSTAT=s) i,',0='
          IF (s /= 0) CALL report_error('Error writing to ldf file -- line 627','File input/output',pcom,1)
       END DO
    END IF

    CALL ldf_close(ldf)
    CALL ldf_compress(ldf)
    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str),'File input/output',pcom,1)

    CALL ldf_send(ldf,del=.FALSE.)
    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str),'File input/output',pcom,1)
!    CLOSE(99)
  END SUBROUTINE get_em_abund

  !---------------------------------------------------------------------
  SUBROUTINE get_em_flux(pcom)
    USE io
    USE large_data
    IMPLICIT NONE
    TYPE(cina_common)             :: pcom
    TYPE(large_data_file)          :: ldf
    CHARACTER(LEN=200)             :: path,file,name
    CHARACTER(LEN=10)              :: tmps,zone
    CHARACTER(LEN=4096)            :: reactions
    INTEGER(KIND=4)                :: sel_reac(MAX_NZ_FLUXES),sel_reac_num,num_nz_sel
    INTEGER(KIND=4)                :: zone_num,i,a,z,step_num,counter,kstep,num_nz,s
    INTEGER(KIND=4)                :: reac_ind(MAX_NZ_FLUXES),sel_reac_ind(MAX_NZ_FLUXES)
    INTEGER(KIND=4)                :: zonemap(MAX_ZONES),zone_cnt,file_num
    REAL(KIND=8)                   :: flux(MAX_NZ_FLUXES),sel_flux(MAX_NZ_FLUXES)
    LOGICAL                        :: loop = .TRUE.,found_reaction,sum
    TYPE(em_reacmap)               :: reacmap
    TYPE(em_isomap)                :: isomap
    TYPE(em_matchmap)              :: matchmap

    name = cina_decode(pcom%BODY)
    path = em_path_to_dir(name,pcom)
    zone = pcom%REACTION
    CALL get_zonemap(pcom,TRIM(path)//'/zonemap',zonemap,zone_cnt)

    IF (pcom%FORMAT == 'Y') THEN
       sum = .TRUE.
    ELSE
       sum = .FALSE.
    END IF

    ! Convert zone to integer
    READ(zone,'(I)',IOSTAT=i) zone_num
    IF (i /= 0) THEN
       WRITE(file,'(3A,I0)') 'Problem interpreting zone number "',TRIM(zone),'" ',i
       CALL report_error(TRIM(file),'Improper usage',pcom,1)
    END IF

    ! Make filename for number of steps
    file_num = get_zonemap_inv(zonemap,zone_cnt,zone_num)
    IF (file_num < 0) THEN
       WRITE(file,'(3A,I0)') 'Simulation ',TRIM(cina_decode(pcom%BODY)), &
            ' does not have zone ',zone_num
       CALL report_error(TRIM(file),'Improper usage',pcom,1)
    END IF

    file = pad_name(TRIM(path)//'/step_num',file_num,zone_cnt)

    ! Get number of timesteps
    CALL get_em_step_num(20,file,step_num,i)
    IF (i /= 0) THEN
    !   WRITE(file,'(A,I0,A,I0)') 'Error ',i,' line 690 getting number of steps for zone ',zone_num
       WRITE(file,'(A,I0,A,I0,A)') 'Error ',i,' line 690 getting number of steps for zone ',zone_num,file
       CALL report_error(TRIM(file),'Improper usage',pcom,1)
    END IF

    ! Load reacmap
    CALL load_em_reacmap(20,TRIM(path)//'/reacmap',reacmap,i,reactions)
    IF (i == 152) THEN
       CALL report_error('Flux data is not available for "'//TRIM(name)//'"', &
            'Warning',pcom,1)
    ELSE IF (i /= 0) THEN
       CALL report_error(TRIM(reactions),'Improper usage',pcom,1)
    END IF

    ! Load match info if summing
    IF (sum) THEN
       ! Load isomap
       CALL load_em_isomap(20,TRIM(path)//'/isomap',isomap,i,reactions)
       IF (i /= 0) CALL report_error(TRIM(reactions),'Improper usage',pcom,1)
       CLOSE(20)

       !PRINT *,'Loading match info'
       CALL match_heavyweights(reacmap, isomap, matchmap)
!!$       print '(A)',''
!!$       CALL print_matchmap(reacmap, isomap, matchmap)
    END IF

    ! Get list of reactions
    reactions = cina_decode(pcom%NOTES)

    ! Record if sel_reac_num should be > 0
    IF (reactions == '') THEN
       found_reaction = .TRUE.
    ELSE
       found_reaction = .FALSE.
    END IF

    sel_reac_num = 0
    IF (reactions /= '') THEN
       ! Parse reaction string into indicies
       DO WHILE (loop)
          loop = next_in_list(tmps,reactions,ACHAR(9))
          READ(tmps,'(2I)',IOSTAT=i) a
          IF (i /= 0) THEN
             WRITE(file,'(A,I0,3A)') 'Error ',i,' interpreting integer "',TRIM(tmps),'"'
             CALL report_error(file,'Improper usage',pcom,1)
          END IF

          IF (a >= 0 .AND. a <= reacmap%last_index) THEN
             sel_reac_num = sel_reac_num + 1
             IF (sel_reac_num <= UBOUND(sel_reac,1)) THEN
                sel_reac(sel_reac_num) = a
             ELSE
                WRITE(path,'(A,I0)') 'Too many reactions were selected.  The maximum is currently ',MAX_NZ_FLUXES
                CALL report_error(path,'Developer Reminder',pcom,1)
             END IF
          END IF
       END DO
    END IF

    IF (sel_reac_num > 0) found_reaction = .TRUE.

    ! Make filename for fluxes
    file = pad_name(TRIM(path)//'/flux',file_num,zone_cnt)

    ldf%path_prefix = TRIM(TEMP_PATH) // 'large_data/'
    CALL ldf_open(ldf,33,pcom%ID)
    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str),'File input/output',pcom,1)

    IF (found_reaction) THEN
       ! Open fluxes file
       CALL open_em_flux(20,21,file,i)
       IF (i /= 0) THEN
          WRITE(path,'(A,I0,A)') 'Error ',i,' opening flux file'
          CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
       END IF

       counter = -1
       DO WHILE (i == 0)

          IF (sum) THEN
             CALL read_em_flux_summed(20,21,kstep,num_nz,reac_ind,flux,matchmap,i)
          ELSE
             CALL read_em_flux(20,21,kstep,num_nz,reac_ind,flux,i)
          END IF

          IF (i == 0) THEN
             counter = counter + 1

             ! Make sure counter and kstep are equal
             IF (counter /= kstep) THEN
                WRITE(33,'(A)',IOSTAT=s) ''  ! Advance line
                IF (s /= 0) CALL report_error('Error writing to ldf file -- line 780','File input/output',pcom,1)
                WRITE(path,'(A,I0,A,I0,A)') 'Invalid fluxes file. counter=',counter,' kstep=',kstep,' :'
                CALL report_error(TRIM(path)//TRIM(file),'Bad file format',pcom,1)
             END IF

             IF (sel_reac_num == 0) THEN
                ! Return all non-zero fluxes
!!$                num_nz_sel = num_nz
!!$                sel_reac_ind = reac_ind
!!$                sel_flux = flux

                num_nz_sel = 0
                sel_flux = 0D0
                DO a = 1, num_nz
                   IF (ABS(flux(a)) >= FLUX_FLOOR) THEN
                      num_nz_sel = num_nz_sel + 1
                      sel_reac_ind(num_nz_sel) = reac_ind(a)
                      sel_flux(num_nz_sel) = flux(a)
                   END IF
                END DO
             ELSE
                ! Find the fluxes of the selected reactions
                num_nz_sel = 0
                sel_flux = 0D0
                DO a = 1, num_nz
                   ! Loop through nonzero fluxes
                   DO z = 1, sel_reac_num
                      IF (reac_ind(a) == sel_reac(z)) THEN
                         IF (ABS(flux(a)) >= FLUX_FLOOR) THEN
                            num_nz_sel = num_nz_sel + 1
                            sel_reac_ind(num_nz_sel) = reac_ind(a)
                            sel_flux(num_nz_sel) = flux(a)
                         END IF
                      END IF
                   END DO
                END DO
             END IF

             WRITE(33,'(I0,A,I0,A)',ADVANCE='NO',IOSTAT=s) kstep,',',num_nz_sel,'='
             IF (s /= 0) CALL report_error('Error writing to ldf file -- line 819','File input/output',pcom,1)

             DO a = 1, num_nz_sel
                IF (a == 1) THEN
                   WRITE(33,'(I0,A,1P,E20.12E3)',ADVANCE='NO',IOSTAT=s) sel_reac_ind(1),',',sel_flux(1)
                ELSE
                   WRITE(33,'(A,I0,A,1P,E20.12E3)',ADVANCE='NO',IOSTAT=s) ACHAR(9),sel_reac_ind(a),',',sel_flux(a)
                END IF
                IF (s /= 0) CALL report_error('Error writing to ldf file -- line 827','File input/output',pcom,1)
             END DO

          ELSE IF (i == -334) THEN
             CALL report_error('MAX_NZ_REACTIONS is too small to load reactions for '//TRIM(file),'Developer Reminder',pcom,1)
          ELSE IF (i > 0) THEN
             WRITE(33,'(A)',IOSTAT=s) ''  ! Advance line
             IF (s /= 0) CALL report_error('Error writing to ldf file -- line 833','File input/output',pcom,1)
             WRITE(path,'(A,I0,A)') 'Error ',i,' reading fluxes file:'
             CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
          END IF

          WRITE(33,'(A)',IOSTAT=s) ''  ! Advance line
          IF (s /= 0) CALL report_error('Error writing to ldf file -- line 838','File input/output',pcom,1)

       END DO

       CLOSE(20)
       CLOSE(21)

       IF (counter /= step_num) THEN
          WRITE(path,'(A,I0,A,I0,A)') 'Invalid fluxes file. counter=',counter,' step_num=',step_num,' :'
          CALL report_error(TRIM(path)//TRIM(file),'Bad file format',pcom,1)
       END IF
    ELSE
       ! None of the selected reactions were found in simulation
!!$       WRITE(33,'(A)') 'CAUTION=None of the selected reactions were found in ' &
!!$            // TRIM(cina_decode(pcom%BODY))

       DO i = 0, step_num
          WRITE(33,'(I0,A)',IOSTAT=s) i,',0='
          IF (s /= 0) CALL report_error('Error writing to ldf file -- line 855','File input/output',pcom,1)
       END DO
    END IF

    !IF (sum) STOP
    CALL ldf_close(ldf)
    CALL ldf_compress(ldf)
    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str),'File input/output',pcom,1)
    CALL ldf_send(ldf,del=.FALSE.)
    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str),'File input/output',pcom,1)

  END SUBROUTINE get_em_flux

  !---------------------------------------------------------------------
  SUBROUTINE make_es_movie(pcom)
!    USE CINA
    USE constants
    IMPLICIT NONE
    TYPE(cina_common),INTENT(INOUT):: pcom
    TYPE(cina_common)             :: pcom2
    CHARACTER(LEN=LEN(pcom%NOTES)) :: java_args
    CHARACTER(LEN=MAX_PATH_LEN)    :: sim_path
    CHARACTER(LEN=20)              :: id,postfix,width,height,skip_int
    CHARACTER(LEN=100)             :: alias,email
    CHARACTER(LEN=4096)            :: err_msg
    INTEGER(KIND=4)                :: i,l,c
    CHARACTER(LEN=10)              :: sec_check

    java_args = cina_decode(pcom%NOTES)
    sim_path = cina_decode(pcom%BODY)
    WRITE(skip_int,'(I0)') pcom%INT
    width = pcom%WIDTH
    height = pcom%HEIGHT

    sec_check = ''
    ! Security check on java_args
    ! Should be exactly 4 double quotes in java_args
    i = 0
    c = INDEX(java_args,'"')
    l = c
    DO WHILE (c > 0)
       i = i + 1
       c = INDEX(java_args(l+1:),'"')
       l = l + c
    END DO
    IF (i /= 4) sec_check = 'java_args'
    IF (i /= 4) CALL report_error(ACHAR(48 + i),'Improbable',pcom,1)

    ! Security check on sim_path
    i = INDEX(sim_path,"'")
    IF (i > 0) sec_check = 'sim_path'

    IF (sec_check /= '') CALL log_error2(pcom%SESSIONP,'A software bug was found on the server and the coordinator has been notified with a bug report.','Improbable',0,1,sec_check,&
         '$Id: element_sim.f90,v 1.30 2009/08/11 16:19:42 elingerf Exp $',1)

    width = ADJUSTL(width)
    height = ADJUSTL(height)
    postfix = CINA_NAME(12:)

    ! Get new ID so that movie maker can run in the background
    pcom2 = pcom
    pcom2%SESSIONP = 0
    pcom2%ACTION="GET ID"
    pcom2%ID = get_CGI_ID(pcom2)
    !id = pcom%ID

    CALL get_account(pcom%SESSIONP,alias,email,err_msg)
    IF (err_msg /= '') CALL log_error2(pcom%SESSIONP,'Error obtaining email address','Improbable',0,1,err_msg, &
         '$Id: element_sim.f90,v 1.30 2009/08/11 16:19:42 elingerf Exp $',1)

    ! This
    ! 1) makes the MOVIE_PICS directory
    ! 2) starts the makemovie script in the background redirecting all output to ID.script.out
    ! 3) echos the pid of the makemovie script to MOVIE_PICS/ID.pid

    i = SYSTEM("/bin/mkdir -p '" // TRIM(TEMP_PATH) // "MOVIE_PICS' && (" // &
         TRIM(BIN_PATH) // "makemovie " // TRIM(CINA_NAME) // &
         " '" // TRIM(alias) // "' '" // TRIM(email) // "' '" // TRIM(sim_path) // &
         "' '" // TRIM(TEMP_PATH) // "MOVIE_PICS/" // TRIM(pcom2%ID) // &
         "' '../html/cina_movies/" // TRIM(pcom2%ID) // TRIM(postfix) // "' '" // &
         TRIM(pcom%HEADER) // "' '" // TRIM(pcom2%ID) // "' '" // &
         TRIM(pcom%USER) // "' '" // TRIM(pcom%PW) // "' '" // TRIM(skip_int) // "' '"// &
         TRIM(width) // "' '" // TRIM(height) // "' " // TRIM(java_args) // " &> '" // &
         TRIM(TEMP_PATH) // "MOVIE_PICS/" // TRIM(pcom2%ID) // TRIM(postfix) // &
         ".script.out' & echo $! &> '" // TRIM(TEMP_PATH) // "MOVIE_PICS/" // &
         TRIM(pcom2%ID) // TRIM(postfix) // ".pid')")

    IF (i == 0) THEN
       WRITE(*,'(A)') 'REPORT=Your movie is now being rendered, and you ' // &
            'will receive an email when it is complete.' // ACHAR(8) // ACHAR(8) // &
            'This process may take twenty minutes or more if no timesteps ' // &
            'were skipped.  Skipping timesteps and adjusting "Timestep min" ' // &
            'and "Timestep max" to include only needed timesteps will ' // &
            'reduce the movie rendering time.' // ACHAR(8) // ACHAR(8) // &
            'Please send comments and suggestions to coordinator@nucastrodata.org' // &
            ACHAR(8) // ACHAR(8) // &
            'Thank you for your patience and continued support of nucastrodata.org.'
    ELSE
       CALL CGI_logout(pcom2)
       WRITE(err_msg,'(A,I0,A)') 'A problem occurred while making your movie.  ' // &
            'Developers have been notified of this problem and will ' // &
            'email you once it is resolved.' // ACHAR(8) // ACHAR(8) // &
            'The error code for this problem, for use by the Developers, is ',i,'.' // &
            ACHAR(8) // ACHAR(8) // &
            'Thank you for your patience and continued support of nucastrodata.org.'
       CALL report_error(TRIM(err_msg),'External program',pcom,1)
    END IF
  END SUBROUTINE make_es_movie

  FUNCTION trimadjl(str)
    character(len=:), allocatable :: trimadjl
    character(len=*), intent(in) :: str

    trimadjl = trim(adjustl(str))

  END FUNCTION


  !---------------------------------------------------------------------
  FUNCTION get_em_tmp_dir(pcom)
    IMPLICIT NONE
    TYPE(cina_common)             :: pcom
    CHARACTER(LEN=200)            :: get_em_tmp_dir
    CHARACTER(LEN=10)             :: sim_workflow_index_string

    WRITE(sim_workflow_index_string, "(I10)") pcom%SIM_WORKFLOW_RUN_INDEX
    get_em_tmp_dir = TRIM(TEMP_PATH)//trimadjl(sim_workflow_index_string)//'/em_syn/'

  END FUNCTION get_em_tmp_dir

  SUBROUTINE em_syn_setup_copy_data(pcom,dir)
    USE constants
    USE fileio
    USE rate_man
    USE rate_man_core
    USE io
    USE convert
    TYPE(cina_common)             :: pcom
    CHARACTER(LEN=100)             :: library
    CHARACTER(LEN=200)             :: dir,tmps,sunet
    INTEGER(KIND=4)                :: s
    CHARACTER(LEN=700000)          :: report

    library = cina_decode(pcom%BODY)

    ! Remove any files that exist and ignore any errors
    s = safe_shell('/bin/rm -fr ' // TRIM(dir) // ' &> /dev/null')
    IF (s /= 0) THEN
        WRITE(tmps,'(I0)') s
        CALL report_error('Could not remove old ID dir.  Error ' // &
                TRIM(tmps),'External fileio',pcom,1)
    END IF

    ! Make the directory for this ID
    s = safe_shell('/bin/mkdir -p ''' // TRIM(dir) // 'data''' // &
             ' &> /dev/null')
    IF (s /= 0) THEN
       WRITE(tmps,'(I0)') s
       CALL report_error('Could not make ID dir.  Error ' // TRIM(tmps), &
            'External fileio',pcom,1)
    END IF

    OPEN(22,FILE=TRIM(dir)// 'notes',ACTION='WRITE',IOSTAT=s)
    IF (s /= 0) THEN
       WRITE(tmps,'(I0)') s
       CALL report_error('Could not open notes file.  Error ' // TRIM(tmps), &
            'External fileio',pcom,1)
    END IF

    WRITE(22,'(A)') 'Library= ' // TRIM(library)
    CLOSE(22)

    i = safe_shell('/bin/cp -R ' // TRIM(CINA_PATH) // 'PUBLIC/preprocessed_rate_libs/' // TRIM(library) // '/data/* ' // TRIM(dir))

    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while running em_syn_setup_copy_data'
       CALL report_error(TRIM(tmps),'External program',pcom,1)
    END IF

    OPEN(RLIB_NETSU_UNIT, FILE=TRIM(dir) // 'data/report', IOSTAT=i, ACTION='READ')
    READ(RLIB_NETSU_UNIT,'(A700000)') report
    CLOSE(RLIB_NETSU_UNIT)

    WRITE(*,'(A)') 'SETUP=SUCCESS'
    WRITE(*,'(A)') 'SUMMARY=Network prepared'
    WRITE(*,'(A)',ADVANCE='NO') 'REPORT='
    CALL print_long_string(TRIM(report))

  END SUBROUTINE em_syn_setup_copy_data

  !---------------------------------------------------------------------
  SUBROUTINE em_syn_setup(pcom)
    USE constants
    USE fileio
    USE rate_man
    USE rate_man_core
    USE io
    USE convert
    IMPLICIT NONE
    TYPE(cina_common)              :: pcom
    CHARACTER(LEN=100)             :: library
    INTEGER(KIND=4)                :: sim_workflow_run_index
    CHARACTER(LEN=10)              :: sim_workflow_index_string
    CHARACTER(LEN=200)             :: dir,tmps,sunet
    INTEGER(KIND=4)                :: s,i,j,minz,mina,maxz,maxa,NUNIT
    INTEGER(KIND=2)                :: isolist(0:MAX_Z,MAX_ISO)
    LOGICAL(KIND=4)                :: meta_stable
    LOGICAL                        :: ex
    CHARACTER(LEN=700000)          :: report

    dir = get_em_tmp_dir(pcom)

    ! Kill any background processes
    CALL kill_bg_proc(pcom,dir)

    sim_workflow_run_index = pcom%SIM_WORKFLOW_RUN_INDEX
    library = cina_decode(pcom%BODY)

    sunet = get_file()

    IF((INDEX(sunet, 'R_Process') /= 0) .AND. ((library .EQ. 'ReaclibV1.1') .OR. (library .EQ. 'ReaclibV2.0') .OR. &
        (library .EQ. 'REACLIB07') .OR. (library .EQ. 'REACLIB 2000 Beta 0.1'))) THEN
        CALL em_syn_setup_copy_data(pcom,dir)
    ELSE
        tmps = cina_decode(pcom%NOTES)
        IF (pcom%TYPE == 'METASTABLE') THEN
           meta_stable = .TRUE.
        ELSE IF (pcom%TYPE == 'STABLE') THEN
           meta_stable = .FALSE.
        ELSE
           CALL report_error('Invalid TYPE: "' // TRIM(pcom%TYPE) // '"', &
                'Improper usage',pcom,1)
        END IF

        ! Remove any files that exist and ignore any errors
        s = safe_shell('/bin/rm -fr ' // TRIM(dir) // ' &> /dev/null')
        IF (s /= 0) THEN
           WRITE(tmps,'(I0)') s
           CALL report_error('Could not remove old ID dir.  Error ' // &
                TRIM(tmps),'External fileio',pcom,1)
        END IF

        ! Make the directory for this ID
        s = safe_shell('/bin/mkdir -p ''' // TRIM(dir) // 'data''' // &
             ' &> /dev/null')
        IF (s /= 0) THEN
           WRITE(tmps,'(I0)') s
           CALL report_error('Could not make ID dir.  Error ' // TRIM(tmps), &
                'External fileio',pcom,1)
        END IF

        OPEN(22,FILE=TRIM(dir)// 'notes',ACTION='WRITE',IOSTAT=s)
        IF (s /= 0) THEN
           WRITE(tmps,'(I0)') s
           CALL report_error('Could not open notes file.  Error ' // TRIM(tmps), &
                'External fileio',pcom,1)
        END IF

        WRITE(22,'(A)') 'Library= ' // TRIM(library)
        CLOSE(22)

        report = 'Library= ' // TRIM(library) // &
        ACHAR(8)//'Preparing for element synthesis.' // ACHAR(8) &
        //'Part 1)'// ACHAR(9)

        ! Now the work begins
        CALL read_rlib_list('PUBLIC',pcom)
        CALL read_rlib_list('SHARED',pcom)
        CALL read_rlib_list('USER',pcom)

        !IF (sunet == '') THEN
           ! Use MAX and MIN ISOTOPE
        !report = TRIM(report)  // ACHAR(9) // &
        !'No sunet file selected, using max/min.' // ACHAR(8)

           ! Read in min isotopes
        !   i = INDEX(pcom%REACTION,',')
        !   IF (i < 2) CALL report_error('MIN_ISOTOPE is invalid "' // &
        !        TRIM(pcom%REACTION) // '"','Improper usage',pcom,1)
        !   READ(pcom%REACTION(:i-1),'(I)',IOSTAT=s) minz
        !   IF (s /= 0) CALL report_error('Error reading z in "' // &
        !        pcom%REACTION(:i-1) // '"','Improper usage',pcom,1)
        !   READ(pcom%REACTION(i+1:),'(I)',IOSTAT=s) mina
        !   IF (s /= 0) CALL report_error('Error reading a in "' // &
        !        pcom%REACTION(i+1:) // '"','Improper usage',pcom,1)

           ! Read in max isotopes
        !   j = INDEX(tmps,',')
        !   IF (j < 2) CALL report_error('MAX_ISOTOPE is invalid "' // TRIM(tmps) // &
        !        '"','Improper usage',pcom,1)
        !   READ(tmps(:j-1),'(I)',IOSTAT=s) maxz
        !   IF (s /= 0) CALL report_error('Error reading z in "' // tmps(:j-1) // &
        !        '"','Improper usage',pcom,1)
        !   READ(tmps(j+1:),'(I)',IOSTAT=s) maxa
        !   IF (s /= 0) CALL report_error('Error reading a in "' // tmps(j+1:) // &
        !        '"','Improper usage',pcom,1)

        !   IF (minz < 0 .OR. minz > maxz) CALL report_error( &
        !        'Invalid value of minz: "' // pcom%REACTION(:i-1) // '"',&
        !        'Improper usage',pcom,1)
        !   IF (maxz < minz .OR. maxz > MAX_Z) CALL report_error( &
        !        'Invalid value of maxz: "' // tmps(:j-1) // '"','Improper usage', &
        !        pcom,1)
        !   IF (mina < 1 .OR. mina > maxa) CALL report_error( &
        !        'Invalid value of mina: "' // TRIM(pcom%REACTION(i+1:)) // '"', &
        !        'Improper usage',pcom,1)
        !   IF (maxa < mina) CALL report_error('Invalid value of maxa: ' // &
        !        TRIM(tmps(j+1:)) // '"','Improper usage',pcom,1)

        !   CALL filter_rlib(library,'temp_' // pcom%ID,pcom,minz,mina,maxz,maxa, &
        !        meta_stable,report,overwrite=.TRUE.)

        !ELSE IF (lowercase(sunet) == 'all') THEN
        !   minz = 0
        !   mina = 1
        !   maxz = MAX_Z
        !   maxa = 600
        !report = TRIM(report)  // ACHAR(9) // &
        !     'Using All.' // ACHAR(8)

        !   CALL filter_rlib(library,'temp_' // pcom%ID,pcom,minz,mina,maxz,maxa, &
        !        meta_stable,report,overwrite=.TRUE.)
           !print *,'done'
        !ELSE
           ! Load sunet file into isolist
           tmps = get_sunet_path(sunet)

           CALL sunet_to_isolist(pcom,tmps,isolist)

           WRITE(sim_workflow_index_string, "(I10)") pcom%SIM_WORKFLOW_RUN_INDEX
           CALL filter_rlib_sunet(library,'temp_' // trimadjl(sim_workflow_index_string),pcom,isolist, &
                tmps,meta_stable,report,overwrite=.TRUE.)
        !END IF

        report = TRIM(report) // ACHAR(8) // 'Part 2)' // ACHAR(9) // &
             'Checking new library for errors.' // ACHAR(8)
        pcom%CHK_TEMP = .FALSE.
        pcom%CHK_OVFL = .FALSE.
        pcom%CHK_INV = .FALSE.
        pcom%FORMAT = 'NO LIB NAME'

        ! Check library for errors
        CALL copy_rlib('temp_' // trimadjl(sim_workflow_index_string),'',pcom,report)

        report = TRIM(report) // ACHAR(8) // 'Part 3)' // ACHAR(9) // &
             'Exporting new library to full netsu format.' // ACHAR(8)

        ! CALL fmt_output_for_lib
        CALL export_rlib('temp_' // trimadjl(sim_workflow_index_string),TRIM(dir) // 'data/', &
             'FULL NETSU',pcom,report,meta_stable,.TRUE.)

        ! Remove library directory
        tmps = get_rlib_path('temp_' // trimadjl(sim_workflow_index_string),pcom%USER)
        IF (tmps == '') CALL report_error('Internal error (empty tmps)', &
             'Improbable',pcom,1)

        CALL rm_rlib('temp_' // trimadjl(sim_workflow_index_string),pcom,move_to_trash=.FALSE.)

        s = safe_shell('/bin/rm -fr ''' // TRIM(tmps) // '''')
        IF (s /= 0) THEN
           WRITE(tmps,'(I0)') s
           CALL report_error('Could not remove new library.  Error ' // &
                TRIM(tmps),'External fileio',pcom,1)
        END IF

        report = TRIM(report) // ACHAR(8) // 'Part 4)' // ACHAR(9) // &
             'Running net_setup.' // ACHAR(8)

        ! Run netsetup
        ! Put current directory in tmps so that BIN_PATH is valid
        i = GETCWD(tmps)
        i = safe_shell('cd ''' // TRIM(dir) // 'data'' && ulimit -s 102400 && ' &
                        // TRIM(tmps) // '/' // TRIM(BIN_PATH) // 'net_setup &> redir')
        IF (i /= 0) THEN
           WRITE(tmps,'(A,I0,A)') 'Error ',i,' while running net_setup'
           CALL report_error(TRIM(tmps),'External program',pcom,1)
        END IF

        ! Open redir file and append contents to report
        OPEN(RLIB_NETSU_UNIT, FILE=TRIM(dir) // 'data/redir', IOSTAT=i, &
             ACTION='READ')
        IF (i /= 0) THEN
           WRITE(tmps,'(A,I0,A)') 'Error ',i,' while opening net_setup output'
           CALL report_error(TRIM(tmps),'File input/output',pcom,1)
        END IF

        DO WHILE (i == 0)
           READ(RLIB_NETSU_UNIT, '(A)', IOSTAT=i) tmps
           IF (i > 0) THEN
              WRITE(tmps,'(A,I0,A)') 'Error ',i,' while reading net_setup output'
              CALL report_error(TRIM(tmps),'File input/output',pcom,1)
           ELSE IF (i == 0) THEN
              report = TRIM(report) // TRIM(tmps) // ACHAR(8)
           END IF
        END DO
        CLOSE(RLIB_NETSU_UNIT)

        i = LEN_TRIM(report)
        IF (report(i-31:i-1) /= 'Network prepared for simulation') THEN
           WRITE(*,'(A)') 'SETUP=FAILURE'
           WRITE(*,'(A)') 'SUMMARY=Network was NOT prepared properly'
           CALL report_error('Problems were encountered that prevented network preparation.  ' // &
                'Please click continue and read the report for more information', &
                'Warning',pcom,2)
           report = TRIM(report) // ACHAR(8) // 'Problems were ' // &
                'encountered while preparing network.' // ACHAR(8) // &
                'Please read report above for more information.' // ACHAR(8)
    !    ELSE IF (INDEX(report,'CAUTION!') > 0 .OR. INDEX(report,'Missing') > 0) THEN
    !       WRITE(*,'(A)') 'SETUP=SUCCESS'
    !       WRITE(*,'(A)') 'SUMMARY=Network prepared but it may have problems'
    !       CALL report_error('Warnings were encountered during this step.  ' // &
    !            'Please click continue and read the report for more information', &
    !            'Warning',pcom,2)
    !       report = TRIM(report) // ACHAR(8) // 'Potential problems were ' // &
    !            'encountered while preparing network.' // ACHAR(8) // &
    !            'Please read report above for more information.' // ACHAR(8)
        ELSE
           WRITE(*,'(A)') 'SETUP=SUCCESS'
           WRITE(*,'(A)') 'SUMMARY=Network prepared'
        END IF

        !report = replaceall(report,ACHAR(8),ACHAR(10))
        WRITE(*,'(A)',ADVANCE='NO') 'REPORT='
        CALL print_long_string(TRIM(report))
    END IF
  END SUBROUTINE em_syn_setup

  !---------------------------------------------------------------------
  SUBROUTINE get_zone_array(string,array,zone_cnt)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)    :: string
    INTEGER(KIND=4),INTENT(OUT)    :: array(:),zone_cnt
    CHARACTER(LEN=LEN(string))     :: zone_str
    CHARACTER(LEN=10)              :: zone
    INTEGER(KIND=4)                :: s
    LOGICAL(KIND=4)                :: loop

    loop = .TRUE.
    zone_cnt = 0
    array = -1
    zone_str = string
    DO WHILE (loop)
       loop = next_in_list(zone,zone_str,',')
       ! Convert zone to integer
       READ(zone,'(I)',IOSTAT=s) array(zone_cnt+1)
       IF (s /= 0) THEN
          array(zone_cnt+1) = -1
          RETURN
       END IF

       zone_cnt = zone_cnt + 1
    END DO
  END SUBROUTINE get_zone_array

  !---------------------------------------------------------------------
  SUBROUTINE get_zonemap(pcom,file,zonemap,zone_cnt,zone_str)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)  :: pcom
    CHARACTER(LEN=*),INTENT(IN)    :: file
    INTEGER(KIND=4),INTENT(OUT)    :: zone_cnt,zonemap(:)
    CHARACTER(LEN=*),INTENT(OUT)   :: zone_str
    INTEGER(KIND=4)                :: i
    CHARACTER(LEN=8192)            :: tmps
    OPTIONAL                       :: zone_str

    IF (PRESENT(zone_str)) zone_str = ''

    OPEN(22,FILE=file,ACTION='READ',IOSTAT=i)
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' opening zonemap file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF

    READ(22,'(A)',IOSTAT=i) tmps
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' reading zonemap file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF
    CLOSE(22)

    IF (PRESENT(zone_str)) zone_str = tmps

    i = num_occurrances(tmps,',')
    CALL get_zone_array(tmps,zonemap,zone_cnt)
    IF (zone_cnt /= 1 + i) THEN
       CALL report_error('Problem interpreting zone numbers', &
            'Improbable',pcom,1)
    END IF
  END SUBROUTINE get_zonemap

  !---------------------------------------------------------------------
  FUNCTION get_zonemap_inv(zonemap,zone_cnt,zone)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)     :: zonemap(:),zone_cnt,zone
    INTEGER(KIND=4)                :: get_zonemap_inv
    INTEGER(KIND=4)                :: i

    get_zonemap_inv = -1
    DO i = 1, zone_cnt
       IF (zonemap(i) == zone) THEN
          get_zonemap_inv = i
          RETURN
       END IF
    END DO
  END FUNCTION get_zonemap_inv

  !---------------------------------------------------------------------
  FUNCTION get_first_line(file,iostat)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)    :: file
    INTEGER(KIND=4),INTENT(OUT)    :: iostat
    CHARACTER(LEN=8192)             :: get_first_line
    INTEGER(KIND=4)                :: i
    OPTIONAL                       :: iostat

    get_first_line = ''

    OPEN(22,FILE=file,ACTION='READ',IOSTAT=i)
    IF (i /= 0) THEN
       IF (PRESENT(iostat)) iostat = i
       RETURN
    END IF

    READ(22,'(A)',IOSTAT=i) get_first_line
    IF (i /= 0) THEN
       IF (PRESENT(iostat)) iostat = i
       RETURN
    END IF

    CLOSE(22,IOSTAT=i)
    IF (PRESENT(iostat)) iostat = i
  END FUNCTION get_first_line

  !---------------------------------------------------------------------
  SUBROUTINE get_lastzone(pcom,file,zone)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)  :: pcom
    CHARACTER(LEN=*),INTENT(IN)    :: file
    INTEGER(KIND=4),INTENT(OUT)    :: zone
    INTEGER(KIND=4)                :: i
    CHARACTER(LEN=200)             :: tmps

    OPEN(22,FILE=file,ACTION='READ',IOSTAT=i)
    IF (i /= 0) THEN
       zone = 1
       RETURN
       !WRITE(tmps,'(A,I0,A)') 'Error ',s,' opening lastzone file'
       !CALL report_error(tmps,'External program',pcom,1)
    END IF

    READ(22,'(I)',IOSTAT=i) zone
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' reading lastzone file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF
    CLOSE(22)
  END SUBROUTINE get_lastzone

  !---------------------------------------------------------------------
  SUBROUTINE em_syn_start(pcom)
    IMPLICIT NONE
    TYPE(cina_common)              :: pcom
    CHARACTER(LEN=200)             :: init_abund_path,thermo_pro_path,dir,tmps
    CHARACTER(LEN=8192)            :: all_zones,zones
    CHARACTER(LEN=10)              :: zone
    INTEGER(KIND=4)                :: i,s,screening,weak
    INTEGER(KIND=4)                :: zone_array(MAX_ZONES),zone_cnt
    LOGICAL(KIND=4)                :: loop

    init_abund_path = get_data1()
    thermo_pro_path = get_data2()
    !init_abund_path = em_path_to_dir(init_abund_path,pcom)
    zones = cina_decode(pcom%BODY)
    dir = get_em_tmp_dir(pcom)

    ! Kill any background processes
    CALL kill_bg_proc(pcom,dir)

    s = num_occurrances(zones,',')
    CALL get_zone_array(zones,zone_array,zone_cnt)
    IF (zone_cnt /= 1 + s) THEN
       CALL report_error('Problem interpreting zone numbers', &
            'Improbable',pcom,1)
    END IF

    ! Make zonemap file
    OPEN(22,FILE=TRIM(dir)//'zonemap',ACTION='WRITE',IOSTAT=s)
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' opening zonemap file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF

    WRITE(22,'(A)',IOSTAT=s) TRIM(zones)
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' writing zonemap file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF
    CLOSE(22)

    ! Make notes file
    OPEN(22,FILE=TRIM(dir)//'notes',ACTION='WRITE', &
      STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=s)
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' opening notes file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF

    WRITE(22,'(A)',IOSTAT=s) cina_decode(TRIM(pcom%NOTES))
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' writing notes file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF
    CLOSE(22)

    init_abund_path = get_init_abund_path(init_abund_path)

	! The commented block below had to be replaced with code
	! to dynamically generate a new initial abundance profile
	! since a custom isotope range can be used. The init_ab file
	! must contain exactly the same amount of isotopes in the same
	! order as the sunet and netwinv files.
	CALL create_init_ab(dir, init_abund_path, pcom)

    ! Copy initial abundances to temp directory
    !s = safe_shell('/bin/cp -f '''//TRIM(init_abund_path)//''' '''//TRIM(dir)//'init_ab'' ')!//' &> /dev/null')
    !IF (s /= 0) THEN
    !   WRITE(tmps,'(A,I0,A)') 'Error ',s,' copying '//TRIM(get_data1())//' initial abundance file'
    !   CALL report_error(tmps,'External program',pcom,1)
    !END IF

    ! Copy thermodynamic zone weights to temp directory
    tmps = thermo_path_to_dir(thermo_pro_path,pcom)

    ! tmps = thermo_pro_path
    s = safe_shell('/bin/cp -f '''//TRIM(tmps)//'/weights'//''' '''//TRIM(dir)//''' ')!//' &> /dev/null')
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' copying zone weights file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF

    ! Make control file
    OPEN(22,FILE=TRIM(dir)//'control',ACTION='WRITE',IOSTAT=s)
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' opening control file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF

    IF (pcom%SCREENING) THEN
       screening = 1
    ELSE
       screening = 0
    END IF
    IF (pcom%WEAK) THEN
       weak = 1
    ELSE
       weak = 0
    END IF

    IF(INDEX(init_abund_path, 'R_Process_Abund') /= 0) THEN
        WRITE(22,'(1P/////,9(I0/),6(E8.2/)/A////A/)',IOSTAT=s) 1            &
        ,zone_cnt,pcom%INT,5,0,3,weak,screening,0,0.1,1.0E-07,1.0E-6,1.0E-4,&
        1.0E-25,2.0,'ev','data'
    ELSE
        WRITE(22,'(1P/////,9(I0/),6(E8.2/)/A////A/)',IOSTAT=s) 1            &
        ,zone_cnt,pcom%INT,5,0,3,weak,screening,1,0.1,1.0E-12,1.0E-6,1.0E-4,&
        1.0E-25,2.0,'ev','data'
    END IF

!changed the 1 default starting zone number to the "real" zone number
!so that the simulation files will be saved correctly.  This will
!probably break something else. Yep sure did, someplace in xnet.  This is
!pretty silly.
!    WRITE(22,'(1P/////,9(I0/),6(E8.2/)/A////A/)',IOSTAT=s) 1            &
!    ,zone_cnt,pcom%INT,5,0,3,weak,screening,0,0.1,1.0E-12,1.0E-6,1.0E-4,&
!    1.0E-25,2.0,'ev','data'
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' writing control file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF

    ! Copy thermodynamic profiles to temp directory
    DO i = 1, zone_cnt
       WRITE(zone,'(I0)') zone_array(i)

       WRITE(22,'(A/2A)',IOSTAT=s) 'init_ab','th',TRIM(zone)
       IF (s /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',s,' writing control file'
          CALL report_error(tmps,'External program',pcom,1)
       END IF

       tmps = thermo_path_to_dir(thermo_pro_path,pcom,zone=zone_array(i))
	   ! tmps = ''

	   ! WRITE(tmps,'(2A,I0)') TRIM(thermo_pro_path),'/',zone_array(i)

       s = safe_shell('/bin/cp -f '''//TRIM(tmps)//''' '''//TRIM(dir)// &
            'th'//TRIM(zone)//''' ')!//' &> /dev/null')
       IF (s /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',s,' copying '//TRIM(get_data2())//' thermodynamic profile file'
          CALL report_error(tmps,'External program',pcom,1)
       END IF
    END DO

    CLOSE(22)
!       this is the new file that sets the start and stop times.
!       this is 'better' than modifying the control file format.
    OPEN(22,FILE=TRIM(dir)//'time_control',ACTION='WRITE',IOSTAT=s)
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',s,' opening time_control file'
       CALL report_error(tmps,'External program',pcom,1)
    END IF
    WRITE(22,'(e13.6/e13.6)',IOSTAT=s) pcom%TMIN,pcom%TMAX
    CLOSE(22)

    s = safe_shell('cd '//TRIM(dir)//' && (../../../bin/xnet &> redir & echo $! &> pid)')
    IF (s /= 0) THEN
       WRITE(tmps,'(I0)') s
       CALL report_error('Error '//TRIM(tmps)// &
            ' while starting element synthesis simulator.','External program',pcom,1)
    END IF

    ! Create file that stores the cursor for the update process
    OPEN(UNIT=11,FILE=TRIM(dir)//'cur',IOSTAT=s,ACTION='WRITE')
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not create update file.  IOSTAT=',s
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF

    ! The cursor starts at 0, meaning 0 characters have been read in so far
    WRITE(11,'(I0)',IOSTAT=s) 0
    IF (s /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write to update file.  IOSTAT=',s
       CALL report_error(TRIM(tmps),'File input/output',pcom,0)
       STOP
    END IF
    CLOSE(11)

    WRITE(*,'(A)') 'REPORT=Element Synthesis Simulation Started'
  END SUBROUTINE em_syn_start

  !---------------------------------------------------------------------
  SUBROUTINE create_init_ab(dir, init_abund_path, pcom)
  	IMPLICIT NONE

  	TYPE(cina_common), INTENT(IN) :: pcom
  	CHARACTER(LEN=200), INTENT(IN) :: init_abund_path, dir

  	INTEGER(KIND=4)                :: s, i, tmp_ny, orig_ny, counter
  	CHARACTER(LEN=200)             :: tmps, abund_desc
  	INTEGER(KIND=4), PARAMETER     :: TMP_SUNET_UNIT = 101
  	INTEGER(KIND=4), PARAMETER     :: TMP_NETWINV_UNIT = 102
  	INTEGER(KIND=4), PARAMETER     :: TMP_INIT_AB_UNIT = 103
  	INTEGER(KIND=4), PARAMETER     :: ORIG_INIT_AB_UNIT = 104
  	CHARACTER(LEN=5), &
  	ALLOCATABLE, DIMENSION(:)      :: tmp_iso_array, orig_iso_array
	REAL(KIND=8), 			  &
	ALLOCATABLE, DIMENSION(:)      :: orig_ab_array
	REAL(KIND=8), DIMENSION(4)     :: ab_array
	CHARACTER(LEN=5), DIMENSION(4) :: iso_array
	INTEGER(KIND=4)                :: ab_counter, orig_iso_index

	i = 0;

	! Open the tmp netwinv file
	OPEN(TMP_NETWINV_UNIT, FILE=TRIM(dir)//'data/netwinv', ACTION='READ', IOSTAT=i)
	IF (i /= 0) CALL report_error('Error opening tmp netwinv file', &
    	'File IO Error', pcom, 1)

	! Read in number of isotopes from tmp netwinv file
	READ(TMP_NETWINV_UNIT, '(I5)', IOSTAT=i) tmp_ny
    IF (i /= 0) CALL report_error('Error reading isotope count from tmp netwinv', &
         'File IO Error', pcom, 1)
	CLOSE(TMP_NETWINV_UNIT)

	ALLOCATE(tmp_iso_array(tmp_ny), STAT=i)
    IF (i /= 0) CALL report_error('Error allocating tmp isotope array', &
         'Array Allocation', pcom, 1)

	! Open the tmp sunet file
	OPEN(TMP_SUNET_UNIT, FILE=TRIM(dir)//'data/sunet', ACTION='READ', IOSTAT=i)
	IF (i /= 0) CALL report_error('Error opening tmp sunet file', &
    	'File IO Error', pcom, 1)

	! Read in number of isotopes from tmp sunet file
	READ(TMP_SUNET_UNIT, '(A5)', IOSTAT=i) (tmp_iso_array(counter), counter = 1, tmp_ny)
    IF (i /= 0) CALL report_error('Error reading isotopes from tmp sunet', &
         'File IO Error', pcom, 1)
	CLOSE(TMP_SUNET_UNIT)

	IF(INDEX(init_abund_path, 'Xray_Burst_Abund') /= 0) THEN
		orig_ny = 304
	ELSE IF(INDEX(init_abund_path, 'Xray_Burst_Expanded_Abund') /= 0) THEN
		orig_ny = 902
	ELSE IF(INDEX(init_abund_path, 'Nova_UpToSe_Abund') /= 0) THEN
        orig_ny = 422
    ELSE IF(INDEX(init_abund_path, 'Post_BBN_Abund') /= 0) THEN
        orig_ny = 422
    ELSE IF(INDEX(init_abund_path, 'Nova_0') /= 0) THEN
        orig_ny = 422
	ELSE IF(INDEX(init_abund_path, 'Nova_UpToCr_Abund') /= 0) THEN
	    orig_ny = 169
	ELSE IF(INDEX(init_abund_path, 'CO_UpToCr_Abund') /= 0) THEN
        orig_ny = 169
    ELSE IF(INDEX(init_abund_path, 'CO_UpToSe_Abund') /= 0) THEN
        orig_ny = 422
    ELSE IF(INDEX(init_abund_path, 'Cold_R_Process_Abund') /= 0) THEN
        orig_ny = 4510
	ELSE IF(INDEX(init_abund_path, 'R_Process_Abund') /= 0) THEN
        orig_ny = 4510
    ELSE IF(INDEX(init_abund_path, 'Magkotsios') /= 0) THEN
        orig_ny = 204
	ELSE IF(INDEX(init_abund_path, 'Other_Abund') /= 0) THEN
        orig_ny = 169
	END IF

	ALLOCATE(orig_iso_array(orig_ny), STAT=i)
    IF (i /= 0) CALL report_error('Error allocating orig isotope array', &
         'Array Allocation', pcom, 1)

    ALLOCATE(orig_ab_array(orig_ny), STAT=i)
    IF (i /= 0) CALL report_error('Error allocating orig abundances array', &
         'Array Allocation', pcom, 1)

	! Open original init_ab and read in abundances into orig_ab_array
	OPEN(ORIG_INIT_AB_UNIT, FILE=TRIM(init_abund_path), ACTION='READ', IOSTAT=i)
	IF (i /= 0) CALL report_error('Error opening original init_ab file', &
    	'File IO Error', pcom, 1)

	! Read in original isotope names from init_abund_path
    READ(ORIG_INIT_AB_UNIT, '(A)', IOSTAT=i) abund_desc
    IF (i /= 0) CALL report_error('Error reading init_abund_path first line', &
    	'File IO Error', pcom, 1)

	READ(ORIG_INIT_AB_UNIT, '(4(A5,15X))', IOSTAT=i) &
		(orig_iso_array(counter), counter=1, orig_ny)

	IF (i /= 0) CALL report_error('Error reading isotopes from init_abund_path', &
    	'File IO Error', pcom, 1)

	REWIND(ORIG_INIT_AB_UNIT)

	! Read in original abundances from init_abund_path
	READ(ORIG_INIT_AB_UNIT, *, IOSTAT=i)

	IF (i /= 0) CALL report_error('Error reading init_abund_path first line', &
    	'File IO Error', pcom, 1)

	READ(ORIG_INIT_AB_UNIT, '(4(5X,ES14.7,1X))', IOSTAT=i) &
		(orig_ab_array(counter), counter=1, orig_ny)

	IF (i /= 0) CALL report_error('Error reading abundances from init_abund_path', &
    	'File IO Error', pcom, 1)

	CLOSE(ORIG_INIT_AB_UNIT)

	OPEN(TMP_INIT_AB_UNIT, FILE=TRIM(dir)//'init_ab', STATUS='REPLACE', ACTION='WRITE', IOSTAT=i)

	IF (i /= 0) CALL report_error('Error opening tmp init_ab file', &
    	'File IO Error', pcom, 1)

    counter = 1

	! Write abund_desc to tmp init_ab
	WRITE(TMP_INIT_AB_UNIT, '(A)', IOSTAT=i) abund_desc

	IF (i /= 0) CALL report_error('Error writing abund desc to tmp init_ab file', &
    	'File IO Error', pcom, 1)

	ab_array = 0.0
	iso_array = ''
	ab_counter = 1

    ! Write tmp init_ab file
    DO counter = 1, tmp_ny

		iso_array(ab_counter) = tmp_iso_array(counter)
		orig_iso_index = orig_iso_index_function(tmp_iso_array(counter), orig_iso_array, orig_ny)
		IF(orig_iso_index /= -1) THEN
			ab_array(ab_counter) = orig_ab_array(orig_iso_index)
		ELSE
			ab_array(ab_counter) = 0.0
		END IF

		IF(ab_counter == 4) THEN

			WRITE(TMP_INIT_AB_UNIT, '(A5,ES14.7,1X,A5,ES14.7,1X,A5,ES14.7,1X,A5,ES14.7)', IOSTAT=i) &
				  iso_array(1), ab_array(1) &
				, iso_array(2), ab_array(2) &
				, iso_array(3), ab_array(3) &
				, iso_array(4), ab_array(4)
			IF (i /= 0) CALL report_error('ab_counter = 4', &
    			'File IO Error', pcom, 1)
			ab_counter = 1

		ELSE IF(counter == tmp_ny) THEN

			SELECT CASE (ab_counter)
			CASE (1)
				WRITE(TMP_INIT_AB_UNIT, '(A5,ES14.7)') &
				  iso_array(1), ab_array(1)
				IF (i /= 0) CALL report_error('ab_counter = 1', &
    			  'File IO Error', pcom, 1)
			CASE (2)
				WRITE(TMP_INIT_AB_UNIT, '(A5,ES14.7,1X,A5,ES14.7)') &
				  iso_array(1), ab_array(1) &
				, iso_array(2), ab_array(2)
				IF (i /= 0) CALL report_error('ab_counter = 2', &
    			  'File IO Error', pcom, 1)
			CASE (3)
				WRITE(TMP_INIT_AB_UNIT, '(A5,ES14.7,1X,A5,ES14.7,1X,A5,ES14.7)') &
				  iso_array(1), ab_array(1) &
				, iso_array(2), ab_array(2) &
				, iso_array(3), ab_array(3)
				IF (i /= 0) CALL report_error('ab_counter = 3', &
    			  'File IO Error', pcom, 1)
			END SELECT

		ELSE

			ab_counter = ab_counter + 1

		END IF

    END DO

	CLOSE(TMP_INIT_AB_UNIT)

	DEALLOCATE(orig_iso_array, STAT=i)
	IF (i /= 0) CALL report_error('Error deallocating orig isotope array', &
         'Array Deallocation', pcom, 1)
    DEALLOCATE(orig_ab_array, STAT=i)
	IF (i /= 0) CALL report_error('Error deallocating orig abundace array', &
         'Array Deallocation', pcom, 1)
    DEALLOCATE(tmp_iso_array, STAT=i)
	IF (i /= 0) CALL report_error('Error deallocating tmp isotope array', &
         'Array Deallocation', pcom, 1)

  END SUBROUTINE create_init_ab

  !---------------------------------------------------------------------
  FUNCTION orig_iso_index_function(iso_name, iso_array, iso_array_length)
    INTEGER(KIND=4),  INTENT(IN) :: iso_array_length
    CHARACTER(LEN=5), INTENT(IN) :: iso_name
    CHARACTER(LEN=5) &
    , DIMENSION(iso_array_length) &
    , INTENT(IN)                 :: iso_array
    INTEGER(KIND=4)              :: counter
    CHARACTER(LEN=5)             :: iso_test_1,iso_test_2

    counter = 1
    orig_iso_index_function = -1
    search_loop: DO WHILE(counter<=iso_array_length)
	iso_test_1 = ADJUSTL(iso_name)
	iso_test_1 = ADJUSTR(iso_test_1)
	iso_test_2 = ADJUSTL(iso_array(counter))
	iso_test_2 = ADJUSTR(iso_test_2)
        IF(iso_test_1==iso_test_2) THEN
            orig_iso_index_function = counter
            EXIT search_loop
        END IF
        counter = counter + 1
    END DO search_loop
  END FUNCTION orig_iso_index_function

  !---------------------------------------------------------------------
  SUBROUTINE abort_em_syn(pcom)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    INTEGER(KIND=4)                  :: i
    CHARACTER(LEN=200)               :: dir

    dir = get_em_tmp_dir(pcom)

    ! Kill any programs running in the background
    CALL kill_bg_proc(pcom,dir)

    ! Delete rate_gen screen output
    i = safe_shell('/bin/rm -f '//TRIM(dir)//'redir')

    WRITE(*,'(A)') 'STOP=SUCCESS'

  END SUBROUTINE abort_em_syn

  !---------------------------------------------------------------------
  SUBROUTINE em_syn_update(pcom)
    IMPLICIT NONE
    ! Use the print_long_string subroutine if MAXCHAR is above 1024
    INTEGER(KIND=4),PARAMETER        :: MAXCHAR = 1000
    TYPE(cina_common),INTENT(IN)     :: pcom
    INTEGER(KIND=4)                  :: i,p,c,e,lastzone
    INTEGER(KIND=4)                  :: zone_cnt,zonemap(MAX_ZONES)
    CHARACTER(LEN=1)                 :: chr
    CHARACTER(LEN=200)               :: dir,tmps
    CHARACTER(LEN=MAXCHAR)           :: buffer,save_buffer
    LOGICAL(KIND=1)                  :: loop

    dir = get_em_tmp_dir(pcom)

    CALL get_lastzone(pcom,TRIM(dir)//'lastzone',lastzone)
    CALL get_zonemap(pcom,TRIM(dir)//'zonemap',zonemap,zone_cnt)
    IF (lastzone > 0) THEN
       WRITE(*,'(A,I0)') 'ZONE=',zonemap(lastzone)
    ELSE
       WRITE(*,'(A)') 'ZONE=0'
    END IF

    ! Get the PID of xnet
    p = bg_pid(pcom,dir)
    IF (p <= 0) CALL report_error('The element synthesis simulator was not started.','Improper usage',pcom,1)
    ! See if it is running
    i = KILL(p,18)  ! Send the process the SIGCONT signal (Continue process if stopped)
    SELECT CASE (i)
    CASE (0)
       ! process is running
       WRITE(*,'(A)') 'SYNTHESIS=RUNNING'
    CASE (-1)
       ! process is not running
       WRITE(*,'(A)') 'SYNTHESIS=COMPLETE'
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

    ! Read in xnet screen output
    OPEN(12,FILE=TRIM(dir)//'redir',IOSTAT=i,ACTION='READ',POSITION='APPEND')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open element synthesis simulator screen output.  IOSTAT=',i
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
    IF (i /= 0) CALL report_error('Could not seek in element synthesis simulator screen output (1).', &
         'File input/output',pcom,1)
    loop = .TRUE.
    ! Store the position of the last newline in p
    DO WHILE (loop)
       READ(12,'(A)',IOSTAT=i) chr
       IF (i > 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read element synthesis simulator screen output.  IOSTAT=',i
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
       IF (i /= 0) CALL report_error('Could not seek in element synthesis simulator screen output (2).', &
            'File input/output',pcom,1)
       ! Find the position after the first newline character
       READ(12,'(A1)',IOSTAT=i) chr
       c = FTELL(12) - 1
    END IF
    !`print *,c
    i = FSEEK(12,c,0)                ! Position file to just after previous cursor
    IF (i /= 0) CALL report_error('Could not seek in element synthesis simulator screen output (3).', &
         'File input/output',pcom,1)

    save_buffer = ''
    DO WHILE (c < e)
       READ(12,'(A)',IOSTAT=i) buffer
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read element synthesis screen output.  IOSTAT=',i
          CALL report_error(TRIM(tmps),'File input/output',pcom,1)
       END IF

       c = FTELL(12)
       save_buffer = TRIM(save_buffer) // TRIM(buffer) // ACHAR(8)
    END DO
    CLOSE(12)

    ! Use the print_long_string subroutine if MAXCHAR is above 1024
    WRITE(*,'(2A)') 'TEXT=',TRIM(save_buffer)

    ! Record new cursor
    OPEN(11,FILE=TRIM(dir)//'cur',IOSTAT=i,ACTION='WRITE')
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Can not open update file (2).  IOSTAT=',i
       CALL report_error(TRIM(tmps),'File input/output',pcom,1)
    END IF
    WRITE(11,'(I0)',IOSTAT=i) e

    CLOSE(11)

  END SUBROUTINE em_syn_update

  !---------------------------------------------------------------------
  SUBROUTINE add_to_file(item,file,iostat,err_str)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)    :: item,file
    CHARACTER(LEN=200),INTENT(OUT) :: err_str
    INTEGER(KIND=4),INTENT(OUT)    :: iostat
    CHARACTER(LEN=LEN(item)+2)     :: line

    ! First check if item is already in file.
    ! If so, just return, otherwise add to file

    err_str = ''
    OPEN(20,FILE=file,ACTION='READ',IOSTAT=iostat)
    IF (iostat == 152) THEN
       ! Let iostat = 152 so that the next loop won't start
    ELSE IF (iostat /= 0) THEN
       WRITE(err_str,'(A,I0,3A)') 'Error ',iostat,' while opening file ',TRIM(file),' for read access'
       RETURN
    END IF

    DO WHILE (iostat == 0)
       READ(20,'(A)',IOSTAT=iostat) line
       IF ((iostat == 0) .AND. (item == line)) THEN
          CLOSE(20)
          RETURN
       ELSE IF (iostat > 0) THEN
          WRITE(err_str,'(A,I0,2A)') 'Error ',iostat,' while reading file ',TRIM(file)
          RETURN
       END IF
    END DO
    CLOSE(20)

    ! Item is not in file, so add it
    OPEN(20,FILE=file,ACTION='WRITE',IOSTAT=iostat,POSITION='APPEND')
    IF (iostat /= 0) THEN
       WRITE(err_str,'(A,I0,3A)') 'Error ',iostat,' while opening file ',TRIM(file),' for write access'
       RETURN
    END IF

    ! Add item
    WRITE(20,'(A)',IOSTAT=iostat) TRIM(item)
    IF (iostat /= 0) THEN
       WRITE(err_str,'(A,I0,2A)') 'Error ',iostat,' while writing file ',TRIM(file)
       RETURN
    END IF

    CLOSE(20)
  END SUBROUTINE add_to_file

  !---------------------------------------------------------------------
  SUBROUTINE em_syn_output(pcom)
    IMPLICIT NONE
    TYPE(cina_common)             :: pcom
    INTEGER(KIND=4)                :: step_num,iostat,kstep
    INTEGER(KIND=4)                :: zonemap(MAX_ZONES),zone_cnt
    REAL(KIND=8)                   :: final_temp,final_time,final_density
    CHARACTER(LEN=200)             :: dir,file

    dir = get_em_tmp_dir(pcom)
    CALL get_zonemap(pcom,TRIM(dir)//'zonemap',zonemap,zone_cnt)
    file = pad_name(TRIM(dir)//'step_num',1,zone_cnt)

    CALL get_em_step_num(20,file,step_num,iostat)
    IF (iostat /= 0) THEN
    !   WRITE(dir,'(A,I0,A)') 'Error ',iostat,' line 1784 reading number of steps in simulation'
       WRITE(dir,'(A,I0,A,A)') 'Error ',iostat,' line 1784 reading number of steps in simulation',file
       CALL report_error(TRIM(dir),'File input/output',pcom,1)
    END IF
    WRITE(*,'(A,I0)') 'STEPS=',step_num

    file = pad_name(TRIM(dir)//'timemap',1,zone_cnt)
    CALL open_em_timemap(20,file,iostat)
    IF (iostat /= 0) THEN
       WRITE(dir,'(A,I0,A)') 'Error ',iostat,' opening time map for simulation'
       CALL report_error(TRIM(dir),'File input/output',pcom,1)
    END IF

    DO WHILE (iostat == 0)
       CALL read_em_timemap(20,kstep,final_time,iostat)
       IF (iostat > 0) THEN
          WRITE(dir,'(A,I0,A)') 'Error ',iostat,' reading time map in simulation'
          CALL report_error(TRIM(dir),'File input/output',pcom,1)
       END IF
       IF (kstep == step_num) WRITE(*,'(A,1P,E13.7)') 'FINAL_TIME=',final_time
    END DO
    CLOSE(20)

    file = pad_name(TRIM(dir)//'thermo',1,zone_cnt)
    CALL open_em_thermo(20,file,iostat)
    IF (iostat /= 0) THEN
       WRITE(dir,'(A,I0,A)') 'Error ',iostat,' opening thermodynamic info for simulation'
       CALL report_error(TRIM(dir),'File input/output',pcom,1)
    END IF

    DO WHILE (iostat == 0)
       CALL read_em_thermo(20,kstep,final_temp,final_density,iostat)
       IF (iostat > 0) THEN
          WRITE(dir,'(A,I0,A)') 'Error ',iostat,' reading thermodynamic info for simulation'
          CALL report_error(TRIM(dir),'File input/output',pcom,1)
       END IF
       IF (kstep == step_num) THEN
          WRITE(*,'(A,1P,E13.7)') 'FINAL_TEMP=',final_temp
          WRITE(*,'(A,1P,E13.7)') 'FINAL_DENSITY=',final_density
       END IF
    END DO
    CLOSE(20)

    WRITE(*,'(A)') 'REPORT=Element Synthesis Simulation Complete'
  END SUBROUTINE em_syn_output

  !---------------------------------------------------------------------
  SUBROUTINE sunet_to_isolist(pcom,file,isolist)
    USE constants
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)  :: file
    INTEGER(KIND=2),INTENT(OUT)  :: isolist(0:MAX_Z,MAX_ISO)
    CHARACTER(LEN=100)           :: line
    CHARACTER(LEN=10)            :: special_num
    INTEGER(KIND=4)              :: z,a,s,c,i

    isolist = -1
    OPEN(22, FILE=file, IOSTAT=s, ACTION='READ')
    IF (s /= 0) CALL report_error('Unable to open ' // TRIM(file), &
         'File input/output',pcom,1)
    ! WRITE(*,'(A)')  'Reading input sunet from '// TRIM(file) // ACHAR(8)

    DO WHILE (s == 0)
       READ(22, '(A)', IOSTAT=s) line
       IF (s > 0) THEN
          CALL report_error('Unable to read ' // TRIM(file), &
               'File input/output',pcom,1)
       ELSE IF (s == 0) THEN
          line = ADJUSTL(line)
          c = 1
          CALL str2za(line,z,a,c,special_num)
          IF (z < 0 .OR. a < 0) CALL report_error( &
               'Error interpreting "' // TRIM(line) // '" in sunet file', &
               'Bad file format',pcom,1)

          ! Go through isolist and see if isotope is already in list, if not add it
          i = 1
          DO WHILE (i <= MAX_ISO)
             IF (isolist(z,i) == -1) THEN
                isolist(z,i) = a
                i = MAX_ISO + 2
             ELSE IF (isolist(z,i) == a) THEN
                i = MAX_ISO + 2
             ELSE
                i = i + 1
             END IF
          END DO

          IF (i /= MAX_ISO + 2) CALL report_error( &
               'Buffer is too small to hold all isotopes in sunet file', &
               'Improper usage',pcom,1)
       END IF
    END DO

    CLOSE(22)
  END SUBROUTINE sunet_to_isolist

  !---------------------------------------------------------------------
  SUBROUTINE filter_rlib_sunet(old_lib,new_lib,pcom,iso_list,sunet,meta_stable, &
       report,overwrite)
    USE constants
    USE convert
    USE reactionstrings
    USE rate_man
    USE rate_man_core
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: old_lib,new_lib,sunet
    INTEGER(KIND=2),INTENT(IN)    :: iso_list(0:MAX_Z,MAX_ISO)
    LOGICAL(KIND=4),INTENT(IN)    :: meta_stable,overwrite
    CHARACTER(LEN=*),INTENT(INOUT):: report
    OPTIONAL                      :: overwrite

    INTEGER(KIND=2)               :: old_iso_list(0:MAX_Z,MAX_ISO)
    CHARACTER(LEN=MAX_RID_LEN)    :: rids(MAX_RATES),tmp_rid
    CHARACTER(LEN=MAX_RATE_LEN)   :: properties
    CHARACTER(LEN=200)            :: tmps
    INTEGER(KIND=4)               :: i,j,found,count,num_rids,rtype
    INTEGER(KIND=4)               :: z,a,a_index,t
    LOGICAL(KIND=4)               :: link,found_rate
    TYPE(reactionparticles)       :: r

    CALL read_rlib_iso_list(old_lib,pcom,old_iso_list)

    ! Check if new_lib exists already
    CALL find_rlib_list_index(new_lib,lib_index=i)
    IF (i > -1) THEN
       link = .FALSE.
       IF (PRESENT(overwrite)) THEN
          IF (overwrite) link = .TRUE.
       END IF

       IF (link) THEN
          CALL rm_rlib(new_lib,pcom,move_to_trash = .FALSE.)
       ELSE
          CALL report_error('New library "' // TRIM(new_lib) // &
               '"already exists','Improper usage',pcom,1)
       END IF
    END IF
    CALL add_rlib(new_lib,pcom,'USER',mkdir=.TRUE.)

    IF (meta_stable) THEN
       tmp_rid = 'meta stable'
    ELSE
       tmp_rid = 'stable'
    END IF
    report = TRIM(report) // 'Making new library containing ' // &
         TRIM(old_lib) // ' rates with ' // TRIM(tmp_rid) // &
         ' Al26 rates and isotopes in ' // TRIM(sunet) // ACHAR(8)

    found = 0  ! found is the number of proper Al26 rates found
    count = 0  ! count is the number of rates in new_lib

    DO z = 0, MAX_Z
       IF (iso_list(z,1) > 0) THEN
          a_index = 1
          a = iso_list(z,a_index)
       ELSE
          a = -1
       END IF
       DO WHILE (a > 0)
          found_rate = .FALSE.

          ! Make sure isotope in iso_list is also in old_iso_list
          IF (exist_in_isolist(old_iso_list,z,a)) THEN

             CALL read_rid_list(old_lib,pcom,z,a,rids,num_rids)
             CALL open_rate_info(new_lib,pcom,z,a)
             found_rate = .TRUE.

             DO i = 1, num_rids
                CALL decode_rid(rids(i),reac_str=tmps)

                ! Check if this rate is meta-stable (*6Al)
                ! rtype is 0 when reaction should be copied regardless of state
                ! rtype is 1 when reaction contains -6Al or *6Al
                ! rtype is 2 when reaction contains 26Al

                IF (INDEX(tmps,'26Al') > 0) THEN
                   rtype = 2
                ELSE IF (INDEX(tmps,'-6Al') > 0) THEN
                   rtype = 1
                ELSE IF (INDEX(tmps,'*6Al') > 0) THEN
                   rtype = 1
                ELSE
                   rtype = 0
                END IF

                ! Make rtype of rates that shouldn't be copied negative
                IF (meta_stable .AND. rtype == 2) rtype = -2
                IF (.NOT. meta_stable .AND. rtype == 1) rtype = -1

                IF (rtype >= 0) THEN
                   ! Read reaction string into r
                   CALL read_reac_str(r,tmps,properties)
                   IF (properties /= '') CALL report_error('Error (' // &
                        TRIM(properties) // ') reading "' // TRIM(tmps) // &
                        '"','Improbable',pcom,1)

                   ! Look for reactants that aren't in iso_list
                   t = getreac_num(r)
                   j = 1
                   ! link becomes FALSE when reaction contains an isotope not in iso_list
                   link = .TRUE.
                   DO WHILE (j <= t .AND. link)
                      IF (.NOT. exist_in_isolist(iso_list,getreac_z(r,j),getreac_a(r,j))) &
                           link = .FALSE.
                      j = j + 1
                   END DO

                   ! Look for products that aren't in iso_list
                   t = getprod_num(r)
                   j = 1
                   ! link becomes FALSE when reaction contains an isotope not in iso_list
                   link = .TRUE.
                   DO WHILE (j <= t .AND. link)
                      IF (.NOT. exist_in_isolist(iso_list,getprod_z(r,j),getprod_a(r,j))) &
                           link = .FALSE.
                      j = j + 1
                   END DO

                   ! Check that all isotopes invoved in reaction are in iso_lists
                   IF (.NOT. link) rtype = -4
                END IF

                IF (rtype > 0) found = found + 1

                IF (rtype >= 0) THEN
                   count = count + 1

                   ! Check if rids(i) is a link but don't modify rids(i)
                   tmp_rid = rids(i)
                   CALL correct_rid(tmp_rid,new_lib,link)

                   ! Don't Load properties if a link
                   IF (link) THEN
                      properties = ''
                   ELSE
                      properties = read_rate_info(rids(i),pcom,'')
                   END IF

                   CALL change_rate_info(rids(i),pcom,properties)
                END IF
             END DO
          END IF

          tmp_rid = ''    ! Discard lib_info
          IF (found_rate) CALL save_rate_info(pcom,report,tmp_rid,plevel=2)

          ! Update a
          a_index = a_index + 1
          IF (a_index <= MAX_ISO) THEN
             a = iso_list(z,a_index)
          ELSE
             a = -1
          END IF
       END DO
    END DO

    ! Put library info into properties
    properties = ''
    IF (meta_stable) THEN
       tmp_rid = 'meta stable'
    ELSE
       tmp_rid = 'stable'
    END IF

    CALL set_prop_value('Library Recipe','New library containing ' // &
         TRIM(old_lib) // ' rates with ' // TRIM(tmp_rid) // &
         ' Al26 rates and isotopes in ' // TRIM(sunet),properties, &
         RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
    CALL set_prop_value('Creation Date',get_date() // ' ' // &
         get_time(),properties,RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)

    CALL save_rlib_info(new_lib,pcom,properties)
    CALL save_rlib_iso_list(new_lib,pcom)
    ! CALL save_rlib_list('USER',pcom)

    WRITE(report,'(A,I0,A)') TRIM(report) // 'New library contains ', &
         count,' rates.' // ACHAR(8)

    IF (found == 0) THEN
       IF (meta_stable) THEN
          report = TRIM(report) // &
!               'CAUTION! No meta stable Al26 rates were found in ' // &
               'No meta stable Al26 rates were found in ' // &
               TRIM(old_lib)// ACHAR(8)
       ELSE
          report = TRIM(report) // &
!               'CAUTION! No stable Al26 rates were found in ' // &
               'No stable Al26 rates were found in ' // &
               TRIM(old_lib)// ACHAR(8)
       END IF
    END IF
  END SUBROUTINE filter_rlib_sunet

  !---------------------------------------------------------------------
  FUNCTION exist_in_isolist(iso_list,z,a)
    USE constants
    IMPLICIT NONE
    INTEGER(KIND=2),INTENT(IN) :: iso_list(0:MAX_Z,MAX_ISO)
    INTEGER(KIND=4),INTENT(IN) :: z,a
    LOGICAL(KIND=4)            :: exist_in_isolist
    INTEGER(KIND=4)            :: i,m

    exist_in_isolist = .FALSE.

    ! Check for valid z
    IF (z > MAX_Z .OR. z < 0) RETURN

    i = 1
    m = iso_list(z,1)

    DO WHILE (m > 0)
       IF (m == a) THEN
          exist_in_isolist = .TRUE.
          RETURN
       END IF
       i = i + 1
       IF (i > MAX_ISO) RETURN
       m = iso_list(z,i)
    END DO
  END FUNCTION exist_in_isolist

  !---------------------------------------------------------------------
  FUNCTION pad_name(name,num,max)
    !-----------------------------------------------------------------------------
    !  This routine appends the number num (padded with "0"s up to the size of
    !  the number max) to the character variable name.
    !-----------------------------------------------------------------------------
    CHARACTER(LEN=*),INTENT(IN)      :: name
    INTEGER(KIND=4),INTENT(IN)       :: num,max
    CHARACTER(LEN=LEN(name)+9)       :: pad_name
    CHARACTER(LEN=9)                 :: num_string,form
    CHARACTER(LEN=1)                 :: lmax_string
    INTEGER(KIND=4)                  :: lmax

    !  Find character length of max
    lmax=INT(LOG10(float(max)))+1
    WRITE(lmax_string,'(I1)') lmax

    !  Construct Format Spec and write num as zero padded string
    form='(I'//lmax_string//'.'//lmax_string//')'
    WRITE(num_string,form) num

    !  Append num_ string to name
    pad_name=TRIM(name)//num_string
  END FUNCTION pad_name

  !---------------------------------------------------------------------
  SUBROUTINE get_em_weighted_abund(pcom)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)  :: pcom
    CHARACTER(LEN=200)             :: path,dir,file,tmps
    CHARACTER(LEN=LEN(pcom%NOTES)) :: isotopes
    REAL(KIND=8)                   :: weights(MAX_ZONES),mapped_weights(MAX_ZONES)
    INTEGER(KIND=4)                :: count,zones(MAX_ZONES),zonemap(MAX_ZONES)
    INTEGER(KIND=4)                :: i,zone,zone_cnt,step_num
    INTEGER(KIND=4)                :: sel_iso(MAX_NZ_ABUNDANCES),sel_iso_num
    INTEGER(KIND=4)                :: iso_ind(MAX_NZ_ABUNDANCES,MAX_ZONES),num_nz(MAX_ZONES)
    INTEGER(KIND=4)                :: z,a,s,alli(5),kstep
    INTEGER(KIND=4)                :: iostatus
    REAL(KIND=8)                   :: abund(MAX_NZ_ABUNDANCES,MAX_ZONES)
    REAL(KIND=8)                   :: abund_sum(0:MAX_NZ_ABUNDANCES)
    REAL(KIND=8)                   :: abund_by_index(0:MAX_NZ_ABUNDANCES,MAX_ZONES)
    LOGICAL                        :: loop,found_an_isotope
    TYPE(em_isomap)                :: isomap

    path = cina_decode(pcom%BODY)
    dir = em_path_to_dir(path,pcom)
    isotopes = cina_decode(pcom%NOTES)

    CALL get_zonemap(pcom,TRIM(dir)//'/zonemap',zonemap,zone_cnt)

    ! has weights for all zones
    CALL read_weights(pcom,TRIM(dir)//'/weights',weights,count,zones)

    ! this may be not be a full multizone simulation so find weights for zones used
    mapped_weights = 0
    DO zone = 1, zone_cnt
       DO i = 1, count
          IF (zonemap(zone) == zones(i)) mapped_weights(zone) = weights(i)
       END DO
    END DO

    ! Quick fix until all zone 0 nova simulations are gone
    IF (zonemap(1) == 0) mapped_weights(1) = 1.0

    ! Load isomap
    CALL load_em_isomap(20,TRIM(dir)//'/isomap',isomap,i,isotopes)
    IF (i /= 0) CALL report_error(TRIM(isotopes),'Improper usage',pcom,1)
    CLOSE(20)

    ! Record if sel_iso_num should be > 0
    IF (isotopes == '') THEN
       found_an_isotope = .TRUE.
    ELSE
       found_an_isotope = .FALSE.
    END IF

!    write (99, *) 'found_an_isotope ',found_an_isotope
    sel_iso_num = 0
    IF (isotopes /= '') THEN
       ! Parse isotope string into indicies
       loop = .TRUE.
       DO WHILE (loop)
          loop = next_in_list(tmps,isotopes,ACHAR(9))
          READ(tmps,'(2I)',IOSTAT=i) z,a
          IF (i /= 0) THEN
             WRITE(file,'(A,I0,3A)') 'Error ',i,' interpreting isotope "',TRIM(tmps),'"'
             CALL report_error(file,'Improper usage',pcom,1)
          END IF

          ! Get the isotope index in isomap for this isotope
          s = get_index_to_isotope(isomap,z,a,alli=alli)
          i = 1
          DO WHILE (alli(i) >= 0)
             IF (alli(i) >= 0) THEN
                sel_iso_num = sel_iso_num + 1
                IF (sel_iso_num <= UBOUND(sel_iso,1)) THEN
                   sel_iso(sel_iso_num) = alli(i)
                ELSE
                   WRITE(path,'(A,I0)') 'Too many isotopes were selected.  The maximum is currently ',MAX_NZ_ABUNDANCES
                   CALL report_error(path,'Developer Reminder',pcom,1)
                END IF
             END IF
             i = i + 1
          END DO
       END DO
    END IF

    IF (sel_iso_num > 0) found_an_isotope = .TRUE.
!    write (99, *) 'found_an_isotope - sel_iso_num ',found_an_isotope,' - ',sel_iso_num

    IF (.NOT. found_an_isotope) THEN
       ! None of the selected isotopes were found in simulation
       WRITE(*,'(A)') ''
       RETURN
    END IF

    ! Read final abundances for all zones into abund(:,zone) array
    DO zone = 1, zone_cnt
       ! Make filename for number of steps
       file = pad_name(TRIM(dir)//'/step_num',zone,zone_cnt)

       ! Get number of timesteps
       CALL get_em_step_num(20,file,step_num,i)
       IF (i /= 0) THEN
       !   WRITE(file,'(A,I0,A,I0)') 'Error ',i,' line 2250 getting number of steps for zone ',zone
          WRITE(file,'(A,I0,A,I0,A)') 'Error ',i,' line 2250 getting number of steps for zone ',zone,file
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF
       !print *,'zone=',zone,' step_num=',step_num

       ! Make filename for abundances
       file = pad_name(TRIM(dir)//'/abund',zone,zone_cnt)

       ! Open abundances file
       CALL open_em_abund(20,21,file,i)
       IF (i /= 0) THEN
          WRITE(path,'(A,I0,A,A)') 'Error ',i,' opening abund file',file
          CALL report_error(TRIM(path)//TRIM(file),'File input/output',pcom,1)
       END IF

       ! Seek to last timestep
       CALL seek_em_abund(20,21,step_num-1,i)
       IF (i /= 0) THEN
          WRITE(file,'(A,I0,A,I0)') 'Error ',i,' seeking to last time step for zone ',zone
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF

       CALL read_em_abund(20,21,kstep,num_nz(zone),iso_ind(:,zone),abund(:,zone),i)
       IF (i /= 0) THEN
          WRITE(file,'(A,I0,A,I0)') 'Error ',i,' reading abundances for zone ',zone
          CALL report_error(TRIM(file),'Improper usage',pcom,1)
       END IF
    END DO
    !print *,'all loaded'

    abund_by_index = 0.0
    abund_sum = 0.0

    DO zone = 1, zone_cnt
       DO s = 1, num_nz(zone)
          i = iso_ind(s,zone)
          abund_by_index(i,zone) = abund(s,zone) * &
               mapped_weights(zone)! * isomap%a(i)
       END DO
    END DO
    !print *,'all ordered by index'

    DO s = 0, isomap%last_index
       abund_sum(s) = SUM(abund_by_index(s,1:zone_cnt),1)
    END DO

    found_an_isotope = .FALSE.
    IF (sel_iso_num == 0) THEN
       ! Return all non-zero abundances
       DO s = 0, isomap%last_index
          IF (abund_sum(s) /= 0.0) THEN
             WRITE(*,'(I0,A,I0,A,I0,A,E20.12E3)',ADVANCE='NO') s,',',zone_cnt,'=', &
                  zonemap(1),',',abund_by_index(s,1)
             DO zone = 2, zone_cnt
                WRITE(*,'(A,I0,A,E20.12E3)',ADVANCE='NO') &
                     ACHAR(9),zonemap(zone),',',abund_by_index(s,zone)
             END DO
             WRITE(*,'(A,E20.12E3)') ACHAR(9),abund_sum(s)
             found_an_isotope = .TRUE.
          END IF
       END DO
    ELSE
       found_an_isotope = .FALSE.
       DO s = 1, sel_iso_num
          IF (abund_sum(sel_iso(s)) /= 0.0) THEN
             WRITE(*,'(I0,A,I0,A,I0,A,E20.12E3)',ADVANCE='NO') sel_iso(s),',',zone_cnt,'=', &
                  zonemap(1),',',abund_by_index(sel_iso(s),1)
             DO zone = 2, zone_cnt
                WRITE(*,'(A,I0,A,E20.12E3)',ADVANCE='NO') &
                     ACHAR(9),zonemap(zone),',',abund_by_index(sel_iso(s),zone)
             END DO
             WRITE(*,'(A,E20.12E3)') ACHAR(9),abund_sum(sel_iso(s))
             found_an_isotope = .TRUE.
          END IF
       END DO
    END IF

    ! Print newline if no output
!    IF (.NOT. found_an_isotope) WRITE(*,'(A)') ''
    IF (.NOT. found_an_isotope) WRITE(*,'(A)') 'SORRY NO ISOTOPES FOUND!!!'
!    CLOSE(99)
  END SUBROUTINE get_em_weighted_abund

  !---------------------------------------------------------------------
  SUBROUTINE read_weights(pcom,file,weights,count,zones)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)  :: pcom
    CHARACTER(LEN=*),INTENT(IN)    :: file
    INTEGER(KIND=4),INTENT(OUT)    :: count,zones(:)
    REAL(KIND=8),INTENT(OUT)       :: weights(:)
    INTEGER(KIND=4)                :: i,zone
    OPTIONAL                       :: zones

    IF (PRESENT(zones)) zones = -1
    weights = 0.0
    count = 0

    OPEN(22,FILE=file,ACTION='READ',IOSTAT=i)
    IF (i /= 0) CALL report_error('Unable to open weights file', &
         'File input/output',pcom,1)

    DO WHILE (i == 0)
       READ(22,*,IOSTAT=i) zone,weights(count+1)
       IF (i > 0) THEN
          CALL report_error('Unable to read weights file', &
               'File input/output',pcom,1)
       ELSE IF (i == 0) THEN
          count = count + 1
          IF (PRESENT(zones)) zones(count) = zone
       END IF
    END DO
    CLOSE(22)
  END SUBROUTINE read_weights

END MODULE element_sim
