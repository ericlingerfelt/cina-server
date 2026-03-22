MODULE nuc_data_man

  USE IFLPORT

  USE cina_core

  USE nuc_data_core

  USE constants

  USE convert



  PUBLIC



CONTAINS

  !---------------------------------------------------------------------

  FUNCTION nuc_data_man_ver()

    !PURPOSE = Return the cvs revision number for this file

    !STATUS = Complete and tested

    IMPLICIT NONE

    CHARACTER(LEN=10)                :: nuc_data_man_ver

    CHARACTER(LEN=20),PARAMETER      :: N_D_MAN_VERSION = '$Revision: 1.1.1.1 $'



    nuc_data_man_ver = N_D_MAN_VERSION(12:LEN_TRIM(N_D_MAN_VERSION)-2)



  END FUNCTION nuc_data_man_ver



  !---------------------------------------------------------------------

  FUNCTION nuc_data_man_temp_path(pcom)

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN)    :: pcom

    CHARACTER(LEN=200)               :: nuc_data_man_temp_path



    nuc_data_man_temp_path = TRIM(TEMP_PATH)//pcom%ID//'/nuc_data/'

  END FUNCTION nuc_data_man_temp_path



  !---------------------------------------------------------------------

  SUBROUTINE get_nuc_data_set_list(pcom)

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN)    :: pcom

    CHARACTER(LEN=100)               :: list,item

    CHARACTER(LEN=2000)              :: set_list,error_str

    INTEGER(KIND=4)                  :: status,tab_index

    LOGICAL                          :: loop = .TRUE.



    list = cina_decode(pcom%BODY)

    DO WHILE (loop)

       loop = next_in_list(item,list,ACHAR(9))

       CALL read_NDS_list(item,pcom,set_list)

       WRITE(*,'(3A)') TRIM(item),'=',TRIM(set_list)

    END DO

  END SUBROUTINE get_nuc_data_set_list



  ! GET NUC DATA SET INFO



  !---------------------------------------------------------------------

  SUBROUTINE get_nuc_data_set_isotopes(pcom)

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    CHARACTER(LEN=2000)           :: list,item,error_str

    CHARACTER(LEN=MAX_ISO*8)      :: z_list

    INTEGER(KIND=4)               :: status,z,m

    INTEGER(KIND=2)               :: iso_list(0:MAX_Z,MAX_ISO) = -1

    LOGICAL                       :: loop = .TRUE.,first



    ! Load rate library lists

    CALL read_NDS_list('PUBLIC',pcom)

    CALL read_NDS_list('SHARED',pcom)

    CALL read_NDS_list('USER',pcom)



    list = cina_decode(pcom%BODY)

    DO WHILE (loop)

       loop = next_in_list(item,list,ACHAR(9))



       CALL read_NDS_iso_list(item,pcom,iso_list)



       WRITE(*,'(2A)',ADVANCE='NO') TRIM(item),'='

       ! Print iso_list to stdout

       ! This is set to .FALSE. once an isotope is printed

       first = .TRUE.

       DO z = 0, MAX_Z

          ! Put the iso_list for each Z into z_list, then print it out

          z_list = ''

          status = 1          ! Use status as a counter of isotopes

          DO WHILE (status <= MAX_ISO)

             m = iso_list(z,status)

             IF (m > 0) THEN

                WRITE(error_str,'(I0,A1,I0)') z,',',m

                z_list = TRIM(z_list)//ACHAR(9)//error_str

             ELSE

                ! Exit loop

                status = MAX_ISO + 1

             END IF

             status = status + 1

          END DO

          ! Remove initial tab

          IF (first .AND. (LEN_TRIM(z_list) > 0)) THEN

             z_list = z_list(2:)

             first = .FALSE.

          END IF

          WRITE(*,'(A)',ADVANCE='NO') TRIM(z_list)

       END DO



       WRITE(*,'(A)') ''      ! Print newline

    END DO

  END SUBROUTINE get_nuc_data_set_isotopes



  !---------------------------------------------------------------------

  SUBROUTINE get_nuc_data_list(pcom)

    USE reactionstrings

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    CHARACTER(LEN=MAX_NDS_LEN)    :: set

    CHARACTER(LEN=40)             :: isotope_list

    CHARACTER(LEN=40)             :: rtypes

    CHARACTER(LEN=4)              :: t

    INTEGER(KIND=4)               :: z,a,i,num_NDids

    CHARACTER(LEN=MAX_ND_ID_LEN)  :: NDids(MAX_ND)



    ! Load rate set lists

    CALL read_NDS_list('PUBLIC',pcom)

    CALL read_NDS_list('SHARED',pcom)

    CALL read_NDS_list('USER',pcom)



    set = cina_decode(pcom%BODY)

    isotope_list = cina_decode(pcom%NOTES)



    ! Add commas to ease the searching

    rtypes = ',' // TRIM(pcom%REACTION) // ','



    ! Extract z and a from isotope_list

    i = INDEX(isotope_list,',')

    READ(isotope_list(:i-1),'(I)') z

    READ(isotope_list(i+1:),'(I)') a



    ! Get rate list

    CALL read_NDid_list(set,pcom,z,a,NDids,num_NDids)



    ! Print out the rate ids if they match one of the rtypes

    DO i = 1, num_NDids

       ! Get type for this rate

       t = ',' // NDids(i)(1:2) // ','

       IF ((INDEX(rtypes,t) > 0) .OR. (rtypes == ',, ')) THEN

          WRITE(*,'(A)') TRIM(NDids(i))

       END IF

    END DO



    IF (num_NDids == 0) WRITE(*,'(A,I0,A)') 'ERROR=The ' //TRIM(set) // &

         ' nuclear data set has no ' // TRIM(symbols(z)),a,' reactions.'

  END SUBROUTINE get_nuc_data_list



  !---------------------------------------------------------------------

  SUBROUTINE get_nuc_data_info(pcom)

    USE io

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    INTEGER(KIND=4)               :: i,z,a

    CHARACTER(LEN=MAX_NDS_LEN)    :: set

    CHARACTER(LEN=MAX_ND_ID_LEN)  :: NDid

    CHARACTER(LEN=MAX_ND_LEN)     :: prop

    CHARACTER(LEN=LEN(pcom%NOTES)):: NDid_list

    CHARACTER(LEN=LEN(pcom%BODY)) :: prop_list

    LOGICAL(KIND=1)               :: loop = .TRUE.



    prop_list = get_prop()

    NDid_list = get_rids()



    ! Load rate set lists

    CALL read_NDS_list('PUBLIC',pcom)

    CALL read_NDS_list('SHARED',pcom)

    CALL read_NDS_list('USER',pcom)



    DO WHILE (loop)

       loop = next_in_list(NDid,NDid_list,ACHAR(10))



       CALL decode_NDid(NDid,set,z,a)



       ! Load NDid list to get seek position to get rate info

       CALL read_NDid_list(set,pcom,z,a)



       prop = read_ND_info(NDid,pcom,prop_list)

       !print *,'len=',LEN_TRIM(prop)

       WRITE(*,'(2A)',ADVANCE='NO') TRIM(NDid),ACHAR(9)

       CALL print_long_string(TRIM(prop))

    END DO

  END SUBROUTINE get_nuc_data_info



  !---------------------------------------------------------------------

   SUBROUTINE nuc_data_exist(pcom)

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    CHARACTER(LEN=10240)          :: NDid_list

    CHARACTER(LEN=MAX_ND_ID_LEN)  :: NDid

    LOGICAL(KIND=1)               :: loop



    NDid_list = get_rids()

    loop = .TRUE.



    ! Load rate set lists

    CALL read_NDS_list('PUBLIC',pcom)

    CALL read_NDS_list('SHARED',pcom)

    CALL read_NDS_list('USER',pcom)



    DO WHILE (loop)

       ! Loop through NDids

       loop = next_in_list(NDid,NDid_list,ACHAR(10))



       ! Print results

       IF (LEN_TRIM(NDid) > 0) THEN

          IF (NDid_exist(NDid,pcom)) THEN

             WRITE(*,'(3A)') TRIM(NDid),ACHAR(9),'EXIST=YES'

          ELSE

             WRITE(*,'(3A)') TRIM(NDid),ACHAR(9),'EXIST=NO'

          END IF

       END IF

    END DO

  END SUBROUTINE nuc_data_exist



  SUBROUTINE modify_nuc_data_set(pcom)

    USE io

    USE convert

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    CHARACTER(LEN=MAX_NDS_LEN)    :: dest_set,set

    CHARACTER(LEN=MAX_NDS_LEN*(MAX_NDS+1)) :: src_sets

    CHARACTER(LEN=MAX_NDS_GRP_LEN):: dest_group

    CHARACTER(LEN=32750)          :: report,set_info

    INTEGER(KIND=4)               :: counter

    LOGICAL(KIND=4)               :: loop



    dest_set = cina_decode(pcom%BODY)

    dest_group = pcom%REACTION

    src_sets = cina_decode(pcom%NOTES)

    report = ''



    ! Check permissions

    SELECT CASE (dest_group)

    CASE ('USER')

       ! Permission granted for creating destination set

    CASE ('')

       CALL report_error('Missing destination nuclear data set group', &

            'Improper usage',pcom,1)

    CASE DEFAULT

       CALL report_error(TRIM(dest_group) // ' nuclear data sets ' // &

            'may not be modified.','Improper usage',pcom,1)

    END SELECT



    ! Make sure a destination set is not a source setrary

    loop = .TRUE.

    counter = 0

    DO WHILE (loop)

       loop = next_in_list(set,src_sets,ACHAR(9))

       IF (set == dest_set) CALL report_error('The output nuclear ' // &

            'data set "' // TRIM(set) // '" may not also be an input ' // &

            'set.','Improper usage',pcom,1)

       counter = counter + 1

    END DO

    IF (counter > MAX_NDS) CALL report_error('Too many nuclear data ' // &

         'sets were specified.  Contact coordinator@nucastrodata.org ' // &

         'to have this limit increased.','Developer Reminder',pcom,1)

    ! Reset src_sets because next_in_list erased them

    src_sets = cina_decode(pcom%NOTES)



    ! Put list of setraries into set_list

    CALL read_NDS_list('PUBLIC',pcom)

    CALL read_NDS_list('SHARED',pcom)

    CALL read_NDS_list('USER',pcom)



    IF (dest_set /= '') THEN

       ! Remove destination setrary if it exists

       CALL find_NDS_list_index(dest_set,set_index=counter)

       IF (counter > 0) THEN

          CALL rm_NDS(dest_set,pcom,move_to_trash=.TRUE.)

          report = TRIM(report) // 'Moving existing ' // TRIM(dest_set) // &

               ' nuclear data set to trash.  Contact ' // &

               'coordinator@nucastrodata.org to inquire about the ' //&

               'possibility of recovering an erased set.' // ACHAR(8)

       END IF



       ! Create new empty destination set

       CALL add_NDS(dest_set,pcom,dest_group,mkdir=.TRUE.)

       report = TRIM(report) // 'Created new empty ' // TRIM(dest_group) &

            // ' nuclear data set named "' // TRIM(dest_set) // '"' // &

            ACHAR(8)



       ! Create setrary into file just in case merge or copy is not called

!!$       set_info = ''

!!$       CALL set_prop_value('Set Recipe','Empty Set',set_info, &

!!$            RSET_INFO_PROP_SEP,RSET_INFO_VAL_SEP)

!!$       CALL set_prop_value('Creation Date',get_date() // ' ' // &

!!$            get_time(),set_info,RSET_INFO_PROP_SEP,RSET_INFO_VAL_SEP)

!!$       CALL save_NDS_info(dest_set,pcom,set_info)

    END IF



    ! Check if merge or copy should be performed

    IF (src_sets == '') THEN

       ! New empty setrary has already been created, skip merge

    ELSE IF (dest_set /= '' .AND. INDEX(src_sets,ACHAR(9)) == 0) THEN

       ! There is only one src_set so copy it to dest_set and check

       ! for errors if requested

       !CALL copy_set(src_sets,dest_set,pcom,report)

       report = TRIM(report)//'Nuclear data sets may not be copied at this time.  This will be fixed shortly.'//ACHAR(8)

    ELSE IF (dest_set /= '' .OR. pcom%CHK_TEMP .OR. pcom%CHK_OVFL .OR. &

         pcom%CHK_INV) THEN

       !CALL merge_setraries()

!!$       report = TRIM(report)//'Pretending to merge '//TRIM(src_sets)// &

!!$            ' into '// TRIM(dest_set)//ACHAR(8)

       report = TRIM(report)//'Nuclear data sets may not be merged at this time.  This will be fixed shortly.'//ACHAR(8)

    ELSE IF (.NOT. pcom%DEL) THEN

       report = TRIM(report) // 'No nuclear data sets were created, ' // &

            'modified, or merged.' // ACHAR(8)

    END IF



    ! Make sure an isotope list is saved even if merge or copy is not called

    IF (dest_set /= '') CALL save_NDS_iso_list(dest_set,pcom)



    ! Check if source setraries should be deleted

    IF (pcom%DEL .AND. src_sets /= '') THEN

       loop = .TRUE.

       DO WHILE (loop)

          loop = next_in_list(set,src_sets,ACHAR(9))

          CALL rm_NDS(set,pcom,move_to_trash=.TRUE.)

       END DO

       ! Reset src_sets because next_in_list erased them

       src_sets = cina_decode(pcom%NOTES)

       ! Setrary list was saved in rm_rset

       report = TRIM(report) // 'Moving ' // TRIM(src_sets) // &

            ' nuclear data sets to trash.  Contact ' // &

            'coordinator@nucastrodata.org to inquire about the ' // &

            'possibility of recovering an erased data set.' // ACHAR(8)

    ELSE

       ! Save setrary list

       CALL save_NDS_list(dest_group,pcom)

    END IF



    WRITE(*,'(A)') 'MODIFY=SUCCESS'

    ! Remove last linefeed from report

    report = report(:LEN_TRIM(report)-1)



    WRITE(*,'(A)',ADVANCE='NO') 'REPORT='

    CALL print_long_string(TRIM(report))

  END SUBROUTINE modify_nuc_data_set



  SUBROUTINE modify_nuc_data(pcom)

    USE io

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    CHARACTER(LEN=LEN(pcom%BODY)) :: NDid_list

    CHARACTER(LEN=LEN(pcom%REACTION)):: dest_set

    CHARACTER(LEN=MAX_ND_ID_LEN)  :: NDid

    CHARACTER(LEN=MAX_ND_LEN)     :: prop_list,old_set_info

    CHARACTER(LEN=MAX_NDS_LEN)    :: set

    CHARACTER(LEN=32750)          :: report,set_info

    INTEGER(KIND=4)               :: z,a

    LOGICAL(KIND=4)               :: loop_NDid,loop_dest_set



    NDid_list = get_rids()

    prop_list = get_prop()

    dest_set = pcom%REACTION

    report = ''

    set_info = ''



    IF ((NDid_list == '') .AND. (prop_list == '')) THEN

       WRITE(*,'(A)') 'MODIFY=SUCCESS'

       WRITE(*,'(A)') 'REPORT=No nuclear data were modified or added to ' // &

            TRIM(dest_set)

       RETURN

    END IF



    IF (pcom%DEL) THEN

       WRITE(*,'(A)') 'MODIFY=SUCCESS'

       WRITE(*,'(A)') 'REPORT=Erasing nuclear data is under development.'//ACHAR(8)

       RETURN

    END IF



    ! Put list of libraries into lib_list

    CALL read_NDS_list('PUBLIC',pcom)

    CALL read_NDS_list('SHARED',pcom)

    CALL read_NDS_list('USER',pcom)



    ! Loop through all NDids given

    loop_NDid = .TRUE.

    DO WHILE (loop_NDid)

       loop_NDid = next_in_list(NDid,NDid_list,ACHAR(10))



       IF (NDid == '') THEN

          ! A new rate

          NDid = make_NDid(prop_list,pcom)

          old_set_info = prop_list

       ELSE

          CALL decode_NDid(NDid,set,z=z,a=a)



          ! Load NDid list

          CALL read_NDid_list(set,pcom,z,a)



          old_set_info = read_ND_info(NDid,pcom,'')

          CALL update_ND_prop(old_set_info,prop_list)

       END IF



       CALL decode_NDid(NDid,z=z,a=a)



       ! Now save NDid and prop_list to all destination libraries

       IF (dest_set == '') THEN

          loop_dest_set = .FALSE.

       ELSE

          loop_dest_set = .TRUE.

       END IF



       DO WHILE (loop_dest_set)

          loop_dest_set = next_in_list(set,dest_set,ACHAR(9))

          ! Load isotope list

          CALL read_NDS_iso_list(set,pcom)



          CALL open_ND_info(set,pcom,z,a)

          CALL change_ND_info(NDid,pcom,old_set_info)

          CALL save_ND_info(pcom,report,set_info)



          ! Save isotope list

          CALL save_NDS_iso_list(set,pcom)

       END DO



       ! Perform the temp behavior check

       ! Perform the overflow check

       ! Perform the inverse check

    END DO



    ! Add new set info properties for all destination libraries

    dest_set = pcom%REACTION

    IF (dest_set == '') THEN

       loop_dest_set = .FALSE.

    ELSE

       loop_dest_set = .TRUE.

    END IF

    !DO WHILE (loop_dest_set)

    !loop_dest_set = next_in_list(dest_set,dest_set,ACHAR(9))

    !CALL read_NDS_info(dest_set,pcom,old_set_info)

    !CALL save_NDS_info(dest_set,pcom,TRIM(old_set_info) // &

    !TRIM(set_info))

    !END DO



    ! Remove last linefeed from report

    report = report(:LEN_TRIM(report)-1)



    z = LEN_TRIM(report)



    WRITE(*,'(A/A)',ADVANCE='NO') 'MODIFY=SUCCESS','REPORT='

    CALL print_long_string(TRIM(report))

  END SUBROUTINE modify_nuc_data



  !---------------------------------------------------------------------

  SUBROUTINE share_NDS(pcom)

    USE io

    USE fileio

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    CHARACTER(LEN=MAX_NDS_GRP_LEN):: group

    CHARACTER(LEN=200)            :: list,item,path

    CHARACTER(LEN=10240)          :: report

    INTEGER(KIND=4)               :: i

    LOGICAL                       :: loop = .TRUE.



    ! Load rate set lists

    CALL read_NDS_list('PUBLIC',pcom)

    CALL read_NDS_list('SHARED',pcom)

    CALL read_NDS_list('USER',pcom)



    report = ''

    list = cina_decode(pcom%BODY)

    DO WHILE (loop)

       loop = next_in_list(item,list,ACHAR(9))



       ! See if this set name is already used in SHARED

       IF (file_exists(TRIM(CINA_PATH) // 'SHARED/nuc_data/' // &

            TRIM(item) // '/Set_Info')) THEN

          CALL report_error('Nuclear data set ' // TRIM(item) // &

               ' can not be added to SHARED because a set with that ' // &

               'name exists.','Improper usage',pcom,1)

       END IF



       path = get_NDS_path(item,pcom%USER,group)



       IF (group == '') CALL report_error('Nuclear data set "' // &

            TRIM(item) // '" was not found.','Improper usage',pcom,1)



       SELECT CASE (group)

       CASE ('USER')

          ! Remove this set from NDS_buffer but don't put it in the

          ! trash

          CALL rm_NDS(item,pcom,move_to_trash=.FALSE.)



          ! Move set to SHARED

          i = safe_shell('/bin/mv -f ''' // TRIM(path) // ''' ''' // &

               TRIM(CINA_PATH) // 'SHARED/nuc_data''')

          IF (i /= 0) THEN

             WRITE(path,'(A,I0,3A)') 'Error (',i, &

                  ') moving nuclear data set "',TRIM(item),'" to SHARED'

             CALL report_error(TRIM(path),'External program',pcom,1)

          END IF



          ! Add set to SHARED set list

          CALL add_NDS(item,pcom,'SHARED',override=.TRUE.)



          ! Save new SHARED set list

          CALL save_NDS_list('SHARED',pcom)



          report = 'The "' // TRIM(item) // '" nuclear data set has ' // &

               'been moved to the SHARED folder.'

       CASE DEFAULT

          CALL report_error('The ' // TRIM(group) // ' nuclear data ' // &

               'set "' // TRIM(item) // '" may not be moved to the ' // &

               'shared folder','Improper usage',pcom,1)

       END SELECT

    END DO



    WRITE(*,'(A)') 'MODIFY=SUCCESS'

    WRITE(*,'(A)',ADVANCE='NO') 'REPORT='

    CALL print_long_string(TRIM(report))

  END SUBROUTINE share_NDS



  !---------------------------------------------------------------------

  SUBROUTINE parse_nuc_data_file(pcom)

    !PURPOSE = Perform the PARSE NUC DATA FILE action

    !STATUS = Complete and tested

    USE fileio

    USE reactionstrings

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN)    :: pcom

    TYPE(fileiooptions)              :: fopt

    INTEGER(KIND=4)                  :: s = 0,i,n,points

    CHARACTER(LEN=200)               :: dir,tmps

    CHARACTER(LEN=32767)             :: str

    CHARACTER(LEN=LEN(pcom%NOTES))   :: notes

    CHARACTER(LEN=4)                 :: fmt

    REAL(KIND=8)                     :: array(array_len,4),tmpr,tmpr2

    LOGICAL(KIND=4)                  :: e = .FALSE.



    dir = nuc_data_man_temp_path(pcom)



    ! Temp bug fix

    ! dir is too long for inputread to use so remove one character

    dir = dir(1:LEN_TRIM(dir)-2) // '/'



    ! Remove commas from FORMAT to use with inputread

    ! Note: no more than 9 columns can be used

    fmt = pcom%FORMAT(1:1)//pcom%FORMAT(3:3)//pcom%FORMAT(5:5)//pcom%FORMAT(7:7)



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

         fmt//' '//TRIM(cina_decode(pcom%BODY))//' &> '//TRIM(dir)//'redir')

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

          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_MIN=',MINVAL(array(1:points,1))

          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_MAX=',MAXVAL(array(1:points,1))

          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_ERROR_MIN=',MINVAL(array(1:points,2))

          WRITE(*,'(1P,A,E15.7E3)') 'TEMP_ERROR_MAX=',MAXVAL(array(1:points,2))

          WRITE(*,'(1P,A,E15.7E3)') 'RATE_MIN=',MINVAL(array(1:points,3))

          WRITE(*,'(1P,A,E15.7E3)') 'RATE_MAX=',MAXVAL(array(1:points,3))

          WRITE(*,'(1P,A,E15.7E3)') 'RATE_ERROR_MIN=',MINVAL(array(1:points,4))

          WRITE(*,'(1P,A,E15.7E3)') 'RATE_ERROR_MAX=',MAXVAL(array(1:points,4))

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

    END IF

  END SUBROUTINE parse_nuc_data_file



  !---------------------------------------------------------------------

  FUNCTION make_NDid(prop_list,pcom)

    USE reactionstrings

    IMPLICIT NONE

    TYPE(cina_common),INTENT(IN) :: pcom

    CHARACTER(LEN=*),INTENT(IN)   :: prop_list

    CHARACTER(LEN=MAX_ND_ID_LEN)  :: make_NDid,name

    CHARACTER(LEN=100)            :: reac_str,message,unique,data_type

    INTEGER(KIND=4)               :: i,t,z,a

    TYPE(reactionparticles)       :: r



    reac_str = get_prop_value('Reaction String',prop_list, &

         ND_INFO_PROP_SEP,ND_INFO_VAL_SEP)

    CALL read_reac_str(r,reac_str,message)

    IF (message /= '') CALL report_error('Problem interpreting ' // &

         'reaction string "' // TRIM(reac_str) // '" (' // &

         TRIM(message) // ')','Improbable',pcom,1)



    !Find reactant with largest z (if z is same, largest a)

    t = getreac_num(r)

    z = getreac_z(r,1)

    a = getreac_a(r,1)

    DO i = 1, t

       IF (getreac_z(r,i) > z) THEN

          z = getreac_z(r,i)

          a = getreac_a(r,i)

       ELSE IF (getreac_z(r,i) == z) THEN

          ! Check if a is greater

          IF (getreac_a(r,i) > a) THEN

             z = getreac_z(r,i)

             a = getreac_a(r,i)

          END IF

       END IF

    END DO



    t = getreactype(r)



    unique = ''

    ! Check for decay or special reaction

    ! Add separator to beginning and end to simplify searches

    message = ',' // TRIM(get_prop_value('Reaction Type',prop_list, &

         ND_INFO_PROP_SEP,ND_INFO_VAL_SEP)) // ','



    IF (INDEX(message,',ec,') > 0) unique = 'ec'

    IF (INDEX(message,',bet+,') > 0) unique = 'bet+'

    IF (INDEX(message,',bet-,') > 0) unique = 'bet-'



    name = get_prop_value('Nuc Data Name',prop_list, &

         ND_INFO_PROP_SEP,ND_INFO_VAL_SEP)



    IF (name == '') CALL report_error('This nuclear data must have ' // &

         'a name.','Improper usage',pcom,1)



    ! Make sure name does not have the value separator (=) because

    ! this is used to differentiate between IDs and properties

    IF (INDEX(name,ND_INFO_VAL_SEP) > 0) CALL report_error( &

         'The name of this nuclear data may not contain "' // &

         ND_INFO_VAL_SEP // '"','Improper usage',pcom,1)



    data_type = get_prop_value('Nuc Data Type',prop_list, &

         ND_INFO_PROP_SEP,ND_INFO_VAL_SEP)



    ! Give caution if data_type is not CS(E) or S(E)



    WRITE(make_NDid,'(I2.2,2I3.3,9A)') t,z,a,' ',ACHAR(9),TRIM(reac_str), &

         ACHAR(11),TRIM(unique),ACHAR(9),TRIM(data_type),ACHAR(11), &

         TRIM(name)



  END FUNCTION make_NDid





  !---------------------------------------------------------------------



END MODULE nuc_data_man

