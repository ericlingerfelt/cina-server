MODULE rate_man
  USE IFLPORT
  USE cina_core
  USE rate_man_core
  USE convert
  USE constants

  ! By default all procedures and global variables are public
  PUBLIC

  TYPE,PUBLIC                      :: sunet_list
     INTEGER(KIND=2)               :: iso_list(0:MAX_Z,MAX_ISO) = -1
     LOGICAL(KIND=4)               :: al26 = .FALSE.
     LOGICAL(KIND=4)               :: al26star = .FALSE.
     LOGICAL(KIND=4)               :: al26dash = .FALSE.
  END TYPE sunet_list

CONTAINS
!***

  !---------------------------------------------------------------------
  !****if* rate_man/rate_man_ver
  ! NAME
  !  FUNCTION rate_man_ver()
  ! PURPOSE
  !  Return the cvs revision number for this file
  ! STATUS
  !  Complete
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start function
  ! SOURCE

  FUNCTION rate_man_ver()
    IMPLICIT NONE
    CHARACTER(LEN=10)                :: rate_man_ver
    CHARACTER(LEN=20),PARAMETER      :: RATE_MAN_B_VER = '$Revision: 1.24 $'
    rate_man_ver = RATE_MAN_B_VER(12:LEN_TRIM(RATE_MAN_B_VER)-2)

  END FUNCTION rate_man_ver
  !***

  !****is* rate_man/get_rlib_list
  ! NAME
  !  SUBROUTINE get_rlib_list(pcom)
  ! PURPOSE
  !  Perform the GET RATE LIBRARY LIST action
  !
  !  This subroutine reads in lists of rate libraries for multiple library
  !  groups from a file and prints them to standard output.
  ! STATUS
  !  Complete
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  !---------------------------------------------------------------------
  SUBROUTINE get_rlib_list(pcom)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=100)               :: list,item
    CHARACTER(LEN=400000)            :: lib_list,error_str
    INTEGER(KIND=4)                  :: status,tab_index
    LOGICAL                          :: loop

    list = cina_decode(pcom%BODY)
    loop = .TRUE.
    DO WHILE (loop)
       loop = next_in_list(item,list,ACHAR(9))
       CALL read_rlib_list(item,pcom,lib_list)
       WRITE(*,'(3A)') TRIM(item),'=',TRIM(lib_list)
    END DO
  END SUBROUTINE get_rlib_list
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/get_rlib_info
  ! NAME
  !  SUBROUTINE get_rlib_info(pcom)
  ! PURPOSE
  !  Perform the GET RATE LIBRARY INFO action
  !
  !  This subroutine reads in the lists of properties for multiple libraries
  !  from a file and prints them to standard output.
  ! STATUS
  !  Complete
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE get_rlib_info(pcom)
    USE io
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=MAX_RLIB_LEN)   :: list,item
    CHARACTER(LEN=MAX_RATE_LEN)   :: lib_info
    INTEGER(KIND=4)               :: status
    LOGICAL                       :: loop

    ! Load rate library lists
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    list = cina_decode(pcom%BODY)
    loop = .TRUE.
    DO WHILE (loop)
       loop = next_in_list(item,list,ACHAR(9))
       CALL read_rlib_info(item,pcom,lib_info)

       WRITE(*,'(2A)',ADVANCE='NO') TRIM(item),'='
       CALL print_long_string(TRIM(lib_info))
    END DO
  END SUBROUTINE get_rlib_info
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/get_rlib_isotopes
  ! NAME
  !  SUBROUTINE get_rlib_isotopes(pcom)
  ! PURPOSE
  !  Perform the GET RATE LIBRARY ISOTOPES action
  !
  !  This subroutine reads in lists of isotopes for multiple libraries from
  !  a file and prints them to standard output.
  ! STATUS
  !  Complete
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE get_rlib_isotopes(pcom)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=2000)           :: list,item,error_str
    CHARACTER(LEN=MAX_ISO*8)      :: z_list
    INTEGER(KIND=4)               :: status,z,m
    INTEGER(KIND=2)               :: iso_list(0:MAX_Z,MAX_ISO) = -1
    LOGICAL                       :: loop,first

    ! Load rate library lists
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    list = cina_decode(pcom%BODY)
    loop = .TRUE.
    DO WHILE (loop)
       loop = next_in_list(item,list,ACHAR(9))
       CALL read_rlib_iso_list(item,pcom,iso_list)

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
          IF (first .AND. (z_list /= '')) THEN
             z_list = z_list(2:)
             first = .FALSE.
          END IF
          WRITE(*,'(A)',ADVANCE='NO') TRIM(z_list)
       END DO

       WRITE(*,'(A)') ''      ! Print newline
    END DO
  END SUBROUTINE get_rlib_isotopes
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/get_rate_list
  ! NAME
  !  SUBROUTINE get_rate_list(pcom)
  ! PURPOSE
  !  Perform the GET RATE LIST action
  !
  !  This subroutine reads in lists of rate IDs for an isotope from a file
  !  and prints them to standard output.
  ! STATUS
  !  Complete
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE get_rate_list(pcom)
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=MAX_RLIB_LEN)   :: library
    CHARACTER(LEN=40)             :: isotope_list
    CHARACTER(LEN=40)             :: rtypes
    CHARACTER(LEN=4)              :: t
    INTEGER(KIND=4)               :: z,a,i,num_rids
    CHARACTER(LEN=MAX_RID_LEN)    :: rids(MAX_RATES)

    ! Load rate library lists
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    library = cina_decode(pcom%BODY)
    isotope_list = cina_decode(pcom%NOTES)

    ! Add commas to ease the searching
    rtypes = ','//TRIM(pcom%REACTION)//','

    ! Extract z and a from isotope_list
    i = INDEX(isotope_list,',')
    IF (i < 2) CALL report_error('Isotope is in incorrect format.', &
         'Improper usage',pcom,1)
    READ(isotope_list(:i-1),'(I)') z
    READ(isotope_list(i+1:),'(I)') a

    ! Get rate list
    CALL read_rid_list(library,pcom,z,a,rids,num_rids)

    ! Print out the rate ids if they match one of the rtypes
    DO i = 1, num_rids
       ! Get type for this rate
       t = ',' // rids(i)(1:2) // ','
       IF ((INDEX(rtypes,t) > 0) .OR. (rtypes == ',, ')) THEN
          WRITE(*,'(A)') TRIM(rids(i))
       END IF
    END DO

    IF (num_rids == 0) WRITE(*,'(A,I0,A)') 'ERROR=The ' //TRIM(library) // &
         ' library has no ' // TRIM(symbols(z)),a,' rates.'
  END SUBROUTINE get_rate_list
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/get_rate_info
  ! NAME
  !  SUBROUTINE get_rate_info(pcom)
  ! PURPOSE
  !  Perform the GET RATE INFO action
  !
  !  This subroutine reads in rate properties for multiple rate IDs from a
  !  file and prints them to standard output.
  ! STATUS
  !  Complete
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE get_rate_info(pcom)
    USE io
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    INTEGER(KIND=4)               :: i,z,a,n
    CHARACTER(LEN=MAX_RLIB_LEN)   :: library
    CHARACTER(LEN=MAX_RID_LEN)    :: rid
    CHARACTER(LEN=MAX_RATE_LEN)   :: prop
    CHARACTER(LEN=LEN(pcom%NOTES)):: rid_list
    CHARACTER(LEN=LEN(pcom%BODY)) :: prop_list
    LOGICAL(KIND=1)               :: loop
    LOGICAL(KIND=4)               :: success

    prop_list = get_prop()
    rid_list = get_rids()

    ! Load rate library lists
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    loop = .TRUE.
    n = 0
    DO WHILE (loop)
       loop = next_in_list(rid,rid_list,ACHAR(10))

       CALL decode_rid(rid,library,z,a,success=success)
       IF (success) THEN
          ! Load rid list to get seek position to get rate info
          CALL read_rid_list(library,pcom,z,a)

          prop = read_rate_info(rid,pcom,prop_list)
          WRITE(*,'(2A)',ADVANCE='NO') TRIM(rid),ACHAR(9)
          CALL print_long_string(TRIM(prop))
          n = n + 1
       ELSE
          WRITE(prop,'(A,I0,A)') 'Over ',n-1,' rates were selected.  Please select fewer rates.'
          CALL report_error(prop,'Improper usage',pcom,1)
!!$          WRITE(prop,'(A,I0,A)') 'Only the first ',n,' rates will be displayed.  ' // &
!!$               'This may happen if too many rates are selected or if an error occurred.'
!!$          CALL report_error(prop,'Warning',pcom,0)
          RETURN
       END IF
    END DO
  END SUBROUTINE get_rate_info
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/rates_exist
  ! NAME
  !  SUBROUTINE rates_exist(pcom)
  ! PURPOSE
  !  Perform the RATES EXIST action
  !
  !  This subroutine checks if rate IDs exist in a library and returns a
  !  YES or NO for each rate ID.
  ! STATUS
  !  Complete
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

   SUBROUTINE rates_exist(pcom)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=10240)          :: rid_list
    CHARACTER(LEN=MAX_RID_LEN)    :: rid
    LOGICAL(KIND=1)               :: loop

    rid_list = get_rids()
    loop = .TRUE.
    DO WHILE (loop)
       ! Loop through rids
       loop = next_in_list(rid,rid_list,ACHAR(10))

       ! Print results
       IF (rid /= '') THEN
          IF (rid_exist(rid,pcom)) THEN
             WRITE(*,'(3A)') TRIM(rid),ACHAR(9),'EXIST=YES'
          ELSE
             WRITE(*,'(3A)') TRIM(rid),ACHAR(9),'EXIST=NO'
          END IF
       END IF
    END DO
  END SUBROUTINE rates_exist
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/modify_rates
  ! NAME
  !  SUBROUTINE modify_rates(pcom)
  ! PURPOSE
  !  Perform the MODIFY RATES action
  !
  !  This subroutine changes properties, performs error checking, and makes
  !  inverses for individual rates.
  ! STATUS
  !  In development
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE modify_rates(pcom)
    USE io
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=LEN(pcom%BODY)) :: rid_list
    CHARACTER(LEN=LEN(pcom%REACTION)):: dest_libs
    CHARACTER(LEN=MAX_RID_LEN)    :: rid
    CHARACTER(LEN=MAX_RATE_LEN)   :: prop_list,props
    CHARACTER(LEN=MAX_RLIB_LEN)   :: dest_lib,lib
    CHARACTER(LEN=32750)          :: report,lib_info
    CHARACTER(LEN=100)            :: reac_str,tmps
    REAL(KIND=8)                  :: parms(MAX_A)
    INTEGER(KIND=4)               :: z,a,parm_num
    LOGICAL(KIND=4)               :: loop_rid,loop_dest_lib,chk

    rid_list = get_rids()
    prop_list = get_prop()
    dest_libs = pcom%REACTION
    report = ''

    IF ((rid_list == '') .AND. (prop_list == '')) THEN
       WRITE(*,'(A)') 'MODIFY=SUCCESS'
       WRITE(*,'(A)') 'REPORT=No rates were modified or added to ' // &
            TRIM(dest_libs)
       RETURN
    END IF

    ! Put list of libraries into lib_list
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    chk = pcom%CHK_TEMP .OR. pcom%CHK_OVFL .OR. pcom%CHK_INV

    IF (pcom%CHK_TEMP) report = TRIM(report) // 'Checking for rate ' // &
         'temperature problems.' // ACHAR(8)
    IF (pcom%CHK_OVFL) report = TRIM(report) // 'Checking for rate ' // &
         'overflow problems.' // ACHAR(8)
    IF (pcom%CHK_INV) report = TRIM(report) // 'Checking for inverse ' // &
         'rates.' // ACHAR(8) // 'CAUTION: Decays are not checked for ' // &
         'inverse rates.' // ACHAR(8)

    ! Load all isotope lists
    IF (dest_libs /= '') THEN
       loop_dest_lib = .TRUE.
       DO WHILE (loop_dest_lib)
          loop_dest_lib = next_in_list(dest_lib,dest_libs,ACHAR(9))
          CALL read_rlib_iso_list(dest_lib,pcom)
       END DO
    END IF
    ! Retrieve dest_libs erased by next_in_list
    dest_libs = pcom%REACTION

    ! Loop through all rids given
    loop_rid = .TRUE.
    DO WHILE (loop_rid)
       loop_rid = next_in_list(rid,rid_list,ACHAR(10))

       IF (rid == '') THEN
          ! A new rate
          rid = make_rid(prop_list,pcom)
          props = prop_list
          CALL decode_rid(rid,z=z,a=a,unique_reac_str=reac_str)
       ELSE
          CALL decode_rid(rid,lib,z=z,a=a)
          CALL read_rlib_iso_list(lib,pcom)
          IF (isotope_exist(lib,z,a)) THEN
             ! Load rid list
             CALL read_rid_list(lib,pcom,z,a)

             IF (rid_exist(rid,pcom)) THEN
                props = read_rate_info(rid,pcom,'')
                CALL update_rate_prop(props,prop_list)
             ELSE
                CALL report_error(TRIM(lib) // &
                     ' does not contain ' // TRIM(reac_str), &
                     'Improper usage',pcom,1)
             END IF
          ELSE
             CALL report_error(TRIM(lib) // &
                  ' does not contain isotope for ' // TRIM(reac_str), &
                  'Improper usage',pcom,1)
          END IF
       END IF

       ! Now save rid and prop_list to all destination libraries
       IF (dest_libs /= '') THEN
          loop_dest_lib = .TRUE.
          DO WHILE (loop_dest_lib)
             loop_dest_lib = next_in_list(dest_lib,dest_libs,ACHAR(9))

             CALL read_rlib_info(dest_lib,pcom,lib_info)
             IF (isotope_exist(dest_lib,z,a)) &
                  CALL read_rid_list(dest_lib,pcom,z,a)
             CALL open_rate_info(dest_lib,pcom,z,a)
             CALL change_rate_info(rid,pcom,props)
             IF (pcom%MK_INV) THEN
                CALL save_rate_info_inv(pcom,report,lib_info)
             ELSE
                CALL save_rate_info(pcom,report,lib_info)
             END IF
             CALL save_rlib_info(dest_lib,pcom,lib_info)
          END DO
       END IF
       ! Retrieve dest_libs erased by next_in_list
       dest_libs = pcom%REACTION

       IF (chk) THEN
          ! Get parameter array from properties
          CALL get_parm_array(props,parms,parm_num)
          CALL decode_rid(rid,lib,unique_reac_str=reac_str)
          IF (lib == '') lib = 'new rate'

          IF (parm_num < 1) THEN
             WRITE(tmps,'(A,I0,A)') 'Problem interpreting parameter ', &
                  -parm_num,' for "' // TRIM(reac_str) // '" in ' // &
                  TRIM(lib)
             report = TRIM(report) // TRIM(tmps) // ACHAR(8)
          ELSE IF (MOD(parm_num,7) /= 0) THEN
             WRITE(tmps,'(A,I0,A)') 'Number of parameters (',parm_num, &
                  ' is not divisible by 7 for "' // TRIM(reac_str) // &
                  '" in ' // TRIM(lib)
             report = TRIM(report) // TRIM(tmps) // ACHAR(8)
          END IF

          IF (pcom%CHK_TEMP) &
               CALL check_temp_behavior(parms,parm_num,report)
          IF (pcom%CHK_OVFL) &
               CALL check_overflow(parms,parm_num,report)
          IF (pcom%CHK_INV) &
               CALL check_for_inverse(rid,props,lib,pcom,report)
       END IF
    END DO

    ! Save all isotope lists and library info for all destination libraries
    IF (dest_libs /= '') THEN
       loop_dest_lib = .TRUE.
       DO WHILE (loop_dest_lib)
          loop_dest_lib = next_in_list(dest_lib,dest_libs,ACHAR(9))
          CALL save_rlib_iso_list(dest_lib,pcom)
       END DO
    END IF

    ! Remove last linefeed from report
    report = report(:LEN_TRIM(report)-1)

    WRITE(*,'(A/A)',ADVANCE='NO') 'MODIFY=SUCCESS','REPORT='
    CALL print_long_string(TRIM(report))
  END SUBROUTINE modify_rates
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/modify_library
  ! NAME
  !  SUBROUTINE modify_library(pcom)
  ! PURPOSE
  !  Perform the MODIFY LIBRARY action
  !
  !  This subroutine enables a list of source rate libraries to be merged
  !  into one destination library, checked for errors, and deleted.
  ! STATUS
  !  Complete
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE modify_library(pcom)
    USE io
    USE convert
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=MAX_RLIB_LEN)   :: dest_lib,lib
    CHARACTER(LEN=MAX_RLIB_LEN*(MAX_RLIBS+1)) :: src_libs
    CHARACTER(LEN=MAX_RGRP_LEN)   :: dest_group
    CHARACTER(LEN=32750)          :: report,lib_info
    CHARACTER(LEN=3)              :: all_inverses
    INTEGER(KIND=4)               :: i
    LOGICAL(KIND=4)               :: loop,chk

    dest_lib = cina_decode(pcom%BODY)
    dest_group = pcom%REACTION
    src_libs = cina_decode(pcom%NOTES)
    report = ''

    chk = pcom%CHK_TEMP .OR. pcom%CHK_OVFL .OR. pcom%CHK_INV

    ! Check permissions
    SELECT CASE (dest_group)
    CASE ('USER')
       ! Permission granted for creating destination library
    CASE DEFAULT
       CALL report_error(TRIM(dest_group) // ' libraries may not be ' // &
            'modified.','Improper usage',pcom,1)
    END SELECT

    ! Put list of libraries into lib_list
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    IF (dest_lib /= '') THEN
       ! Set All Inverses Present lib_info property for destlib
       IF (src_libs == '') THEN
          loop = .FALSE.
       ELSE
          loop = .TRUE.
          report = src_libs
       END IF
       chk = .TRUE.
       DO WHILE (loop)
          loop = next_in_list(lib,report,ACHAR(9))
          CALL read_rlib_info(lib,pcom,lib_info)
          all_inverses = get_prop_value('All Inverses Present',lib_info, &
               RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
          IF (all_inverses /= 'YES') THEN
             chk = .FALSE.
             loop = .FALSE.
          END IF
       END DO
       all_inverses = 'NO'
       IF (chk) all_inverses = 'YES'
       report = ''

       ! Remove destination library if it exists
       CALL find_rlib_list_index(dest_lib,lib_index=i)
       IF (i > 0) THEN
          CALL rm_rlib(dest_lib,pcom,move_to_trash=.TRUE.)
          report = TRIM(report) // 'Moving existing ' // TRIM(dest_lib) // &
               ' library to trash.  Contact coordinator@nucastrodata.org ' &
               // 'to inquire about the possibility of recovering an ' // &
               'erased library.' // ACHAR(8)
       END IF

       ! Create new empty destination library
       CALL add_rlib(dest_lib,pcom,dest_group,mkdir=.TRUE.)
       report = TRIM(report) // 'Created new empty ' // TRIM(dest_group) &
            // ' library named "' // TRIM(dest_lib) // '"' // ACHAR(8)

       ! Create library into file just in case merge or copy is not called
       lib_info = ''
       CALL set_prop_value('All Inverses Present',all_inverses,lib_info, &
            RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
       CALL set_prop_value('Library Recipe','Empty Library',lib_info, &
            RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
       CALL set_prop_value('Creation Date',get_date() // ' ' // &
            get_time(),lib_info,RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
       CALL save_rlib_info(dest_lib,pcom,lib_info)
    END IF

    ! Check if merge or copy should be performed
    IF (src_libs == '') THEN
       ! New empty library has already been created, skip merge
    ELSE IF (INDEX(src_libs,ACHAR(9)) == 0 .AND. &
         .NOT. (pcom%DEL .AND. dest_lib == '' .AND. .NOT. chk)) THEN
       ! Run if one source library unless deleting with no destination
       ! libraries or no error checking

       ! There is only 0 or 1 libraries in src_lib so copy it to dest_lib
       ! and check for errors if requested
       CALL copy_rlib(src_libs,dest_lib,pcom,report)
    ELSE IF (dest_lib /= '' .OR. chk) THEN
       CALL merge_rlib(src_libs,dest_lib,pcom,report)
    ELSE IF (.NOT. pcom%DEL) THEN
       report = TRIM(report) // 'No libraries were created, modified, ' // &
            'or merged.' // ACHAR(8)
    END IF

    ! Make sure an isotope list is saved even if merge or copy is not called
    IF (dest_lib /= '') CALL save_rlib_iso_list(dest_lib,pcom)

    ! Check if source libraries should be deleted
    IF (pcom%DEL .AND. src_libs /= '') THEN
       loop = .TRUE.
       DO WHILE (loop)
          loop = next_in_list(lib,src_libs,ACHAR(9))
          CALL rm_rlib(lib,pcom,move_to_trash=.TRUE.)
       END DO
       ! Reset src_libs because next_in_list erased them
       src_libs = cina_decode(pcom%NOTES)
       ! Library list was saved in rm_rlib
       report = TRIM(report) // 'Moving "' // TRIM(src_libs) // &
            '" to trash.  Contact coordinator@nucastrodata.org ' &
            // 'to inquire about the possibility of recovering an ' // &
            'erased library.' // ACHAR(8)
    ELSE IF (dest_lib /= '') THEN
       ! Save library list
       ! CALL save_rlib_list(dest_group,pcom)
    END IF

    WRITE(*,'(A)') 'MODIFY=SUCCESS'
    ! Remove last linefeed from report
    report = report(:LEN_TRIM(report)-1)

    WRITE(*,'(A)',ADVANCE='NO') 'REPORT='
    CALL print_long_string(TRIM(report))
  END SUBROUTINE modify_library
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/add_missing_inv
  ! NAME
  !  SUBROUTINE add_missing_inv(pcom)
  ! PURPOSE
  !  Perform the ADD MISSING INV RATES action
  !
  !  This subroutine adds any missing inverse rates for a list of rate
  !  libraries.
  ! STATUS
  !  In development
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE add_missing_inv(pcom)
    USE io
    USE convert
    USE reactionstrings
    USE inv_parm
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=MAX_RLIB_LEN)   :: lib,linklib
    CHARACTER(LEN=MAX_RLIB_LEN*(MAX_RLIBS+1)) :: libs
    CHARACTER(LEN=MAX_RID_LEN)    :: rids(MAX_RATES),invrid
    CHARACTER(LEN=MAX_RGRP_LEN)   :: lib_group
    CHARACTER(LEN=MAX_RATE_LEN)   :: props,inv_props,value
    TYPE(reactionparticles)       :: r,r_inv
    INTEGER(KIND=2)               :: iso_list(0:MAX_Z,MAX_ISO)
    CHARACTER(LEN=32750)          :: report,lib_info
    CHARACTER(LEN=200)            :: tmps,tmps2
    INTEGER(KIND=4)               :: i,j,k,z,a,index_to_a,num_rids,parm_num
    LOGICAL(KIND=4)               :: loop,link,found_inverse,saved,force_save
    LOGICAL(KIND=4)               :: isinverse,isinverse2,all_inv_present,tmpl
    REAL(KIND=8)                  :: qvalue,inv_qvalue,c_qvalue,c_inv_qvalue
    REAL(KIND=8)                  :: parm(MAX_A),parm_inv(MAX_A)
    REAL(KIND=8)                  :: parm_inv_calc(MAX_A),del_a(MAX_A)

    libs = cina_decode(pcom%BODY)
    report = ''

    ! Put list of libraries into lib_list
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    loop = .TRUE.
    DO WHILE (loop)
       loop = next_in_list(lib,libs,ACHAR(9))
       CALL find_rlib_list_index(lib,group_name=lib_group,lib_index=i)
       IF (i == -1) CALL report_error('Rate library "' // TRIM(lib) // &
            '" was not found.','Improper usage',pcom,1)
       IF (lib_group /= 'USER') THEN
          report = TRIM(report) // 'Can not add missing inverse rates to ' // &
               TRIM(lib_group) // ' library "' // TRIM(lib) // '"' // ACHAR(8)
       ELSE
          report = TRIM(report) // 'Adding missing inverse rates to ' // &
               TRIM(lib_group) // ' library "' // TRIM(lib) // '"' // ACHAR(8)
          CALL read_rlib_iso_list(lib,pcom,iso_list)
          CALL read_rlib_info(lib,pcom,lib_info)
          all_inv_present = .TRUE.

          ! Loop through isotopes in library
          DO z = 0, MAX_Z
             index_to_a = 1
             a = iso_list(z,index_to_a)

             DO WHILE (a > 0)
                !print *,z,a
                CALL read_rid_list(lib,pcom,z,a,rids,num_rids)

                ! Loop through rates in isotope
                DO i = 1, num_rids
                   !print *,'rid=',TRIM(rids(i))

                   saved = .FALSE.
                   force_save = .FALSE.

                   ! Determine if rid is a link
                   invrid = rids(i)
                   CALL correct_rid(invrid,lib,link)

                   IF (link) THEN
                      CALL decode_rid(rids(i),linklib)
                      CALL read_rid_list(linklib,pcom,z,a)
                   END IF
                   props = read_rate_info(rids(i),pcom,'')

                   ! Is this a forward rate or inverse rate
                   value = get_prop_value('Reaction Type',props, &
                        RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
                   IF (INDEX(','//TRIM(value)//',', ',v,') > 0) THEN
                      isinverse = .TRUE.
                   ELSE
                      isinverse = .FALSE.
                   END IF

                   tmps = ADJUSTL(get_prop_value('Q-value',props, &
                        RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP))
                   READ(tmps,*,IOSTAT=j) qvalue
                   IF (j /= 0) qvalue = 0.0

                   ! Get forward parameters
                   CALL get_parm_array(props, parm, parm_num)
                   IF (parm_num < 1) THEN
                      CALL decode_rid(rids(i),tmps2,unique_reac_str=tmps)
                      CALL report_error('Unable to read parameters for ' // &
                           TRIM(tmps) // ' from ' // TRIM(tmps2), 'Improbable',pcom,1)
                   END IF

                   ! Make inverse rid
                   CALL inv_rid(pcom,invrid,rids(i),rid_r=r,inv_r=r_inv)
                   !print *,'   made inverse rid'

                   c_qvalue = get_qvalue(r)
                   IF (ABS((c_qvalue - qvalue) / c_qvalue) > 0.1) THEN
                      IF (link) THEN
                         CALL decode_rid(rids(i),tmps2,unique_reac_str=tmps)
                         report = TRIM(report) // 'Can''t correct q value for ' // &
                              TRIM(tmps) // ' in ' // TRIM(tmps2) // ACHAR(8)
                      ELSE
                         CALL decode_rid(rids(i),unique_reac_str=tmps2)
                         WRITE(tmps,'(3A,1P,G14.7,A,G14.7)') 'Changing q value for ',TRIM(tmps2), &
                              ' from ',qvalue,' to calculated value ',c_qvalue
                            !print '(A,3G16.9)',TRIM(tmps2),qvalue,c_qvalue,(c_qvalue - qvalue)/c_qvalue
                         qvalue = c_qvalue
                         report = TRIM(report) // TRIM(tmps) // ACHAR(8)
                         WRITE(tmps,'(1P,G14.7)') qvalue
                         CALL set_prop_value('Q-value',TRIM(tmps),props, &
                              RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
                         force_save = .TRUE.
                      END IF
                   END IF

                   IF (decay(r)) THEN
                      isinverse2 = .NOT. isinverse
                      inv_props = ''
                   ELSE
                      found_inverse = rid_exist(invrid,pcom,lib,inv_props)
                      ! Is this a forward rate or inverse rate
                      value = get_prop_value('Reaction Type',inv_props, &
                           RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
                      IF (INDEX(','//TRIM(value)//',', ',v,') > 0) THEN
                         isinverse2 = .TRUE.
                      ELSE
                         isinverse2 = .FALSE.
                      END IF
                   END IF

                   !print *,'   got all input'
                   IF (found_inverse) THEN
                      IF (isinverse == isinverse2) THEN
                         ! Problem if both are marked as forward or inverse
                         ! Let forward one have positive Q-value

                         ! If forward rate is wrong, resave to fix inverse flag
                         IF (qvalue < 0.0 .AND. .NOT. isinverse) THEN
                            ! Resave forward as inverse
                            CALL decode_rid(rids(i),tmps2,unique_reac_str=tmps)

                            IF (link) THEN
                               report = TRIM(report) // &
                                    'Can''t fix inverse flag for ' // &
                                    TRIM(tmps) // ' from ' // TRIM(tmps2) // ACHAR(8)
                            ELSE
                               report = TRIM(report) // &
                                    'Correcting wrong inverse flag, marking ' // &
                                    TRIM(tmps) // ' as inverse rate' // ACHAR(8)
                               isinverse = .TRUE.
                            END IF
                         ELSE IF (qvalue > 0.0 .AND. isinverse) THEN
                            ! Resave inverse as forward
                            CALL decode_rid(rids(i),tmps2,unique_reac_str=tmps)

                            IF (link) THEN
                               report = TRIM(report) // &
                                    'Can''t fix inverse flag for ' // &
                                    TRIM(tmps) // ' from ' // TRIM(tmps2) // ACHAR(8)
                            ELSE
                               report = TRIM(report) // &
                                    'Correcting wrong inverse flag, marking ' // &
                                    TRIM(tmps) // ' as forward rate' // ACHAR(8)
                               isinverse = .FALSE.
                            END IF
                         END IF
                      END IF
                      !print *,'   checked for wrong inverse flag'
                      ! Get inverse parameters
!!$                      CALL get_parm_array(inv_props, parm_inv, k)
!!$                      IF (k < 1) THEN
!!$                         CALL decode_rid(invrid,tmps2,unique_reac_str=tmps)
!!$                         CALL report_error('Unable to read parameters for ' // &
!!$                              TRIM(tmps) // ' from ' // TRIM(tmps2), 'Improbable',pcom,1)
!!$                      END IF
!!$
!!$                      !print *,'got inverse parm array'
!!$                       ! Get inverse parameters
!!$                      CALL get_inv_parm(parm,parm_num,parm_inv_calc,r,del_a,j,tmps)
!!$                      IF (j /= 0) THEN
!!$                         ! Can't get inverse parameters for some reason
!!$
!!$                         IF (k > parm_num) THEN
!!$                            CALL decode_rid(rids(i),unique_reac_str=tmps2)
!!$                            report = TRIM(report) // &
!!$                                 'ERROR! Can''t fix missing parameters for ' // &
!!$                                 TRIM(tmps2) // ' reason: ' // TRIM(tmps) // ACHAR(8)
!!$                         END IF
!!$                      ELSE
!!$                         ! Check that parm_inv_calc matches parm_inv
!!$                         tmpl = .FALSE.  ! becomes TRUE when inverse is wrong
!!$                         IF (k /= parm_num .AND. isinverse) THEN
!!$                            WRITE(tmps2,'(I0)') parm_num
!!$                            CALL set_prop_value('Number of Parameters',tmps2, &
!!$                                 inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
!!$                            tmpl = .TRUE.
!!$                         END IF
!!$
!!$                         ! warn if inv parameters differ
!!$                         DO j = 1, parm_num
!!$                            IF (ABS((parm_inv_calc(j) - parm_inv(j)) / &
!!$                                 parm_inv_calc(j)) > 0.1) tmpl = .TRUE.
!!$                         END DO
!!$
!!$                         IF (tmpl) THEN
!!$                            ! replace inv parameters
!!$                            IF (link) THEN
!!$                               CALL decode_rid(rids(i),tmps2,unique_reac_str=tmps)
!!$                               report = TRIM(report) // &
!!$                                    'Can''t correct inverse parameters for ' // &
!!$                                    TRIM(tmps) // ' in ' // TRIM(tmps2) // ACHAR(8)
!!$                            ELSE
!!$                               WRITE(tmps2,'(I0)') parm_num - 1
!!$                               WRITE(tmps,'(1P,'//tmps2//'(E12.5,'','')'//',E12.5)') &
!!$                                    parm_inv_calc(:parm_num)
!!$                               CALL set_prop_value('Parameters',tmps, &
!!$                                    inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
!!$
!!$                               !saved = .FALSE.
!!$                               !force_save = .TRUE.
!!$                               CALL decode_rid(invrid,unique_reac_str=tmps)
!!$                               report = TRIM(report) // &
!!$                                    'Correcting inverse parameters for ' // &
!!$                                    TRIM(tmps) // ACHAR(8)
!!$                            END IF
!!$                         END IF
!!$                      END IF

                   ELSE
                      !print *,'   no inverse found'
                      ! inverse not found
                      IF (link) THEN
                         CALL decode_rid(rids(i),tmps2,unique_reac_str=tmps)
                         !all_inv_present = .FALSE.
                         report = TRIM(report) // 'Can''t add inverse for ' // &
                              TRIM(tmps) // ' from ' // TRIM(tmps2) // ACHAR(8)
                      ELSE IF (isinverse) THEN
                         CALL decode_rid(rids(i),unique_reac_str=tmps)
                         !!!!Commented out by EJL on 11/30/10
                         !all_inv_present = .FALSE.
                         report = TRIM(report) // 'Leaving inverse with no forward rate ' &
                              // TRIM(tmps) // ACHAR(8)
                      ELSE
                         !print *,'   making inverse for ',TRIM(invrid)
                         inv_props = get_inv_props(pcom,props,r,r_inv,rids(i),invrid,tmps)
                         IF (tmps /= '') THEN
                            report = TRIM(report) // TRIM(tmps) // ACHAR(8)
                            !all_inv_present = .FALSE.
                         ELSE
                            !print *,'forward: '//TRIM(rids(i))//' = '//TRIM(props)
                            !print *,''
                            !print *,'inverse: '//TRIM(invrid)//' = '//TRIM(inv_props)

                            !CALL decode_rid(invrid,unique_reac_str=tmps)
                            !report = TRIM(report) // 'Adding ' // TRIM(tmps) // ACHAR(8)

                            ! Remove library name from inverse rate
                            invrid = change_rid(invrid,library='')

                            CALL save_rate_info_no_buf(pcom,report,lib_info,lib,invrid, &
                                 inv_props,inv_rates=2)
                         END IF
                      END IF
                   END IF

                   IF (force_save .AND. .NOT. saved .AND. .NOT. link) THEN
                      !print *,'   saving changes'
                      CALL open_rate_info(lib,pcom,z,a)
                      CALL change_rate_info(rids(i),pcom,props)
                      IF (isinverse) THEN
                         CALL save_rate_info(pcom,report,lib_info,inv_rates=2)
                      ELSE
                         CALL save_rate_info(pcom,report,lib_info,inv_rates=1)
                      END IF
                   END IF
                   !print *,'   done with rate'
                END DO

                index_to_a = index_to_a + 1
                IF (index_to_a <= MAX_ISO) THEN
                   a = iso_list(z,index_to_a)
                ELSE
                   a = -1
                END IF
             END DO
          END DO

       END IF

       CALL save_rlib_iso_list(lib,pcom)
       ! Set Library Info property All Inverses Present
       IF (all_inv_present) THEN
          tmps = 'YES'
       ELSE
          tmps = 'NO'
       END IF
       CALL set_prop_value('All Inverses Present',tmps,lib_info, &
            RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
       CALL save_rlib_info(lib,pcom,lib_info)
    END DO

    ! Remove last linefeed from report
    report = report(:LEN_TRIM(report)-1)

    WRITE(*,'(A)') 'MODIFY=SUCCESS'
    WRITE(*,'(A)',ADVANCE='NO') 'REPORT='
    !report = replaceall(report,ACHAR(8),ACHAR(10))
    CALL print_long_string(TRIM(report))
  END SUBROUTINE add_missing_inv
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/share_rlib
  ! NAME
  !  SUBROUTINE share_rlib(pcom)
  ! PURPOSE
  !  Perform the SHARE RATE LIBRARY action
  !
  !  This subroutine moves libraries from the USER library group to the
  !  SHARED group.
  ! STATUS
  !  Complete
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE share_rlib(pcom)
    USE io
    USE fileio
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=MAX_RGRP_LEN)   :: group
    CHARACTER(LEN=200)            :: list,item,path
    CHARACTER(LEN=10240)          :: report
    CHARACTER(LEN=MAX_RATE_LEN)   :: lib_info,notes
    INTEGER(KIND=4)               :: i
    LOGICAL                       :: loop

    ! Put list of libraries into lib_list
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    report = ''
    list = cina_decode(pcom%BODY)
    loop = .TRUE.
    DO WHILE (loop)
       loop = next_in_list(item,list,ACHAR(9))

       ! See if this library name is already used in SHARED
       IF (file_exists(TRIM(CINA_PATH) // 'SHARED/rate_libs/' // &
            TRIM(item) // '/Library_Info')) THEN
          CALL report_error('Library ' // TRIM(item) // ' can not be ' // &
               'added to SHARED because a library with that name exists.', &
               'Improper usage',pcom,1)
       END IF

       path = get_rlib_path(item,pcom%USER,group)

       SELECT CASE (group)
       CASE ('')
          CALL report_error('Rate library "' // TRIM(item) // &
               '" was not found.','Improper usage',pcom,1)
       CASE ('USER')
          ! Remove this library from rlib_buffer but don't put it in the
          ! trash
          CALL rm_rlib(item,pcom,move_to_trash=.FALSE.)

          ! Move library to SHARED
          i = safe_shell('/bin/mv -f ''' // TRIM(path) // ''' ''' // &
               TRIM(CINA_PATH) // 'SHARED/rate_libs''')
          IF (i /= 0) THEN
             WRITE(path,'(A,I0,3A)') 'Error (',i, &
                  ') moving rate library "',TRIM(item),'" to SHARED'
             CALL report_error(TRIM(path),'External program',pcom,1)
          END IF

          ! Add library to SHARED library list
          CALL add_rlib(item,pcom,'SHARED',override=.TRUE.)

          ! Save new SHARED library list
          ! CALL save_rlib_list('SHARED',pcom)

          ! Append message to Library notes
          CALL read_rlib_info(item,pcom,lib_info)
          notes = get_prop_value('Library Notes',lib_info, &
               RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
          IF (notes /= '') notes = TRIM(notes) // ACHAR(10)
          notes = TRIM(notes) // 'This library was moved to the SHARED' // &
               ' folder by the "' // TRIM(pcom%USER) // '" user on ' // &
               get_date() // ' ' // get_time() // '.'
          CALL set_prop_value('Library Notes',notes,lib_info, &
               RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
          CALL save_rlib_info(item,pcom,lib_info)

          report = 'The "' // TRIM(item) // '" library has been moved ' // &
               'to the SHARED folder.'
       CASE DEFAULT
          CALL report_error('The ' // TRIM(group) // ' library "' // &
               TRIM(item) // '" may not be moved to the shared folder', &
               'Improper usage',pcom,1)
       END SELECT
    END DO

    WRITE(*,'(A)') 'MODIFY=SUCCESS'
    WRITE(*,'(A)',ADVANCE='NO') 'REPORT='
    CALL print_long_string(TRIM(report))
  END SUBROUTINE share_rlib
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/export_rlib_cgi
  ! NAME
  !  SUBROUTINE export_rlib_cgi(pcom)
  ! PURPOSE
  !  Perform the EXPORT RATE LIBRARY action
  !
  !  This subroutine exports libraries to a specified format, compresses
  !  the exported library, and sends it to the user.
  ! STATUS
  !  Complete
  ! MODIFICATION HISTORY
  !  02/16/2005 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE export_rlib_cgi(pcom)
    USE large_data
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=MAX_RLIB_LEN)   :: lib
    CHARACTER(LEN=MAX_PATH_LEN)   :: path
    CHARACTER(LEN=10000)          :: tmps
    INTEGER(KIND=4)               :: s
    TYPE(large_data_file)         :: ldf

    ! Put list of libraries into lib_list
    CALL read_rlib_list('PUBLIC',pcom)
    CALL read_rlib_list('SHARED',pcom)
    CALL read_rlib_list('USER',pcom)

    lib = cina_decode(pcom%BODY)
    ! Path for exported files
    path = TRIM(TEMP_PATH) // pcom%ID // '/export_rlib/'

    ! Make sure path exists
    s = SYSTEM('/bin/mkdir -p ''' // TRIM(path) // '''')
    IF (s /= 0) CALL report_error('Unable to make dir for library', &
         'External program',pcom,1)

    ! Make sure path is empty
    s = SYSTEM('/bin/rm -fr ''' // TRIM(path) // '*''')
    IF (s /= 0) CALL report_error('Unable to clean dir for library', &
         'External program',pcom,1)

    tmps = ''
    CALL export_rlib(lib,path,pcom%FORMAT,pcom,tmps,.FALSE.,.FALSE.)

    ldf%Ufile = TRIM(path) // '*'
    ldf%Cfile = TRIM(path) // TRIM(lib) // '.zip'
    CALL ldf_compress(ldf,whole_dir=.TRUE.)

    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str), &
         'File input/output',pcom,1)

    ldf%Usize = ldf%Csize  ! Send compressed size in header
    CALL ldf_send(ldf,del=.TRUE.)

    IF (ldf%status /= LDF_ERR_OK) CALL report_error(TRIM(ldf%err_str), &
         'File input/output',pcom,1)
  END SUBROUTINE export_rlib_cgi
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/copy_rlib
  ! NAME
  !  SUBROUTINE copy_rlib(src_lib,dest_lib,pcom,report,dest_path)
  ! PURPOSE
  !  Copy and/or error check a source library to a destination library
  ! STATUS
  !  Complete
  ! CAUTION
  !  Load rlib_lists and call add_rlib beforehand. This procedure copies all
  !  the rates in src_lib to dest_lib, updates rid lists, updates isotope
  !  lists, and creates library info.  However, save_rlib_list is not called
  !  and should be called after copy_rlib unless saving the destination
  !  library outside the suite.
  !
  !  dest_lib may be empty if a library should only be checked for errors
  !
  !  If pcom%FORMAT == 'NO LIB NAME' then library name will not appear in
  !  report.  This is used in em_syn_setup.
  ! INPUTS
  !  src_lib: name of library that should be copied to a new library
  !  dest_lib: name of new library that is a copy of src_lib or empty
  !  pcom: cina_common variable needed for reporting errors
  !  report: string containing existing report
  !  dest_path (OPTIONAL): use for saving dest_lib outside of suite
  ! OUTPUTS
  !  report: string with new report appended to existing report
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  !  11/17/2004 jpscott       Added pcom%FORMAT test
  ! SOURCE

  SUBROUTINE copy_rlib(src_lib,dest_lib,pcom,report,dest_path)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: src_lib,dest_lib,dest_path
    CHARACTER(LEN=*),INTENT(INOUT):: report
    OPTIONAL                      :: dest_path

    CHARACTER(LEN=32750)          :: lib_info
    CHARACTER(LEN=MAX_RLIB_LEN)   :: lib
    CHARACTER(LEN=MAX_RID_LEN)    :: rids(MAX_RATES)
    CHARACTER(LEN=MAX_RATE_LEN)   :: properties,value
    CHARACTER(LEN=100)            :: reac_str
    INTEGER(KIND=2)               :: iso_list(0:MAX_Z,MAX_ISO)
    INTEGER(KIND=4)               :: z,a,index_to_a,num_rids,i,parm_num
    INTEGER(KIND=4)               :: rate_counter
    REAL(KIND=8)                  :: parms(MAX_A)
    LOGICAL(KIND=4)               :: chk,problem

    ! Make sure a destination library is not a source library
    IF (src_lib == dest_lib) CALL report_error('The output library "' // &
         TRIM(dest_lib) // '" may not also be an input library.', &
         'Improper usage',pcom,1)

    CALL read_rlib_iso_list(src_lib,pcom,iso_list)

    chk = pcom%CHK_TEMP .OR. pcom%CHK_OVFL .OR. pcom%CHK_INV

    IF (dest_lib == '' .AND. (.NOT. chk)) THEN
       report = TRIM(report) // 'No libraries were modified or ' // &
            'checked for errors.' // ACHAR(8)
       RETURN
    END IF

    IF (pcom%FORMAT /= 'NO LIB NAME') THEN
       IF (pcom%CHK_TEMP) report = TRIM(report) // TRIM(src_lib) // &
            ' will be checked for rate temperature problems.' // &
            ACHAR(8)
       IF (pcom%CHK_OVFL) report = TRIM(report) // TRIM(src_lib) // &
            ' will be checked for rate overflow problems.' // ACHAR(8)
       IF (pcom%CHK_INV) report = TRIM(report) // TRIM(src_lib) // &
            ' will be checked for inverse rates.' // ACHAR(8) // &
            'CAUTION: Decays are not checked for inverse rates.' // &
            ACHAR(8)
    END IF


    ! Create lib_info for library
    lib_info = ''
    CALL set_prop_value('Library Recipe','Copied all rates from ' // &
         TRIM(src_lib),lib_info,RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
    CALL set_prop_value('Creation Date',get_date() // ' ' // &
         get_time(),lib_info,RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)

    rate_counter = 0
    DO z = 0, MAX_Z
       index_to_a = 1
       a = iso_list(z,index_to_a)

       DO WHILE (a > 0)
          CALL read_rid_list(src_lib,pcom,z,a,rids,num_rids)
          rate_counter = rate_counter + num_rids
          IF (dest_lib /= '') CALL open_rate_info(dest_lib,pcom,z,a)

          DO i = 1, num_rids
             ! Use reac_str as a temp string
             reac_str = rids(i)
             ! problem will be T if rids(i) is a link
             CALL correct_rid(reac_str,dest_lib,problem)

             ! Don't load properties if a link
             CALL decode_rid(rids(i),lib)
             IF (problem) THEN  ! If a link then
                properties = ''
             ELSE
                properties = read_rate_info(rids(i),pcom,'')
             END IF

             IF (dest_lib /= '') CALL change_rate_info(rids(i),pcom, &
                  properties)

             IF (chk) THEN
                ! Load properties if rids(i) is a link
                IF (problem) THEN
                   CALL read_rid_list(lib,pcom,z,a)
                   properties = read_rate_info(rids(i),pcom,'')
                   problem = .FALSE.
                END IF

                ! Get parameter array from properties
                CALL get_parm_array(properties,parms,parm_num)
                CALL decode_rid(rids(i),unique_reac_str=reac_str)

                IF (parm_num == 0) THEN
                   report = TRIM(report) // 'Problem obtaining ' // &
                        'parameters for "' // TRIM(reac_str) // '"' // &
                        ACHAR(8)
                   problem = .TRUE.
                ELSE IF (parm_num < 0) THEN
                   WRITE(value,'(A,I0,A)') &
                        'Problem interpreting parameter ',-parm_num, &
                        ' for "' // TRIM(reac_str) // '"'
                   report = TRIM(report) // TRIM(value) // ACHAR(8)
                   problem = .TRUE.
                ELSE IF (MOD(parm_num,7) /= 0) THEN
                   WRITE(value,'(A,I0,A)') 'Number of parameters (', &
                        parm_num,' is not divisible by 7 for "' // &
                        TRIM(reac_str) // '"'
                   report = TRIM(report) // TRIM(value) // ACHAR(8)
                   problem = .TRUE.
                ELSE
                   problem = .FALSE.
                END IF

                IF (.NOT. problem) THEN
                   IF (pcom%CHK_TEMP) &
                        CALL check_temp_behavior(parms,parm_num,report)
                   IF (pcom%CHK_OVFL) &
                        CALL check_overflow(parms,parm_num,report)
                   IF (pcom%CHK_INV) CALL check_for_inverse( &
                        rids(i),properties,src_lib,pcom,report)
                END IF
             END IF
          END DO

          IF (dest_lib /= '') THEN
             IF (PRESENT(dest_path)) THEN
                CALL save_rate_info(pcom,report,lib_info,dest_path,plevel=2)
             ELSE
                CALL save_rate_info(pcom,report,lib_info,plevel=2)
             END IF
          END IF

          index_to_a = index_to_a + 1
          IF (index_to_a <= MAX_ISO) THEN
             a = iso_list(z,index_to_a)
          ELSE
             a = -1
          END IF
       END DO
    END DO

    IF (pcom%FORMAT /= 'NO LIB NAME') THEN
       IF (pcom%CHK_TEMP) report = TRIM(report) // 'Rate temperature ' // &
            'check complete.' // ACHAR(8)
       IF (pcom%CHK_OVFL) report = TRIM(report) // 'Rate overflow ' // &
            'check complete.' // ACHAR(8)
       IF (pcom%CHK_INV) report = TRIM(report) // 'Inverse rate ' // &
            'check complete.' // ACHAR(8)
    END IF

    IF (dest_lib /= '') THEN
       IF (PRESENT(dest_path)) THEN
          CALL save_rlib_iso_list(dest_lib,pcom,dest_path)
          CALL save_rlib_info(dest_lib,pcom,lib_info,dest_path)
       ELSE
          CALL save_rlib_iso_list(dest_lib,pcom)
          CALL save_rlib_info(dest_lib,pcom,lib_info)
       END IF

       WRITE(value,'(A,I0,A)') TRIM(dest_lib) // ' now has ', &
            rate_counter, ' rates.' // ACHAR(8)
       report = TRIM(report) // 'All rates in ' // TRIM(src_lib) // &
            ' were copied to ' // TRIM(dest_lib) // ACHAR(8) // TRIM(value)
    END IF
  END SUBROUTINE copy_rlib
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/merge_rlib
  ! NAME
  !  SUBROUTINE merge_rlib(src_libs,dest_lib,pcom,report,src_path,dest_path)
  ! PURPOSE
  !  Merge and error check source rate libraries to a destination library
  ! STATUS
  !  Complete
  ! CAUTION
  !  Load rlib_lists and call add_rlib beforehand. This procedure merges all
  !  the rates in src_libs to dest_lib, updates rid lists, updates isotope
  !  lists, and creates library info.  However, save_rlib_list is not called
  !  and should be called after merge_rlib unless saving the destination
  !  library outside the suite.  The library listed first in src_libs has
  !  the highest priority.
  !
  !  dest_lib may be empty if the merge should only be checked for errors
  ! INPUTS
  !  src_libs: name of libraries that should be merged to a new library
  !  dest_lib: name of new library (may not be empty)
  !  pcom: cina_common variable needed for reporting errors
  !  report: string containing existing report
  !  dest_path (OPTIONAL): use for saving dest_lib outside of suite
  ! OUTPUTS
  !  report: string with new report appended to existing report
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE merge_rlib(src_libs,dest_lib,pcom,report,dest_path)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: src_libs,dest_lib,dest_path
    CHARACTER(LEN=*),INTENT(INOUT):: report
    OPTIONAL                      :: dest_path

    INTEGER(KIND=2)               :: iso_lists(MAX_RLIBS,0:MAX_Z,MAX_ISO)
    INTEGER(KIND=4)               :: src_lib_num,i,j,z,a,a_old,m
    INTEGER(KIND=4)               :: index_to_a(MAX_RLIBS),slib_index
    INTEGER(KIND=4)               :: rid_num,dest_rid_num,rate_counter
    INTEGER(KIND=4)               :: lib_counter(MAX_RLIBS)
    CHARACTER(LEN=MAX_RLIB_LEN)   :: lib,src_lib(MAX_RLIBS)
    CHARACTER(LEN=MAX_RID_LEN)    :: rids(MAX_RATES),dest_rids(MAX_RATES)
    CHARACTER(LEN=MAX_RATE_LEN)   :: properties
    CHARACTER(LEN=200)            :: tmps,slibs
    CHARACTER(LEN=32750)          :: lib_info
    LOGICAL(KIND=4)               :: loop,chk

    chk = pcom%CHK_TEMP .OR. pcom%CHK_OVFL .OR. pcom%CHK_INV
    IF (dest_lib == '') THEN
       report = TRIM(report) // 'No libraries were merged or ' // &
            'checked for errors.' // ACHAR(8)
       RETURN
    END IF

    ! Load src_libs into src_lib array and
    ! Make sure a destination library is not a source library and
    ! Load isotope list into iso_lists
    loop = .TRUE.
    src_lib_num = 0
    slibs = src_libs
    DO WHILE (loop)
       loop = next_in_list(lib,slibs,ACHAR(9))
       IF (lib == dest_lib) CALL report_error('The output library "' // &
            TRIM(lib) // '" may not also be an input library.', &
            'Improper usage',pcom,1)
       src_lib_num = src_lib_num + 1

       IF (src_lib_num > MAX_RLIBS) CALL report_error('Too many ' // &
            'libraries were specified.  Contact ' // &
            'coordinator@nucastrodata.org to have this limit increased.', &
            'Developer Reminder',pcom,1)

       src_lib(src_lib_num) = lib
       CALL read_rlib_iso_list(lib,pcom,iso_lists(src_lib_num,:,:))
    END DO

    ! Now make slibs a comma separated list of source libraries
    DO i = 1, src_lib_num
       IF (i == 1) THEN
          slibs = src_lib(1)
       ELSE
          slibs = TRIM(slibs) // ', ' // src_lib(i)
       END IF
    END DO

    ! Create lib_info for library
    lib_info = ''
    CALL set_prop_value('Library Recipe','Merged rates from ' // &
         TRIM(slibs),lib_info,RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
    CALL set_prop_value('Creation Date',get_date() // ' ' // &
         get_time(),lib_info,RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)

    rate_counter = 0
    lib_counter = 0
    DO z = 0, MAX_Z
       a_old = 0
       a = HUGE(a)
       index_to_a = 1

       DO WHILE (a > a_old)
          ! Find smallest a > a_old in all source lib isotope lists
          ! Note that iso_lists should be sorted from lowest to highest
          DO i = 1, src_lib_num
             m = iso_lists(i,z,index_to_a(i))
             IF (m > a_old .AND. m < a) a = m
          END DO
          ! Update index_to_a
          DO i = 1, src_lib_num
             IF (iso_lists(i,z,index_to_a(i)) == a) THEN
                IF (index_to_a(i) < MAX_ISO) THEN
                   index_to_a(i) = index_to_a(i) + 1
                   m = iso_lists(i,z,index_to_a(i))
                   IF (m > 0 .AND. m <= iso_lists(i,z,index_to_a(i)-1)) THEN
                      WRITE(tmps,'(3A,I0,A,I0)') 'Isotope list for ', &
                           TRIM(src_lib(i)),' was not sorted when z=',z, &
                           ' and a=',a
                      CALL report_error(tmps,'Bad file format',pcom,1)
                   END IF
                END IF
             END IF
          END DO

          IF (a == HUGE(a)) THEN
             a = 0 ! Exit loop
          ELSE
             ! Do merge for this isotope
             ! Clear dest_rids buffer
             dest_rids = ''
             dest_rid_num = 0

             DO slib_index = 1, src_lib_num
                IF (isotope_exist(src_lib(slib_index),z,a)) THEN
                   CALL read_rid_list(src_lib(slib_index),pcom,z,a,rids, &
                        rid_num)
                   IF (slib_index == 1) THEN
                      ! Copy all rate ids into dest_rids for library with
                      ! highest priority
                      dest_rids = rids
                      dest_rid_num = rid_num
                      rate_counter = rate_counter + rid_num
                      lib_counter(1) = lib_counter(1) + rid_num
                   ELSE
                      ! Copy any rate ids in rids to dest_rids if a
                      ! similar reaction isn't present
                      ! similar means same reaction and unique string
                      DO i = 1, rid_num
                         ! Store part to compare with dest_rids into tmps
                         tmps = rids(i)(INDEX(rids(i),ACHAR(9))+1:)

                         ! Check if this reaction is in dest_rids already
                         ! If so don't copy, otherwise copy
                         ! loop is FALSE if reaction exists already
                         loop = .TRUE.
                         j = 1
                         DO WHILE(loop .AND. j <= dest_rid_num)
                            IF (tmps == dest_rids(j)(INDEX(dest_rids(j), &
                                 ACHAR(9))+1:)) loop = .FALSE.
                            j = j + 1
                         END DO
                         IF (loop) THEN
                            ! Copy rid to dest_rids
                            dest_rid_num = dest_rid_num + 1
                            dest_rids(dest_rid_num) = rids(i)
                            rate_counter = rate_counter + 1
                            lib_counter(slib_index) = &
                                 lib_counter(slib_index) + 1
                         END IF
                      END DO
                   END IF
                END IF
             END DO

             CALL open_rate_info(dest_lib,pcom,z,a)

             DO i = 1, dest_rid_num
                tmps = dest_rids(i)
                ! loop will be T if a link
                CALL correct_rid(tmps,dest_lib,loop)

                ! Don't load properties if a link
                IF (loop) THEN  ! If a link then
                   properties = ''
                ELSE
                   properties = read_rate_info(dest_rids(i),pcom,'')
                END IF

                CALL change_rate_info(dest_rids(i), pcom,properties)
             END DO

             IF (PRESENT(dest_path)) THEN
                CALL save_rate_info(pcom,report,lib_info,dest_path, &
                     plevel=2)
             ELSE
                CALL save_rate_info(pcom,report,lib_info,plevel=2)
             END IF

             a_old = a
             a = HUGE(a)
          END IF
       END DO
    END DO

    IF (PRESENT(dest_path)) THEN
       CALL save_rlib_iso_list(dest_lib,pcom,dest_path)
       CALL save_rlib_info(dest_lib,pcom,lib_info,dest_path)
    ELSE
       CALL save_rlib_iso_list(dest_lib,pcom)
       CALL save_rlib_info(dest_lib,pcom,lib_info)
    END IF

    WRITE(tmps,'(A,I0,A)') TRIM(dest_lib) // ' now has ', &
         rate_counter, ' rates.' // ACHAR(8)
    report = TRIM(report) // 'Libraries ' // TRIM(slibs) // &
         ' were merged from highest to lowest priority to create ' // &
         TRIM(dest_lib) // ACHAR(8) // 'The ' // TRIM(src_lib(1)) // &
         ' library has the highest priority.' // ACHAR(8) // TRIM(tmps)
    DO i = 1, src_lib_num
       WRITE(tmps,'(I0,2A)') lib_counter(i),' rates were copied from ', &
            src_lib(i)
       report = TRIM(report) // TRIM(tmps) // ACHAR(8)
    END DO

    ! Check for errors after merge is complete
    IF (chk) CALL copy_rlib(dest_lib,'',pcom,report)
  END SUBROUTINE merge_rlib
  !***

  !-------------------------------------------------------------------------
  !****if* rate_man/in_iso_list
  ! NAME
  !  FUNCTION in_iso_list(sunet,z,a,name,any26al)
  ! PURPOSE
  !  Check if isotope is in sunet
  ! STATUS
  !  Complete but untested
  ! CAUTION
  !  Isotope list must be loaded beforehand
  ! INPUTS
  !  sunet:
  !  z:
  !  a:
  !  name (OPTIONAL):
  !  any26al (OPTIONAL): Set to T to search for any state of 26Al
  ! RETURN VALUE
  !  .TRUE. or .FALSE. LOGICAL(KIND=4) if isotope exists
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  12/06/2004 jpscott       Start function
  ! SOURCE

  FUNCTION in_iso_list(sunet,z,a,name,any26al)
    IMPLICIT NONE
    TYPE(sunet_list),INTENT(IN)      :: sunet
    CHARACTER(LEN=*),INTENT(IN)      :: name
    INTEGER(KIND=4),INTENT(IN)       :: z,a
    LOGICAL(KIND=4),INTENT(IN)       :: any26al
    LOGICAL(KIND=4)                  :: in_iso_list
    INTEGER(KIND=4)                  :: i,m
    CHARACTER(LEN=5)                 :: sname
    OPTIONAL                         :: name,any26al

    in_iso_list = .FALSE.
    IF (PRESENT(any26al)) THEN
       in_iso_list = sunet%al26 .OR. sunet%al26star .OR. sunet%al26dash
       RETURN
    END IF

    IF (PRESENT(name)) THEN
       sname = lowercase(name)
       sname = ADJUSTL(sname)

       IF (z == 13 .AND. a == 26) THEN
          IF (sname == 'al26 ' .OR. sname == '26al ') THEN
             in_iso_list = sunet%al26
          ELSE IF (sname == 'al*6 ' .OR. sname == '*6al ') THEN
             in_iso_list = sunet%al26star
          ELSE IF (sname == 'al-6 ' .OR. sname == '-6al ') THEN
             in_iso_list = sunet%al26dash
          END IF

          RETURN
       END IF
    END IF

    ! Check for valid z
    IF (z > MAX_Z .OR. z < 0) RETURN

    m = sunet%iso_list(z,1)
    i = 1  ! counter through iso_list
    DO WHILE (m > 0)
       IF (m == a) THEN
          in_iso_list = .TRUE.
          RETURN
       END IF
       i = i + 1
       IF (i > MAX_ISO) RETURN
       m = sunet%iso_list(z,i)
    END DO
  END FUNCTION in_iso_list
  !***

  !---------------------------------------------------------------------
  !****is* rate_man/export_rlib
  ! NAME
  !  SUBROUTINE export_rlib(src_lib,dest_path,format,pcom,report,
  !                         meta_stable)
  ! PURPOSE
  !  Export library to a file in a specific format
  ! STATUS
  !  Complete
  ! CAUTION
  !  Load rlib_lists beforehand.
  !
  !  If pcom%FORMAT == 'NO LIB NAME' then library name will not appear in
  !  report.  This is used in em_syn_setup.
  ! INPUTS
  !  src_lib: name of library that should be copied to a new library
  !  dest_path: path for saving exported files.  Must end with a slash
  !  format: format for exported library
  !  pcom: cina_common variable needed for reporting errors
  !  report: string containing existing report
  !  meta_stable: T if meta-stable Al26 rates are in src_lib
  !  header: T if number of weak reactions should be first line in file
  ! OUTPUTS
  !  report: string with new report appended to existing report
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  !  Eric Lingerfelt (lingerfeltej@ornl.gov)
  ! MODIFICATION HISTORY
  !  07/28/2009 lingerfeltej  Added code so that only isotopes with
  !                           an entry in the winvn file are added
  !                           to the netwinv and sunet files
  !  11/17/2004 jpscott       Start subroutine
  !  02/15/2005 jpscott       Added required header attribute
  ! SOURCE

  SUBROUTINE export_rlib(src_lib,dest_path,format,pcom,report, &
       meta_stable,header)
    USE reactionstrings
    USE convert
    USE fmt_parm
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: src_lib,dest_path,format
    CHARACTER(LEN=*),INTENT(INOUT):: report
    LOGICAL(KIND=4),INTENT(IN)    :: meta_stable,header

    TYPE(fmt_parm_opt)            :: opt
    CHARACTER(LEN=MAX_RLIB_LEN)   :: lib
    CHARACTER(LEN=MAX_RID_LEN)    :: rids(MAX_RATES),srids(MAX_RATES)
    CHARACTER(LEN=MAX_RATE_LEN)   :: properties,value
    CHARACTER(LEN=100)            :: reac_str,tmps
    CHARACTER(LEN=5)              :: name
    CHARACTER(LEN=10)             :: rtn
    INTEGER(KIND=2)               :: iso_list(0:MAX_Z,MAX_ISO)
    INTEGER(KIND=2)               :: iso_local(0:MAX_Z,MAX_ISO)
    INTEGER(KIND=4)               :: z,a,index_to_a,num_rids,i,j,parm_num,val
    INTEGER(KIND=4)               :: rate_counter,rtype,sorti(MAX_RATES)
    REAL(KIND=8)                  :: parms(MAX_A),ar
    LOGICAL(KIND=4)               :: problem,rtype_sort
    CHARACTER(LEN=5) &
    , ALLOCATABLE, DIMENSION(:)   :: allowed_iso_array
    INTEGER(KIND=4)               :: allowed_iso_counter,allowed_iso_num
    TYPE(reactionparticles)       :: r(MAX_RATES)

    CALL read_rlib_iso_list(src_lib,pcom,iso_list)
    iso_local = -1

    IF (dest_path == '') THEN
       report = TRIM(report) // 'No libraries were exported.' // ACHAR(8)
       RETURN
    ELSE
       OPEN(RLIB_NETSU_UNIT, FILE=TRIM(dest_path) // 'netsu', IOSTAT=i, &
            ACTION='WRITE')
       IF (i /= 0) CALL report_error('Error opening new netsu file', &
            'File input/output',pcom,1)

       OPEN(RLIB_SUNET_UNIT, FILE=TRIM(dest_path) // 'sunet', IOSTAT=i, &
            ACTION='WRITE')
       IF (i /= 0) CALL report_error('Error opening new sunet file', &
            'File input/output',pcom,1)

       OPEN(RLIB_NETWINV_UNIT, FILE=TRIM(dest_path) // 'netwinv', IOSTAT=i, &
            ACTION='WRITE')
       IF (i /= 0) CALL report_error('Error opening new netwinvn file', &
            'File input/output',pcom,1)

       OPEN(RLIB_WINVN_UNIT, FILE=WINVN_PATH, IOSTAT=i, ACTION='READ')
       IF (i /= 0) CALL report_error('Error opening winvn file', &
            'File input/output',pcom,1)
    END IF

    IF (pcom%FORMAT /= 'NO LIB NAME') report = TRIM(report) // &
         TRIM(src_lib) // ' will be exported in ' // TRIM(format) // &
         ' format.' // ACHAR(8)

    rate_counter = 0
    rtype_sort = parm_fmt_sort_rtype(format)
    IF (rtype_sort) THEN
       rtype = 1
    ELSE
       rtype = 8
    END IF

    ! Read in allowed_iso_num
    READ(RLIB_WINVN_UNIT,'(I4)',IOSTAT=i) allowed_iso_num
    IF (i /= 0) CALL report_error('Error reading winvn allowed_iso_num', &
         'File input/output',pcom,1)

    ! Read in winvn header 2
    READ(RLIB_WINVN_UNIT,'(A)',IOSTAT=i) value
    IF (i /= 0) CALL report_error('Error reading winvn header2', &
         'File input/output',pcom,1)

    ! Skip over isotope list in winvn and get the maximum length
    ! for the allowed_iso_array. This array stores an isotope
    ! string for each isotope that has an entry in the complete
    ! winvn file.
    !allowed_iso_counter = 0;
    !i = 0
    !name = ''
    !DO WHILE (i == 0)
    !   tmps(1:5) = name
    !   READ(RLIB_WINVN_UNIT,'(A5)',IOSTAT=i) name
    !   IF (i > 0) THEN
    !      CALL report_error('Error skipping to data in winvn', &
    !             'File input/output',pcom,1)
    !   ELSE IF (i == 0) THEN
    !      IF (name == tmps(1:5)) THEN
    !          i = -20
    !      ELSE
    !          allowed_iso_counter = allowed_iso_counter + 1
    !      END IF
    !   END IF
    !END DO

    ! Allocate the allowed_iso_array to the maximum length
    ALLOCATE(allowed_iso_array(allowed_iso_num), STAT=i)
    IF (i /= 0) CALL report_error('Error allocating allowed isotope array', &
         'Array Allocation',pcom,1)

	allowed_iso_array = '';

    ! Rewind winvn back to the beginning of the file
    !REWIND(RLIB_WINVN_UNIT)

    ! Jump two lines to skip two headers
    !READ(RLIB_WINVN_UNIT, '(/)', IOSTAT=i)
    !IF (i /= 0) CALL report_error('Error reading winvn', &
    !     'File input/output',pcom,1)

    ! Read each isotope string into allowed_iso_array
    ! from the winvn file
    allowed_iso_counter = 1;
    i = 0
    name = ''
    DO WHILE (allowed_iso_counter <= allowed_iso_num)
       !tmps(1:5) = name
       READ(RLIB_WINVN_UNIT,'(A5)',IOSTAT=i) name
       !IF (i > 0) THEN
       !   CALL report_error('Error skipping to data in winvn', &
       !          'File input/output',pcom,1)
       !ELSE IF (i == 0) THEN
          !IF (name == tmps(1:5)) THEN
          !    i = -20
          !ELSE
              allowed_iso_array(allowed_iso_counter) = ADJUSTR(name)
              allowed_iso_counter = allowed_iso_counter + 1
          !END IF
       !END IF
    END DO

	i=0

 	! Make sunet file and count the number of isotopes
    rate_counter = 0
    DO z = 0, MAX_Z
       index_to_a = 1
       a = iso_list(z,index_to_a)
       
       DO WHILE (a > 0)
          IF (z == 13 .AND. a == 26 .AND. meta_stable) THEN
             rate_counter = rate_counter + 2
             WRITE(RLIB_SUNET_UNIT,'(A5/A5)',IOSTAT=i) ' al-6',' al*6'
          ELSE
          	name = lowercase(get_iso_str(z,a,1))
            name = ADJUSTR(name)
            ! Test to see if the current isotope has an entry in
            ! the winvn file
            IF(is_iso_allowed(name, allowed_iso_array, allowed_iso_num+1)) THEN
             rate_counter = rate_counter + 1
             WRITE(RLIB_SUNET_UNIT,'(A5)',IOSTAT=i) name
            END IF
          END IF
          IF (i /= 0) CALL report_error('Error writing sunet file', &
               'File input/output',pcom,1)
          index_to_a = index_to_a + 1
          IF (index_to_a <= MAX_ISO) THEN
             a = iso_list(z,index_to_a)
          ELSE
             a = -1
          END IF
       END DO
    END DO
    CLOSE(RLIB_SUNET_UNIT)

    WRITE(tmps,'(A,I0,A)') 'Library has ',rate_counter,' isotopes.'
    report = TRIM(report) // TRIM(tmps) // ACHAR(8)

	i=0

	WRITE(RLIB_NETWINV_UNIT,'(I5)',IOSTAT=i) rate_counter
    IF (i /= 0) CALL report_error('Error writing netwinv header1', &
         'File input/output',pcom,1)

    WRITE(RLIB_NETWINV_UNIT,'(A)',IOSTAT=i) TRIM(value)
    IF (i /= 0) CALL report_error('Error writing netwinv header2', &
         'File input/output',pcom,1)

    i=0

    DO z = 0, MAX_Z
       index_to_a = 1
       a = iso_list(z,index_to_a)
       DO WHILE (a > 0)
          IF (isotope_exist(src_lib,z,a)) THEN
            IF (z == 13 .AND. a == 26 .AND. meta_stable) THEN
               WRITE(RLIB_NETWINV_UNIT,'(A5/A5)',IOSTAT=i) ' al-6',' al*6'
            ELSE
                name = lowercase(get_iso_str(z,a,1))
                name = ADJUSTR(name)
                ! Test to see if the current isotope has an entry in
                ! the winvn file
                IF(is_iso_allowed(name, allowed_iso_array, allowed_iso_counter)) THEN
                    WRITE(RLIB_NETWINV_UNIT,'(A5)',IOSTAT=i) name
                END IF
            END IF
            iso_local(z,a) = 1
            IF (i /= 0) CALL report_error('Error writing netwinv isotopes', &
                 'File input/output',pcom,1)

          index_to_a = index_to_a + 1
          IF (index_to_a <= MAX_ISO) THEN
             a = iso_list(z,index_to_a)
          ELSE
             a = -1
          END IF
        END IF
       END DO
    END DO

    ! Read rest of winvn and copy isotopes in sunet to netwinv
    i = 0
    DO WHILE (i == 0)
       READ(RLIB_WINVN_UNIT,'(A5,F12.3,I4,A/A/A/A)',IOSTAT=i) name,ar,z,tmps, &
            reac_str,properties,value
       IF (i > 0) THEN
          WRITE(*,'(A,I0)') 'CAUTION=read error: ',i
          CALL report_error('Error reading winvn data', &
               'File input/output',pcom,1)
       ELSE IF (i == 0) THEN
          a = ar  ! Convert ar to integer
          IF (isotope_exist(src_lib,z,a)) THEN
            IF (z == 13 .AND. a == 26 .AND. meta_stable) THEN
               WRITE(RLIB_NETWINV_UNIT,'(A5,F12.3,I4,A/A/A/A)',IOSTAT=j) &
                    ' al-6',ar,z,TRIM(tmps),TRIM(reac_str),TRIM(properties), &
                    TRIM(value)
               IF (j /= 0) CALL report_error('Error writing al-6 to netwinv', &
                    'File input/output',pcom,1)

               WRITE(RLIB_NETWINV_UNIT,'(A5,F12.3,I4,A/A/A/A)',IOSTAT=j) &
                    ' al*6',ar,z,TRIM(tmps),TRIM(reac_str),TRIM(properties), &
                    TRIM(value)
               IF (j /= 0) CALL report_error('Error writing al*6 to netwinv', &
                    'File input/output',pcom,1)
  !          ELSE IF (isotope_exist(src_lib,z,a)) THEN
            ELSE
  !          ELSE IF (iso_local(z,a) > 0 ) THEN
               WRITE(RLIB_NETWINV_UNIT,'(A5,F12.3,I4,A/A/A/A)',IOSTAT=j) &
                    name,ar,z,TRIM(tmps),TRIM(reac_str),TRIM(properties), &
                    TRIM(value)
               IF (j /= 0) CALL report_error('Error writing ' // name // &
                    ' to netwinv','File input/output',pcom,1)
            END IF
         END IF
       END IF
    END DO

    CLOSE(RLIB_NETWINV_UNIT)
    CLOSE(RLIB_WINVN_UNIT)

    ! Deallocate the allowed_iso_array from memory
	DEALLOCATE(allowed_iso_array, STAT=i)
	IF (i /= 0) CALL report_error('Error deallocating allowed isotope array', &
         'Array Deallocation',pcom,1)

    ! Write number of weak reactions to netsu file
    IF (header) WRITE(RLIB_NETSU_UNIT,'(A)') '    0'
    rate_counter = 0
    DO WHILE (rtype <= 8)
       ! Print header for reaction type if needed
       IF (rtype_sort) THEN
          opt%reac_str = ''
          opt%unique_str = ''
          opt%biblio = ''
          opt%inverse = .FALSE.
          opt%resonant = .FALSE.
          opt%qvalue = 0.0
          opt%a = 0.0
          opt%a_num = 7

          WRITE(tmps,'(I0)') rtype
          value = parm_fmt_main(opt,format,iostat=j,rtype_in=tmps(1:1))
          IF (j == 0) THEN
             WRITE(RLIB_NETSU_UNIT,'(A)',ADVANCE='NO') TRIM(value)
          ELSE
             WRITE(tmps,'(A,I0,4A)') 'Error ',j,' while producing ', &
                  TRIM(format),' format for ',TRIM(reac_str)
             report = TRIM(report) // TRIM(tmps) // ACHAR(8)
          END IF
       END IF

       DO z = 0, MAX_Z
          index_to_a = 1
          a = iso_list(z,index_to_a)

          DO WHILE (a > 0)
             CALL read_rid_list(src_lib,pcom,z,a,rids,num_rids)

             ! Read reaction strings from rids and put in r
             DO i = 1, num_rids
                CALL decode_rid(rids(i),lib,reac_str=reac_str)
                CALL read_reac_str(r(i),reac_str,tmps)
                IF (tmps /= '') THEN
                   CALL report_error('Error interpreting string "' // &
                        TRIM(reac_str) // '" in ' // TRIM(lib) // ': ' // &
                        TRIM(tmps),'Improbable',pcom,1)
                END IF
             END DO

             ! Sort rids
             CALL sort_r(r,num_rids,sorti)
             !WRITE(tmps,'(9(I0,1X))') sorti(1:9)
             !report = TRIM(report) // 'sorti = ' // TRIM(tmps) // ACHAR(8)

             DO i = 1, num_rids
                !sorti(i) = num_rids - i + 1
                srids(i) = rids(sorti(i))
                !report = TRIM(report) // TRIM(srids(i)) // ACHAR(8)
             END DO

             DO i = 1, num_rids
                ! Load rid list for linked library if needed
                CALL decode_rid(srids(i),lib,rtype=j,unique_reac_str=reac_str)
                IF ((.NOT. rtype_sort) .OR. (j == rtype)) THEN
                   ! Save rate if not sorting by reaction type or if
                   ! rate is correct reaction type

                   rate_counter = rate_counter + 1

                   IF (lib /= src_lib) CALL read_rid_list(lib,pcom,z,a)
                   properties = read_rate_info(srids(i),pcom,'')

                   ! Get parameter array from properties
                   CALL get_parm_array(properties,parms,parm_num)

                   problem = .FALSE.
                   IF (parm_num == 0) THEN
                      report = TRIM(report) // 'Problem obtaining ' // &
                           'parameters for "' // TRIM(reac_str) // '"' // &
                           ACHAR(8)
                      problem = .TRUE.
                   ELSE IF (parm_num < 0) THEN
                      WRITE(value,'(A,I0,A)') &
                           'Problem interpreting parameter ',-parm_num, &
                           ' for "' // TRIM(reac_str) // '"'
                      report = TRIM(report) // TRIM(value) // ACHAR(8)
                      problem = .TRUE.
                   ELSE IF (MOD(parm_num,7) /= 0) THEN
                      WRITE(value,'(A,I0,A)') 'Number of parameters (', &
                           parm_num,' is not divisible by 7 for "' // &
                           TRIM(reac_str) // '"'
                      report = TRIM(report) // TRIM(value) // ACHAR(8)
                      problem = .TRUE.
                   ELSE
                      problem = .FALSE.
                   END IF

                   ! Get Q-value
                   value = get_prop_value('Q-value',properties, &
                        RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
                   READ(value,'(G)') opt%qvalue
                   IF (.NOT. problem .AND. opt%qvalue == 0d0) report = &
!                        TRIM(report) // 'CAUTION! Q-value is zero for ' // &
                        TRIM(report) // 'Q-value is zero for ' // &
                        TRIM(reac_str) // ACHAR(8)

                   IF (.NOT. problem) THEN
                      ! Fill in opt
                      CALL decode_rid(srids(i),reac_str=opt%reac_str, &
                           unique=opt%unique_str)
                      opt%a = parms
                      opt%a_num = parm_num

                      opt%biblio = get_prop_value('Biblio Code',properties, &
                           RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

                      ! Get value of inverse flag
                      tmps = ',' // TRIM(get_prop_value('Reaction Type', &
                           properties,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)) &
                           // ','
                      IF (INDEX(tmps,',v,') > 0) THEN
                         opt%inverse = .TRUE.
                      ELSE
                         opt%inverse = .FALSE.
                      END IF

                      ! Get n, r flags
                      opt%resonant = .FALSE.
                      tmps = get_prop_value('Resonant Components',properties, &
                           RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
                      j = 1  ! Index into tmps
                      parm_num = 1     ! Index into opt%resonant
                      DO WHILE (tmps(j:j) /= ' ')
                         IF (tmps(j:j+1) == 'nr') THEN
                            opt%resonant(parm_num) = .FALSE.
                            j = j + 2
                            parm_num = parm_num + 1
                         ELSE IF (tmps(j:j) == 'r') THEN
                            opt%resonant(parm_num) = .TRUE.
                            j = j + 1
                            parm_num = parm_num + 1
                         END IF
                         IF (tmps(j:j) == ',') THEN
                            j = j + 1
                         ELSE IF (tmps(j:j) == ' ') THEN
                         ELSE
                            report = TRIM(report) // &
                                 'Resonant components for '// TRIM(reac_str) &
                                 // ' are in wrong format' // ACHAR(8)
                         END IF
                      END DO

                      value = parm_fmt_main(opt,format,j,rtype_in='0')
                      IF (j == 0) THEN
                         WRITE(RLIB_NETSU_UNIT,'(A)',ADVANCE='NO') TRIM(value)
                      ELSE
                         WRITE(tmps,'(A,I0,A)') 'Error ',j, &
                              ' while producing ' // TRIM(format) //  &
                              ' format for ' // TRIM(reac_str)
                         report = TRIM(report) // TRIM(tmps) // ACHAR(8)
                      END IF
                   END IF
                END IF
             END DO

             index_to_a = index_to_a + 1
             IF (index_to_a <= MAX_ISO) THEN
                a = iso_list(z,index_to_a)
             ELSE
                a = -1
             END IF
          END DO
       END DO
       rtype = rtype + 1
    END DO

    IF (pcom%FORMAT /= 'NO LIB NAME') report = TRIM(report) // &
         'Library export complete.' // ACHAR(8)

    WRITE(value,'(A,I0,A)') 'Exported library now has ',rate_counter, &
         ' rates.' // ACHAR(8)
    report = TRIM(report) // TRIM(value)

    CLOSE(RLIB_NETSU_UNIT)
  END SUBROUTINE export_rlib

!---------------------------------------------------------------------
  !****is* rate_man/is_iso_allowed
  ! NAME
  !  FUNCTION is_iso_allowed(iso_name, iso_array)
  ! PURPOSE
  !  To detect if iso_name is an element of iso_array
  ! STATUS
  !  Complete
  !
  !  This is used in export_rlib.
  ! INPUTS
  !  iso_name: name of the isotope to search for.
  !  iso_array_length: number of elements in the array
  !  iso_array: an array of isotope names
  ! OUTPUTS
  !  returns: true is iso_name is found in the array
  ! AUTHORS
  !  Eric J. Lingerfelt (lingerfeltej@ornl.gov)
  ! MODIFICATION HISTORY
  !  07/28/2009 elingerf      Function created and tested
  ! SOURCE
  LOGICAL FUNCTION is_iso_allowed(iso_name, iso_array, iso_array_length)
    INTEGER(KIND=4),  INTENT(IN) :: iso_array_length
    CHARACTER(LEN=5), INTENT(IN) :: iso_name
    CHARACTER(LEN=5) &
    , DIMENSION(iso_array_length) &
    , INTENT(IN)                 :: iso_array
    INTEGER(KIND=4)              :: counter

    counter = 1
    is_iso_allowed = .FALSE.
    search_loop: DO WHILE(counter<=iso_array_length)
        IF(iso_name==iso_array(counter)) THEN
            is_iso_allowed = .TRUE.
            EXIT search_loop
        END IF
        counter = counter + 1
    END DO search_loop
  END FUNCTION is_iso_allowed

  !---------------------------------------------------------------------
  !****if* rate_man/check_temp_behavior
  ! NAME
  !  SUBROUTINE check_temp_behavior(a,a_num,report)
  ! PURPOSE
  !  Check if parameters have proper temperature bahavior
  ! STATUS
  !  In development
  ! CAUTION
  !  Parameters are considered to have proper temperature bahavior if . . .
  ! INPUTS
  !  a: array of parameters
  !  a_num: number of parameters to use in a array
  !  report: string containing existing report
  ! OUTPUTS
  !  report: string with new report appended to existing report
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE check_temp_behavior(a,a_num,report)
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)       :: a(:)
    INTEGER(KIND=4),INTENT(IN)    :: a_num
    CHARACTER(LEN=*),INTENT(INOUT):: report

    ! Finish this later
  END SUBROUTINE check_temp_behavior
  !***

  !---------------------------------------------------------------------
  !****if* rate_man/check_overflow
  ! NAME
  !  SUBROUTINE check_overflow(a,a_num,report)
  ! PURPOSE
  !  Check if parameters cause an overflow
  ! STATUS
  !  In development
  ! CAUTION
  !  Parameters are considered to cause an overflow if . . .
  ! INPUTS
  !  a: array of parameters
  !  a_num: number of parameters to use in a array
  !  report: string containing existing report
  ! OUTPUTS
  !  report: string with new report appended to existing report
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE check_overflow(a,a_num,report)
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)       :: a(:)
    INTEGER(KIND=4),INTENT(IN)    :: a_num
    CHARACTER(LEN=*),INTENT(INOUT):: report

    ! Finish this later
  END SUBROUTINE check_overflow
  !***

  !---------------------------------------------------------------------
  !****if* rate_man/check_for_inverse
  ! NAME
  !  SUBROUTINE check_for_inverse(rid,pcom,report,inv_exist)
  ! PURPOSE
  !  Check for a valid inverse reaction for a rid
  ! STATUS
  !  Complete but detailed balance isn't checked yet
  ! CAUTION
  !  Doesn't modify unique string and it should for decays
  ! INPUTS
  !  rid: rate id to check
  !  src_lib: name of library rid was obtained from
  !  pcom: cina_common variable needed for reporting errors
  !  report: string containing existing report
  ! OUTPUTS
  !  report: string with new report appended to existing report
  !  inv_exist (OPTIONAL): T if inverse rate is present
  ! TODO
  !  Check for detailed balance between rate and inverse rate
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/23/2004 jpscott       Start subroutine
  !  11/18/2004 jpscott       Fixed bugs involving links
  ! SOURCE

  SUBROUTINE check_for_inverse(rid,properties,src_lib,pcom,report,inv_exist)
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: rid,properties,src_lib
    CHARACTER(LEN=*),INTENT(INOUT):: report
    LOGICAL(KIND=4),INTENT(OUT)   :: inv_exist
    OPTIONAL                      :: inv_exist
    TYPE(reactionparticles)       :: r
    CHARACTER(LEN=MAX_RLIB_LEN)   :: library
    CHARACTER(LEN=MAX_RATE_LEN)   :: inv_props
    CHARACTER(LEN=MAX_RID_LEN)    :: reac_str,unique,unique_reac_str,inv_rid
    CHARACTER(LEN=150)            :: message
    CHARACTER(LEN=10)             :: bib1,bib2
    INTEGER(KIND=4)               :: z,a,rtype,i
    LOGICAL(KIND=4)               :: exist,v_flag,inv_v_flag

    IF (PRESENT(inv_exist)) inv_exist = .FALSE.
    CALL decode_rid(rid,library,z,a,rtype,reac_str,unique,unique_reac_str)

    CALL read_reac_str(r,reac_str,message,unique)
    IF (message /= '') THEN
       report = TRIM(report) // TRIM(unique_reac_str) // ': ' // &
            TRIM(message) // ACHAR(8)
       RETURN
    END IF

    ! See if v flag is in Reaction Type
    message = ',' // TRIM(get_prop_value('Reaction Type',properties, &
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)) // ','
    bib1 = get_prop_value('Biblio Code',properties, &
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
    IF (INDEX(message,',v,') > 0) THEN
       v_flag = .TRUE.
    ELSE
       v_flag = .FALSE.
    END IF

    ! Don't check for inverses for decays
    IF (decay(r)) RETURN

    r = getinverse(r)

    ! Update rtype and unique
    rtype = getreactype(r,unique)
    reac_str = getreac_str(r,1)

    ! Update z and a for new reaction string
    z = -1
    a = -1
    DO i = 1, getreac_num(r)
       IF ((getreac_z(r,i) > z) .OR. &
            (getreac_z(r,i) == z .AND. getreac_a(r,i) > a)) THEN
          z = getreac_z(r,i)
          a = getreac_a(r,i)
       END IF
    END DO

    inv_rid = encode_rid(library,z,a,rtype,reac_str,unique)
    exist = rid_exist(inv_rid,pcom,src_lib,inv_props)
    IF (PRESENT(inv_exist)) inv_exist = exist

    IF (.NOT. exist) then
       ! If missing and a link check for non-linked inverse rate
       IF (src_lib /= library) THEN
          inv_rid = encode_rid(src_lib,z,a,rtype,reac_str,unique)

          IF (rid_exist(inv_rid,pcom)) THEN
!             report = TRIM(report) // 'CAUTION! Inverse rate for ' // &
             report = TRIM(report) // 'Inverse rate for ' // &
                  TRIM(unique_reac_str) // ' (' // TRIM(bib1) // &
                  ') comes from wrong library!' // ACHAR(8)
             RETURN
          END IF
       END IF

       report = TRIM(report) // 'No inverse rate for ' // &
            TRIM(unique_reac_str) // ACHAR(8)
       RETURN
    END IF

    ! Get new unique reaction string
    CALL decode_rid(inv_rid,unique_reac_str=reac_str)

    ! Get v flag for Reaction Type of inverse reaction
    message = ',' // TRIM(get_prop_value('Reaction Type',inv_props, &
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)) // ','
    bib2 = get_prop_value('Biblio Code',inv_props, &
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
    IF (INDEX(message,',v,') > 0) THEN
       inv_v_flag = .TRUE.
    ELSE
       inv_v_flag = .FALSE.
    END IF

    ! Check if biblio codes are the same
    ! (but don't return since this isn't a critical error)
!    IF (bib1 /= bib2) report = TRIM(report) // 'CAUTION! ' // &
    IF (bib1 /= bib2) report = TRIM(report) // &
         TRIM(unique_reac_str) // ' (' // TRIM(bib1) // ') and ' // &
         TRIM(reac_str) // ' (' // TRIM(bib2) // &
            ') have different biblio codes' // &
            ACHAR(8)

    ! Make sure inverse flags for both reactions differ
    IF (v_flag .AND. inv_v_flag) THEN
!       report = TRIM(report) // 'CAUTION! ' // TRIM(unique_reac_str) // &
       report = TRIM(report) // TRIM(unique_reac_str) // &
            ' (' // TRIM(bib1) // ') and ' // TRIM(reac_str) // ' (' // &
            TRIM(bib2) // ') are both listed as inverses' // &
            ACHAR(8)
       RETURN
    END IF
    IF (.NOT. (v_flag .OR. inv_v_flag)) THEN
!       report = TRIM(report) // 'CAUTION! One of ' // &
       report = TRIM(report) // 'One of ' // &
            TRIM(unique_reac_str) // ' (' // TRIM(bib1) // ') or ' // &
            TRIM(reac_str) // ' (' // TRIM(bib2) // &
            ') should be listed as an inverse' // &
            ACHAR(8)
       RETURN
    END IF

    ! Check detailed balance
  END SUBROUTINE check_for_inverse
  !***

  !---------------------------------------------------------------------
  FUNCTION get_inv_props(pcom,props,r,r_inv,rid,inv_rid,reason,a,a_inv,a_num)
    USE inv_parm
    USE constants
    USE reactionstrings
    USE convert
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: props,rid,inv_rid
    TYPE(reactionparticles),INTENT(IN):: r,r_inv
    CHARACTER(LEN=*),INTENT(OUT)     :: reason
    REAL(KIND=8),INTENT(OUT)         :: a(:),a_inv(:)
    INTEGER(KIND=4),INTENT(OUT)      :: a_num
    OPTIONAL                         :: a,a_inv,a_num
    CHARACTER(LEN=MAX_RATE_LEN)      :: get_inv_props
    REAL(KIND=8)                     :: int_a(MAX_A),int_a_inv(MAX_A),del_a(MAX_A)
    INTEGER(KIND=4)                  :: int_a_num,i
    CHARACTER(LEN=400)               :: tmps,tmps2

    reason = ''
    get_inv_props = props
    IF (PRESENT(a)) a = 0.0
    IF (PRESENT(a_inv)) a_inv = 0.0
    IF (PRESENT(a_num)) a_num = -1

    ! Get forward parameters
    CALL get_parm_array(props, int_a, int_a_num)
    IF (int_a_num < 1) reason = 'parameter parse error'

    ! Get inverse parameters
    IF (reason == '') THEN
       CALL get_inv_parm(int_a,int_a_num,int_a_inv,r,del_a,i,reason)
       IF (i /= 0) reason = 'inverse parameter error: ' // reason
    END IF

    IF (reason == '') THEN
       ! Set Reaction String
       CALL decode_rid(inv_rid,reac_str=tmps)
       CALL set_prop_value('Reaction String',tmps, &
            get_inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       ! Set Reaction Type
       i = getreactype(r_inv,tmps)
       IF (tmps == '') THEN
          WRITE(tmps2,'(I0,A)') i,',v'
       ELSE
          WRITE(tmps2,'(I0,2A)') i,',v',tmps
       END IF
       CALL set_prop_value('Reaction Type',tmps2, &
            get_inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       ! Set qvalue
       WRITE(tmps,'(1P,G13.6)') get_qvalue(r_inv)
       tmps = ADJUSTL(tmps)
       !WRITE(*,'(2A)') 'qvalue=',TRIM(tmps)
       CALL set_prop_value('Q-value',tmps, &
            get_inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       ! Set reactant num
       WRITE(tmps,'(I0)') getreac_num(r_inv)
       CALL set_prop_value('Number of Reactants',tmps, &
            get_inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       ! Set product num
       WRITE(tmps,'(I0)') getprod_num(r_inv)
       CALL set_prop_value('Number of Products',tmps, &
            get_inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       ! Set Parameters
       WRITE(tmps2,'(A,I0,A)') '(1P,',int_a_num-1,'(E12.5,'',''),E12.5)'
       WRITE(tmps,tmps2) int_a_inv(:int_a_num)
       CALL set_prop_value('Parameters',tmps, &
            get_inv_props,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       IF (PRESENT(a)) a(:int_a_num) = int_a(:int_a_num)
       IF (PRESENT(a_inv)) a_inv(:int_a_num) = int_a_inv(:int_a_num)
       IF (PRESENT(a_num)) a_num = int_a_num
    END IF

    IF (reason /= '') THEN
       CALL decode_rid(rid,unique_reac_str=tmps)
       reason = 'Unable to calculate inverse rate for "' // &
         TRIM(tmps) // '" reason: ' // reason
    END IF
  END FUNCTION get_inv_props
  !***

  !---------------------------------------------------------------------
END MODULE rate_man
