MODULE rate_man_core
! PURPOSE
!  This module contains fundamental procedures, data types, and buffers for 
!  rate library management.
! COPYRIGHT
!  Copyright 2004 Astrophysics Data Team, Physics Division, 
!  Oak Ridge National Laboratory.  All rights reserved.
!  This program is for internal, private use only.
! SOURCE

  USE IFLPORT
  USE constants
  USE cina_core
  USE convert

  ! By default all procedures and global variables are private
  PRIVATE

  ! Public Library_Lists procedures
  ! PUBLIC :: read_rlib_list, save_rlib_list, add_rlib, rm_rlib
  PUBLIC :: read_rlib_list, add_rlib, rm_rlib
  PUBLIC :: get_rgroup_number, find_rgroup_index, find_rlib_list_index
  PUBLIC :: clear_rlib_list
  ! Public Isotope_Lists procedures
  PUBLIC :: read_rlib_iso_list, save_rlib_iso_list, add_isotope
  PUBLIC :: isotope_exist, get_iso_list_number, find_rlib_iso_list
  PUBLIC :: clear_riso_list
  ! Public Rate_IDs procedures
  PUBLIC :: read_rid_list, save_rid_list, decode_rid, encode_rid
  PUBLIC :: find_rlib_rid_list, get_rid_list_number, correct_rid, rid_exist
  PUBLIC :: make_rid, inv_rid, change_rid
  ! Public Library_Info procedures
  PUBLIC :: read_rlib_info, save_rlib_info
  ! Public Rate_Info procedures
  PUBLIC :: read_rate_info, save_rate_info, open_rate_info, change_rate_info
  PUBLIC :: read_rate_info_no_buf, clear_rate_info, update_rate_prop
  PUBLIC :: save_rate_info_inv, save_rate_info_no_buf
  ! Public Rate_Man_Core_Misc procedures
  PUBLIC :: rate_man_core_ver, get_rlib_path, get_riso_path, get_parm_array
  !Public Rate_Man_Core_Locking procedures
  PUBLIC :: set_rlib_list_unlocked, check_rlib_list_lock

  ! Parameters defining maximums for rate management
  ! Max # of rate groups in buffer
  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_RGROUPS = 3

  ! Max # of libraries in a group
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RLIB_IN_GRP = 10000

  ! Max length of rate group name
  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_RGRP_LEN = 10

  ! Max length of rate library name
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RLIB_LEN = 100

  ! Max # of open rate libraries
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RLIBS = 10000

  ! Max # of rates per library to buffer in rate list
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RATES = 200

  ! Max length of rate id
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RID_LEN = 120

  ! Max length of rate property string
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RATE_LEN = 5120

  ! Max # of rate property pairs in buffer
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RATE_INFO = MAX_RATES

  !----------------------------------------------------------------------
!!$  ! DEVELOPMENT Parameters defining maximums for rate management
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_RGROUPS = 3
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_RLIB_IN_GRP = 50
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_RGRP_LEN = 10
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_RLIB_LEN = 40
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_RLIBS = 5
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RATES = 120
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RID_LEN = 120
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RATE_LEN = 5120
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_RATE_INFO = MAX_RATES
  
  ! Unit numbers used for files in this module
  ! INTEGER(KIND=4),PARAMETER        :: RLIB_LIST_UNIT = 56
  INTEGER(KIND=4),PARAMETER        :: RLIB_INFO_UNIT = 57
  INTEGER(KIND=4),PARAMETER        :: RLIB_ISO_LIST_UNIT = 58
  INTEGER(KIND=4),PARAMETER        :: RLIB_RID_LIST_UNIT = 59
  INTEGER(KIND=4),PARAMETER        :: RLIB_RATE_INFO_UNIT = 55
  INTEGER(KIND=4),PARAMETER        :: RLIB_RATE_INFO_OLD_UNIT = 54
  INTEGER(KIND=4),PARAMETER        :: RLIB_RATE_INFO_UNIT_NO_BUF = 53
  INTEGER(KIND=4),PARAMETER,PUBLIC :: RLIB_NETSU_UNIT = 52
  INTEGER(KIND=4),PARAMETER,PUBLIC :: RLIB_SUNET_UNIT = 51
  INTEGER(KIND=4),PARAMETER,PUBLIC :: RLIB_NETWINV_UNIT = 50
  INTEGER(KIND=4),PARAMETER,PUBLIC :: RLIB_WINVN_UNIT = 49

  ! Deliminators for Rate Library Info Properties
  CHARACTER(LEN=1),PARAMETER,PUBLIC  :: RLIB_INFO_PROP_SEP = ACHAR(9)
  CHARACTER(LEN=1),PARAMETER,PUBLIC  :: RLIB_INFO_VAL_SEP = '='
  CHARACTER(LEN=1),PARAMETER,PUBLIC  :: RATE_INFO_PROP_SEP = ACHAR(9)
  CHARACTER(LEN=3),PARAMETER,PUBLIC  :: RATE_INFO_VAL_SEP = ' = '
  

!***
  !****in* rate_man_core/Library_Lists
  ! PURPOSE
  !  Group procedures and data types for managing rate library lists and
  !  groups into one section
  ! SEE ALSO
  !  DATA TYPES: rlib_list_buffer
  !  
  !  VARIABLES: rlib_list
  !  
  !  PUBLIC PROCEDURES: read_rlib_list, save_rlib_list, add_rlib, rm_rlib
  !                     get_rgroup_number, find_rgroup_index,
  !                     find_rlib_list_index, clear_rlib_list
  !  
  !  Note: rlib_list is a rate_man_core variable of type rlib_list_buffer
  !***

  !****in* rate_man_core/Isotope_Lists
  ! PURPOSE
  !  Group procedures and data types for managing rate library isotope lists
  !  into one section
  ! SEE ALSO
  !  DATA TYPES: riso_list_buffer
  !  
  !  VARIABLES: riso_list
  !  
  !  PUBLIC PROCEDURES: read_rlib_iso_list, save_rlib_iso_list, add_isotope
  !                     isotope_exist, get_iso_list_number, 
  !                     find_rlib_iso_list, clear_riso_list
  !  
  !  PRIVATE PROCEDURES: get_sorted_a_list
  !  
  !  Note: riso_list is a rate_man_core variable of type riso_list_buffer
  !***
  
  !****in* rate_man_core/Rate_IDs
  ! PURPOSE
  !  Group procedures and data types for managing rate IDs into one section
  ! SEE ALSO
  !  DATA TYPES: rid_list_buffer
  !  
  !  VARIABLES: rid_list
  !  
  !  PUBLIC PROCEDURS: read_rid_list, save_rid_list, decode_rid, encode_rid,
  !                    find_rlib_rid_list, get_rid_list_number, correct_rid
  !                    rid_exist, make_rid, inv_rid, change_rid
  !  
  !  PRIVATE PROCEDURES: get_rid_seek
  !  
  !  Note: rid_list is a rate_man_core variable of type rid_list_buffer
  !  save_rid_list should not be called because it is called from 
  !  save_rate_info
  !***

  !****in* rate_man_core/Library_Info
  ! PURPOSE
  !  Group procedures for managing Library Info into one section
  ! SEE ALSO
  !  PUBLIC PROCEDURES: read_rlib_info, save_rlib_info
  !  
  !  Use get_prop_value and set_prop_value from convert module for reading
  !  and modifying rate library info properties.
  !***
  
  !****in* rate_man_core/Rate_Info
  ! PURPOSE
  !  Group procedures for managing Rate Info into one section
  ! SEE ALSO
  !  DATA TYPES: save_rate_info_buffer
  !  
  !  VARIABLES: rate_file
  !  
  !  PUBLIC PROCEDURES: read_rate_info, save_rate_info, open_rate_info, 
  !                     change_rate_info, read_rate_info_no_buf,
  !                     clear_rate_info, update_rate_prop
  !                     save_rate_info_inv, save_rate_info_no_buf
  !  
  !  PRIVATE PROCEDURES: save_rate_info_internal, read_rate_prop,
  !                      find_reac_rate_file, write_rate_info
  !  
  !  Note that rate_file is a rate_man_core variable of type
  !  save_rate_info_buffer.
  !  
  !  Note that open_rate_info and change_rate_info are used before 
  !  save_rate_info for saving multiple rates for one isotope at one time.
  !  save_rate_info_inv is just like save_rate_info except inverse rate is
  !  updated
  !  
  !  Use get_prop_value and set_prop_value from convert module for reading
  !  and modifying rate info properties.
  !***

  !****in* rate_man_core/Rate_Man_Core_Misc
  ! PURPOSE
  !  Group all other core rate management procedures into one section
  ! SEE ALSO
  !  PUBLIC PROCEDURES: rate_man_core_ver, get_rlib_path, get_riso_path
  !                     get_parm_array
  !***

  !****it* Library_Lists/rlib_list_buffer
  ! NAME
  !  TYPE rlib_list_buffer
  ! PURPOSE
  !  Stores lists of libraries for one rate library group
  ! SOURCE
  
  TYPE,PRIVATE                       :: rlib_list_buffer
     ! Nothing outside this module can access these types or components
     PRIVATE 
     ! Holds the library group name
     CHARACTER(LEN=MAX_RGRP_LEN)     :: group = ''

     ! Holds library names
     CHARACTER(LEN=MAX_RLIB_LEN)     :: library(MAX_RLIB_IN_GRP) = ''

     ! Holds the number of libraries
     INTEGER(KIND=2)                 :: num_libraries = -1
  END TYPE rlib_list_buffer
  !***

  !****it* Isotope_Lists/riso_list_buffer
  ! NAME
  !  TYPE riso_list_buffer
  ! PURPOSE
  !  Stores lists of isotopes for one rate library
  ! SOURCE
  
  TYPE,PRIVATE                       :: riso_list_buffer
     ! Nothing outside this module can access these types or components
     ! CAUTION, if KIND=1 then Z_MAX may not be higher than 127
     ! 1st dim is Z, 2nd dim selects the nth isotope, value is atomic mass
     PRIVATE
     ! Library name
     CHARACTER(LEN=MAX_RLIB_LEN)     :: library = ''

     ! isotope list
     INTEGER(KIND=2)                 :: iso_list(0:MAX_Z,MAX_ISO) = -1

     ! T if buffer is same as in isotope file
     LOGICAL(KIND=1)                 :: saved = .TRUE.
  END TYPE riso_list_buffer
  !***

  !****it* Rate_IDs/rid_list_buffer
  ! NAME
  !  TYPE rid_list_buffer
  ! PURPOSE
  !  Stores lists of rates for one isotope in a rate library
  ! SOURCE
  
  TYPE,PRIVATE                       :: rid_list_buffer
     ! Nothing outside this module can access these types or components
     PRIVATE
     ! Library name
     CHARACTER(LEN=MAX_RLIB_LEN)     :: library = ''

     ! Holds Z of isotope
     INTEGER(KIND=2)                 :: z = -1

     ! Holds A of isotope
     INTEGER(KIND=2)                 :: a = -1

     ! Holds number of rate ids
     INTEGER(KIND=2)                 :: rid_num = -1

     ! Holds rate ids
     CHARACTER(LEN=MAX_RID_LEN)      :: rid(MAX_RATES) = ''

     ! File seek position for rate
     INTEGER(KIND=2)                 :: seek_pos(MAX_RATES) = -1
  END TYPE rid_list_buffer
  !***

  !****it* Rate_Info/save_rate_info_buffer
  ! NAME
  !  TYPE save_rate_info_buffer
  ! PURPOSE
  !  Stores info for using save_rate_info subroutine
  ! SOURCE
  
  TYPE,PRIVATE                       :: save_rate_info_buffer
     ! Nothing outside this module can access these types or components
     PRIVATE
     ! Library name
     CHARACTER(LEN=MAX_RLIB_LEN)     :: library = ''

     ! Holds Z of isotope
     INTEGER(KIND=2)                 :: z = -1

     ! Holds A of isotope
     INTEGER(KIND=2)                 :: a = -1

     ! Holds number of rids in buffer
     INTEGER(KIND=2)                 :: rid_num = 0

     ! Holds rate ids
     CHARACTER(LEN=MAX_RID_LEN)      :: rids(MAX_RATE_INFO) = ''

     ! Holds rate properties
     CHARACTER(LEN=MAX_RATE_LEN)     :: prop(MAX_RATE_INFO) = ''
  END TYPE save_rate_info_buffer
  !***

  ! Create buffers global to this module

  !****iv* Library_Lists/rlib_list
  ! NAME
  !  VARIABLE rlib_list
  ! PURPOSE
  !  Store list of libraries for multiple rate library groups
  ! SOURCE
  
  TYPE(rlib_list_buffer)             :: rlib_list(MAX_RGROUPS)
  !***

  !****iv* Isotope_Lists/riso_list
  ! NAME
  !  VARIABLE riso_list
  ! PURPOSE
  !  Store isotope lists for multiple rate libraries
  ! SOURCE
  
  TYPE(riso_list_buffer)             :: riso_list(MAX_RLIBS)
  !***

   !****iv* Rate_IDs/rid_list
  ! NAME
  !  VARIABLE rid_list
  ! PURPOSE
  !  Store rate lists for multiple rate libraries
  ! SOURCE
  
  TYPE(rid_list_buffer)             :: rid_list(MAX_RLIBS)
  !***

  !****iv* Rate_Info/rate_file
  ! NAME
  !  VARIABLE rate_file
  ! PURPOSE
  !  Rate info buffer for modifing an isotope
  ! SOURCE
  
  TYPE(save_rate_info_buffer)        :: rate_file
  !***

CONTAINS

  !-------------------------------------------------------------------------
  !****if* Rate_Man_Core_Misc/rate_man_core_ver
  ! NAME
  !  FUNCTION rate_man_core_ver()
  ! PURPOSE
  !  Return the cvs revision number for this file
  ! STATUS
  !  Complete
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/19/2004 jpscott       Start function
  ! SOURCE

  FUNCTION rate_man_core_ver()
    IMPLICIT NONE
    CHARACTER(LEN=10)           :: rate_man_core_ver
    CHARACTER(LEN=20),PARAMETER :: RATE_MAN_C_VER = '$Revision: 1.1.1.1 $'
    
    rate_man_core_ver = RATE_MAN_C_VER(12:LEN_TRIM(RATE_MAN_C_VER)-2)
    
  END FUNCTION rate_man_core_ver
  !***

  !-------------------------------------------------------------------------
  !****is* Library_Lists/read_rlib_list
  ! NAME
  !  SUBROUTINE read_rlib_list(group,pcom,list_out)
  ! PURPOSE
  !  Load/return the list of rate libraries into rlib_list
  ! STATUS
  !  Complete
  ! USAGE
  !  Call this subroutine to load or return the list of rate libraries for a
  !  rate library group.  The list is loaded from disk if not in memory and
  !  returned in list_out if present.  This subroutine should be called 
  !  anytime the library lists need to be loaded into memory (rlib_list).
  !  It doesn't modify rlib_list if the library group is loaded so no damage
  !  will occur if the same library list is loaded more than once.
  ! INPUTS
  !  group: string with group name to load into rlib_list
  !  pcom: cina_common derived type needed for reporting errors
  ! OUTPUTS
  !  list_out (OPTIONAL): tab separated list of rate libraries for a group
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/19/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE read_rlib_list(group,pcom,list_out)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)  :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: group
    CHARACTER(LEN=*),INTENT(OUT)  :: list_out
    CHARACTER(LEN=MAX_PATH_LEN)   :: line, line_file
    INTEGER(KIND=4)               :: counter, group_index, status, NUM_LINES
    OPTIONAL                      :: list_out
    CHARACTER*256, ALLOCATABLE    :: LIB_LIST_ARRAY(:)

    ! See if library list is in memory
    group_index = find_rgroup_index(group)

    CALL get_rlib_list_as_array(group, pcom, LIB_LIST_ARRAY, NUM_LINES)

    IF (group_index == -1) THEN

       ! Find next available group_index
       group_index = get_rgroup_number() + 1
       IF (group_index > MAX_RGROUPS) CALL report_error('rlib_list ' // &
            'is full','Developer Reminder',pcom,1)

          IF ( (NUM_LINES .EQ. 0) .AND. (group .EQ. 'USER') ) THEN

             IF (PRESENT(list_out)) list_out = ''

             ! Save into rlib_list
             rlib_list(group_index)%num_libraries = 0
             rlib_list(group_index)%group = 'USER'
             rlib_list(group_index)%library = ''
             RETURN

          END IF

       DO counter=1,NUM_LINES
          rlib_list(group_index)%library(counter) = LIB_LIST_ARRAY(counter)
       END DO

       rlib_list(group_index)%num_libraries = counter
       rlib_list(group_index)%group = group

    END IF

    IF (PRESENT(list_out)) THEN
       list_out = ''
       DO counter = 1, rlib_list(group_index)%num_libraries
          list_out = TRIM(list_out) // ACHAR(9) // &
               rlib_list(group_index)%library(counter)
       END DO
       ! Remove initial tab
       list_out = TRIM( list_out(2:) )
    END IF

    DEALLOCATE(LIB_LIST_ARRAY)

  END SUBROUTINE read_rlib_list
  !***

  SUBROUTINE get_rlib_list_as_array(group, pcom, LIB_LIST_ARRAY, NUM_LINES)

    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)                  :: pcom
    CHARACTER(LEN=*),INTENT(IN)                   :: group
    CHARACTER(LEN=256), ALLOCATABLE, INTENT(OUT)  :: LIB_LIST_ARRAY(:)
    CHARACTER(LEN=256)                            :: OUTPUT_FILENAME, OUTPUT_FILEPATH, OUTPUT_PATH, CTMP
    INTEGER                                       :: OUTPUT_FILE_ID = 77
    INTEGER                                       :: I = 0, IERR = 0, LIB_LIST_ARRAY_COUNTER = 0
    INTEGER, INTENT(OUT)                          :: NUM_LINES

    NUM_LINES = 0
    OUTPUT_FILENAME = 'tmp_rate_list.txt'

    SELECT CASE (group)

       CASE ('USER')

            OUTPUT_PATH = TRIM(CINA_PATH) // TRIM(group) // '/' // TRIM(pcom%USER) // '/rate_libs/'
            OUTPUT_FILEPATH = TRIM(OUTPUT_PATH) // TRIM(OUTPUT_FILENAME)

       CASE DEFAULT

            OUTPUT_PATH = TRIM(CINA_PATH) // TRIM(group) // '/rate_libs/'
            OUTPUT_FILEPATH =  TRIM(OUTPUT_PATH) // TRIM(OUTPUT_FILENAME)

    END SELECT

    IERR = SYSTEM('/bin/rm -f ' // TRIM(OUTPUT_FILEPATH))
    IERR = SYSTEM('/bin/ls ' // TRIM(OUTPUT_PATH) // ' > ' // TRIM(OUTPUT_FILEPATH))

    OPEN(UNIT=OUTPUT_FILE_ID, FILE=OUTPUT_FILEPATH)

    DO WHILE (IERR == 0)

        READ(OUTPUT_FILE_ID, *, IOSTAT=IERR) CTMP

        IF ( TRIM(CTMP) .NE. TRIM(OUTPUT_FILENAME) ) THEN

            NUM_LINES = NUM_LINES + 1

        END IF

    END DO

    ALLOCATE(LIB_LIST_ARRAY(NUM_LINES))
    REWIND(OUTPUT_FILE_ID)
    NUM_LINES = NUM_LINES + 1
    LIB_LIST_ARRAY_COUNTER = 1

    DO I = 1, NUM_LINES

        READ(OUTPUT_FILE_ID, '(A)', IOSTAT=IERR) CTMP

        IF ( IERR .NE. 0 ) THEN
            EXIT        
        END IF 

        IF ( TRIM(CTMP) .NE. TRIM(OUTPUT_FILENAME) ) THEN
            LIB_LIST_ARRAY(LIB_LIST_ARRAY_COUNTER) = CTMP
            LIB_LIST_ARRAY_COUNTER = LIB_LIST_ARRAY_COUNTER + 1
        END IF

    END DO

    NUM_LINES = NUM_LINES - 1

    CLOSE(OUTPUT_FILE_ID)

    IERR = SYSTEM('/bin/rm -f ' // TRIM(OUTPUT_FILEPATH))

  END SUBROUTINE

  !-------------------------------------------------------------------------
  !****is* Library_Lists/add_rlib
  ! NAME
  !  SUBROUTINE add_rlib(library,pcom,group,override,mkdir)
  ! PURPOSE
  !  Add rate library to rlib_list and riso_list
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library and isotope lists are loaded (read_rlib_list
  !  read_rlib_iso_list) before calling this subroutine.  Otherwise, saving
  !  rlib_list will erase all existing libraries.  Note that add_rlib does
  !  not call save_rlib_list while rm_rlib does.
  ! INPUTS
  !  library: Name of library to add
  !  pcom: cina_common variable needed for reporting errors
  !  group: Name of rate library group that new library should belong to
  !  override (OPTIONAL): Set to .TRUE. to allow adding non-USER libraries
  !  mkdir (OPTIONAL): Set to .TRUE. to make a directory for new library
  ! OUTPUTS
  !  None. This subroutine returns if successful otherwise reports an error.
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/19/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE add_rlib(library,pcom,group,override,mkdir)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: library,group
    LOGICAL(KIND=4),INTENT(IN)    :: override,mkdir
    INTEGER(KIND=4)               :: lib_index,group_index
    LOGICAL(KIND=4)               :: check
    OPTIONAL                      :: override,mkdir

    IF (PRESENT(override)) THEN
       ! check is T when permissions should be checked
       check = .NOT. override
       ! Log this
       CALL report_error('Override set when adding library "' // &
            TRIM(library) // '" to group ' // TRIM(group),'Warning',pcom,3)
    ELSE
       check = .TRUE.
    END IF

    IF (check) THEN
       ! Check for permissions
       SELECT CASE (group)
       CASE ('USER ')
          ! Do nothing
       CASE DEFAULT
          CALL report_error('New libraries may not be saved in the ' // &
               'rate library group "' // TRIM(group) // '"', &
               'Improper usage',pcom,1)
       END SELECT
    END IF

    group_index = find_rgroup_index(group)

    IF (group_index < 1) THEN
       ! Group is not in rlib_list
       ! Find next empty group_index
       group_index = get_rgroup_number() + 1
       IF (group_index > MAX_RGROUPS) CALL report_error('rlib_list ' // &
            'is too small to hold new group.','Developer Reminder',pcom,1)

       ! Add empty group to rlib_list
       rlib_list(group_index)%group = 'USER'
       rlib_list(group_index)%num_libraries = 0
    END IF

    ! Check if library is already in rlib_list.  If so, skip this section
    CALL find_rlib_list_index(library,LIB_INDEX=lib_index)
    IF (lib_index <= 0) THEN
       ! Add user library entry
       lib_index = rlib_list(group_index)%num_libraries + 1
       IF (lib_index > MAX_RLIB_IN_GRP) CALL report_error( &
            'MAX_RLIB_IN_GRP is too small to hold new library', &
            'Developer reminder',pcom,1)
       rlib_list(group_index)%num_libraries = lib_index
       rlib_list(group_index)%library(lib_index) = library
    END IF
    
    IF (PRESENT(mkdir)) THEN
       IF (mkdir) THEN
          ! Make directory for new library
          SELECT CASE (group)
          CASE ('USER')
             lib_index = safe_shell('/bin/mkdir -p ''' // &
                  TRIM(CINA_PATH) // 'USER/' // TRIM(pcom%USER) // &
                  '/rate_libs/' // TRIM(library) // ''' &> /dev/null')
             IF (lib_index /= 0) CALL report_error('Unable to make ' // &
                  'user directory on server.','External fileio',pcom,1)
          CASE DEFAULT
             CALL report_error('Missing path for making dir for "' // &
                  TRIM(library) // '" library in ' // TRIM(group) // &
                  ' group.','Developer Reminder',pcom,1)
          END SELECT
       END IF
    END IF

    ! Now add entry to riso_list for this library
    ! Check if library is already in riso_list.  If so, return
    lib_index = find_rlib_iso_list(library)
    IF (lib_index >= 1) RETURN

    ! Add user library entry
    lib_index = get_iso_list_number() + 1
    IF (lib_index > MAX_RLIBS)  CALL report_error('riso_list is full' // &
         ' and isotope list for "' // TRIM(library) // '" can not ' // &
         ' be added.','Improper usage',pcom,1)

    riso_list(lib_index)%iso_list = -1
    riso_list(lib_index)%saved = .FALSE.
    riso_list(lib_index)%library = library

  END SUBROUTINE add_rlib
  !***

  !-------------------------------------------------------------------------
  !****is* Library_Lists/rm_rlib
  ! NAME
  !  SUBROUTINE rm_rlib(library,pcom,move_to_trash)
  ! PURPOSE
  !  Remove rate library from rlib_list and riso_list and may move library
  !  to trash.
  ! STATUS
  !  Complete
  ! CAUTION
  !  Moving a library to the trash will overwrite an existing library in the
  !  trash with the same name.
  !  
  !  save_rlib_list is called internally to save the new library list to 
  !  disk.  Note that add_rlib does not call save_rlib_list while rm_rlib
  !  does
  ! INPUTS
  !  library: name of library to remove (or move to trash)
  !  pcom: cina_common variable needed for reporting errors
  !  move_to_trash (OPTIONAL): Set to .FALSE. to prevent moving library to
  !                            trash
  ! OUTPUTS
  !  None. This subroutine returns if successful otherwise reports an error.
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/23/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE rm_rlib(library,pcom,move_to_trash)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: library
    LOGICAL(KIND=4),INTENT(IN)    :: move_to_trash
    CHARACTER(LEN=MAX_RGRP_LEN)   :: group
    CHARACTER(LEN=MAX_PATH_LEN)   :: lib_path
    INTEGER(KIND=4)               :: lib_index,group_index,num_libs,i
    LOGICAL(KIND=4)               :: loop,move
    OPTIONAL                      :: move_to_trash

    ! Get path to library
    lib_path = get_rlib_path(library,pcom%USER,group)
    IF (lib_path == '') CALL report_error('Library "' // TRIM(library) // &
         '" was not found.','Improper usage',pcom,1)

    IF (PRESENT(move_to_trash)) THEN
       move = move_to_trash
    ELSE
       ! If move_to_trash is not specified, default to yes
       move = .TRUE.
    END IF

    ! Check for permissions
    SELECT CASE (group)
    CASE ('USER ')
       ! Do nothing
    CASE DEFAULT
       CALL report_error('Libraries may not be removed from the "' // &
            TRIM(group) // '" rate library group.','Improper usage',pcom,1)
    END SELECT

    ! Get library and group indicies
    CALL find_rlib_list_index(library,group_index=group_index, &
         lib_index=lib_index)
    IF (group_index < 1 .OR. lib_index < 1) CALL report_error( &
         'Error finding indicies for removing library "' // TRIM(library) &
         // '"','Improbable',pcom,1)

    num_libs = rlib_list(group_index)%num_libraries

    ! Remove library from rlib_list
    rlib_list(group_index)%library(lib_index) = ''

    ! Move remaining library entries down one position in rlib_list array
    ! BE SURE TO TEST THIS
    rlib_list(group_index)%library(lib_index : num_libs-1) = &
         rlib_list(group_index)%library(lib_index+1 : num_libs)
    rlib_list(group_index)%num_libraries = num_libs - 1

    ! Clear last entry
    rlib_list(group_index)%library(num_libs) = ''

    ! Check if library is in riso_list.  If so, remove
    lib_index = find_rlib_iso_list(library)
    IF (lib_index >= 1) THEN
       ! Remove library from riso_list

       num_libs = get_iso_list_number()

       ! Move remaining library entries down one position in riso_list array
       ! BE SURE TO TEST THIS
       riso_list(lib_index : num_libs - 1) = &
            riso_list(lib_index + 1 : num_libs)

       ! Then erase last entry in riso_list
       riso_list(num_libs)%library = ''
       riso_list(num_libs)%iso_list = -1
    END IF
    
    ! Save new rate library list
    ! CALL save_rlib_list(group,pcom)

    IF (move) THEN
       ! Make sure trash has directory for user
       i = safe_shell('/bin/mkdir -p ' // TRIM(CINA_PATH) // 'trash/' // &
            TRIM(pcom%USER) // '/rate_libs')
       IF (i /= 0) THEN
          WRITE(lib_path,'(A,I0,A)') 'Error (',i, &
               ') making user''s trash directory'
          CALL report_error(TRIM(lib_path),'External program',pcom,1)
       END IF
                   
       ! Delete any library with the same name in user's trash
       i = safe_shell('/bin/rm -fr ''' // TRIM(CINA_PATH) // 'trash/' // &
            TRIM(pcom%USER) // '/rate_libs/' // TRIM(library) // '''')
       IF (i /= 0) THEN
          WRITE(lib_path,'(A,I0,3A)') 'Error (',i,') erasing rate ' // &
               'library in user''s trash"',TRIM(library),'" to trash'
          CALL report_error(TRIM(lib_path),'External program',pcom,1)
       END IF
                   
       ! Move library to trash
       i = safe_shell('/bin/mv -f ''' // TRIM(lib_path) // ''' ''' // &
            TRIM(CINA_PATH) // 'trash/' // TRIM(pcom%USER) // &
            '/rate_libs''')
       IF (i /= 0) THEN
          WRITE(lib_path,'(A,I0,3A)') 'Error (',i, &
               ') moving rate library "',TRIM(library),'" to trash'
          CALL report_error(TRIM(lib_path),'External program',pcom,1)
       END IF
    END IF
  END SUBROUTINE rm_rlib
  !***

  !-------------------------------------------------------------------------
  !****if* Library_Lists/get_rgroup_number
  ! NAME
  !  FUNCTION get_rgroup_number()
  ! PURPOSE
  !  Return the number of rate library groups in rlib_list
  ! STATUS
  !  Complete
  ! RETURN VALUE
  !  INT(KIND=4) with the number of rate library groups in rlib_list
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/27/2004 jpscott       Start function
  ! SOURCE

  FUNCTION get_rgroup_number()
    IMPLICIT NONE
    INTEGER(KIND=4)                  :: get_rgroup_number

    get_rgroup_number = 0
    DO WHILE (rlib_list(get_rgroup_number + 1)%group /= '')
       get_rgroup_number = get_rgroup_number + 1
       IF (get_rgroup_number > MAX_RGROUPS) RETURN
    END DO
  END FUNCTION get_rgroup_number
  !***

  !-------------------------------------------------------------------------
  !****is* Library_Lists/find_rlib_list_index
  ! NAME
  !  SUBROUTINE find_rlib_list_index(library,group_name,group_index,
  !                                  lib_index)
  ! PURPOSE
  !  Find library in rlib_list and return rate library and group indicies
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) before calling
  !  this subroutine.  This function may be used outside this module to
  !  determine if a rate library is loaded into rlib_list; however, its
  !  value is useless because rlib_list is private to this module.
  !  
  !  This subroutine finds the first library in rlib_list with the correct 
  !  name.  If libraries with the same name are in different groups, only 
  !  indicies to one library will be returned.  Different users may use the
  !  same library name.
  ! INPUTS
  !  library: Library name to find in rlib_list
  ! OUTPUTS
  !  group_index (OPTIONAL): Index into rlib_list to rate library group
  !  group_name (OPTIONAL): Name of rate group library belongs to
  !  lib_index (OPTIONAL): Index into rlib_list to correct rate library
  !  
  !  If library name was not found in rlib_list then group_name will be 
  !  empty and indicies will be -1
  ! EXAMPLES
  !  CALL find_rlib_list_index(library,group_name,group_index,lib_index)
  !  
  !  ! Print group name
  !  print *,group_name
  !  print *,rlib_list(group_index)%group
  !  
  !  ! Print library name
  !  print *,library
  !  print *,rlib_list(group_index)%library(lib_index)
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/20/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE find_rlib_list_index(library,group_name,group_index,lib_index)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: library
    CHARACTER(LEN=*),INTENT(OUT)  :: group_name
    INTEGER(KIND=4),INTENT(OUT)   :: lib_index,group_index
    INTEGER(KIND=4)               :: grp,lib
    OPTIONAL                      :: group_name,group_index,lib_index

    ! Set return values if error occurs
    IF (PRESENT(group_name)) group_name = ''
    IF (PRESENT(group_index)) group_index = -1
    IF (PRESENT(lib_index)) lib_index = -1

    ! Just in case library is empty, exit with error
    IF (library == '') RETURN

    ! Loop through all rate library groups
    DO grp = 1, MAX_RGROUPS
       IF (rlib_list(grp)%group /= '') THEN

          ! Loop through all libraries in group
          DO lib = 1, rlib_list(grp)%num_libraries

             IF (rlib_list(grp)%library(lib) == library) THEN
                IF (PRESENT(group_name)) group_name = &
                     rlib_list(grp)%group
                IF (PRESENT(group_index)) group_index = grp
                IF (PRESENT(lib_index)) lib_index = lib
                RETURN
             END IF
          END DO
       ELSE
          ! All rate library groups have been searched so exit with error
          RETURN
       END IF
    END DO

    ! Library not found so exit with error
  END SUBROUTINE find_rlib_list_index
  !***

  !-------------------------------------------------------------------------
  !****is* Library_Lists/clear_rlib_list
  ! NAME
  !  SUBROUTINE clear_rlib_list()
  ! PURPOSE
  !  Empty rlib_list buffer
  ! STATUS
  !  Complete
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  10/28/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE clear_rlib_list()
    INTEGER(KIND=4)               :: i

    DO i = 1, MAX_RGROUPS
       rlib_list(i)%group = ''
       rlib_list(i)%library = ''
       rlib_list(i)%num_libraries = -1
    END DO
  END SUBROUTINE clear_rlib_list
  !***

  !-------------------------------------------------------------------------
  !****if* Library_Lists/find_rgroup_index
  ! NAME
  !  FUNCTION find_rgroup_index(group)
  ! PURPOSE
  !  Return rate library group index in rlib_list for a group name
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) before calling
  !  this subroutine.  This function may be used outside this module to 
  !  determine if a rate library group is loaded into rlib_list; however,
  !  its value is useless because rlib_list is private to this module.
  ! INPUTS
  !  group: Name of rate library group to find
  ! RETURN VALUE
  !  INT(KIND=4) of index into rlib_list for rate library group.  If group 
  !  name is not found, -1 will be returned.
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/20/2004 jpscott       Start function
  ! SOURCE

  FUNCTION find_rgroup_index(group)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: group
    INTEGER(KIND=4)               :: find_rgroup_index,i

    ! Default value in case group is not found
    find_rgroup_index = -1

    DO i = 1, MAX_RGROUPS
       IF (rlib_list(i)%group == '') THEN
          RETURN
       ELSE IF (rlib_list(i)%group == group) THEN
          find_rgroup_index = i
          RETURN
       END IF
    END DO
  END FUNCTION find_rgroup_index
  !***

  !-------------------------------------------------------------------------
  !****is* Isotope_Lists/read_rlib_iso_list
  ! NAME
  !  SUBROUTINE read_rlib_iso_list(library,pcom,iso_list)
  ! PURPOSE
  !  Load/return the isotope list for a rate library into riso_list
  ! STATUS
  !  Complete
  ! INPUTS
  !  library: Name of library to retrieve isotope list for
  !  pcom: cina_common variable needed for reporting errors
  ! OUTPUTS
  !  iso_list (OPTIONAL): array containing isotope list for library
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/23/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE read_rlib_iso_list(library,pcom,iso_list)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: library
    INTEGER(KIND=2),INTENT(OUT)   :: iso_list(0:MAX_Z,MAX_ISO)
    CHARACTER(LEN=5000)           :: line
    CHARACTER(LEN=5)              :: a_str
    INTEGER(KIND=4)               :: lib_index,status,counter,tab_index,z,a
    LOGICAL(KIND=4)               :: loop
    OPTIONAL                      :: iso_list

    ! Find library index into riso_list
    lib_index = find_rlib_iso_list(library)

    IF (lib_index < 1) THEN
       ! library is not is riso_list
       
       line = get_rlib_path(library,pcom%USER)
       IF (line == '') CALL report_error('Library "' // TRIM(library) // &
            '" was not found.','Improper usage',pcom,1)

       ! Get next empty lib_index in riso_list
       lib_index = get_iso_list_number() + 1
       IF (lib_index > MAX_RLIBS) CALL report_error('riso_list is full' // &
            ' and isotope list for "' // TRIM(library) // '" can not ' // &
            ' be loaded.','Improper usage',pcom,1)

       OPEN(RLIB_ISO_LIST_UNIT, FILE=TRIM(line) // 'Rate_Isotopes', &
            IOSTAT=status, ACTION='READ')

       IF (status /= 0) CALL report_error('Error opening rate isotope ' // &
            'list for library ' // TRIM(library),'File input/output',pcom,1)

       riso_list(lib_index)%iso_list = -1

       DO WHILE (status == 0)
          READ(RLIB_ISO_LIST_UNIT,'(A)',IOSTAT=status) line

          IF (status > 0) THEN
             CALL report_error('Error reading rate isotope list for ' // &
                  'library ' // TRIM(library),'File input/output',pcom,1)

          ELSE IF (status == 0) THEN
             counter = INDEX(line,'=')
             IF (counter > 0) THEN
                READ(line(1:counter-1),'(I)') z
                IF (z > MAX_Z) CALL report_error('riso_list(1) is too ' // &
                     'small','Developer Reminder',pcom,1)

                ! Now that z is found, read tab separated list of masses
                line = line(counter+1:)
                counter = 0
                loop = (line /= '')
                DO WHILE (loop)
                   loop = next_in_list(a_str,line,ACHAR(9))
                   READ(a_str,'(I)') a
                   counter = counter + 1
                   IF (counter > MAX_ISO) CALL report_error('riso_list' // &
                        '(2) is too small','Developer Reminder',pcom,1)

                   riso_list(lib_index)%iso_list(z,counter) = a
                END DO
             END IF
          END IF
       END DO
       CLOSE(RLIB_ISO_LIST_UNIT)

       riso_list(lib_index)%library = library
       riso_list(lib_index)%saved = .TRUE.
    END IF

    ! Return iso_list
    IF (PRESENT(iso_list)) iso_list = riso_list(lib_index)%iso_list

  END SUBROUTINE read_rlib_iso_list
  !***

  !-------------------------------------------------------------------------
  !****is* Isotope_Lists/save_rlib_iso_list
  ! NAME
  !  SUBROUTINE save_rlib_iso_list(library,pcom,path,force_save)
  ! PURPOSE
  !  Save isotope list for a library to disk
  ! STATUS
  !  Complete
  ! CAUTION
  !  This subroutine returns without saving if isotope list on disk is same
  !  as one in riso_list unless force_save is TRUE.
  ! INPUTS
  !  library: Name of library to save isotope list for
  !  pcom: cina_common variable needed for reporting errors
  !  path (OPTIONAL): need for saving isotope list outside of suite
  !  force_save (OPTIONAL): set to T to force saving of isotope list
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/24/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE save_rlib_iso_list(library,pcom,path,force_save)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: library,path
    LOGICAL(KIND=4),INTENT(IN)       :: force_save
    INTEGER(KIND=4)                  :: lib_index,status,z
    CHARACTER(LEN=MAX_PATH_LEN)      :: r_path
    CHARACTER(LEN=5000)              :: a_list
    LOGICAL(KIND=4)                  :: fsave
    OPTIONAL                         :: path,force_save

    fsave = .FALSE.
    IF (PRESENT(force_save)) fsave = force_save

    lib_index = find_rlib_iso_list(library)
    IF (lib_index < 1) CALL report_error('Library "' // TRIM(library) // &
         '" not found in riso_list when trying to save isotope list.', &
         'Improper usage',pcom,1)

    ! Return if saved already
    IF (riso_list(lib_index)%saved .AND. .NOT. fsave) RETURN

    IF (PRESENT(path)) THEN
       r_path = path
    ELSE
       r_path = get_rlib_path(library,pcom%USER)
    END IF

    OPEN(RLIB_ISO_LIST_UNIT, FILE=TRIM(r_path) // 'Rate_Isotopes', &
         IOSTAT=status, ACTION='WRITE')
    IF (status /= 0) CALL report_error('Error opening isotope list for' // &
         ' library ' // TRIM(library),'File input/output',pcom,1)

    DO z = 0, MAX_Z
       a_list = get_sorted_a_list(lib_index,z)

       IF (a_list /= '') WRITE(RLIB_ISO_LIST_UNIT,'(I0,2A)',IOSTAT=status) &
            z,'=',TRIM(a_list)
    END DO

    CLOSE(RLIB_ISO_LIST_UNIT)
    riso_list(lib_index)%saved = .TRUE.
  END SUBROUTINE save_rlib_iso_list
  !***

  !-------------------------------------------------------------------------
  !****is* Isotope_Lists/add_isotope
  ! NAME
  !  SUBROUTINE add_isotope(library,z,a)
  ! PURPOSE
  !  Add isotope to riso_list for a library
  ! STATUS
  !  Complete
  ! CAUTION
  !  Isotope list should be loaded before calling this subroutine, otherwise
  !  this subroutine simply returns.  It also returns without changing 
  !  riso_list if MAX_ISO is too small to hold new isotope.
  ! INPUTS
  !  library: Name of library to add isotope to
  !  z: Number of protons of isotope to add
  !  a: Number of protons and neutrons (atomic mass) of isotope to add
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/23/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE add_isotope(library,z,a)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: library
    INTEGER(KIND=4),INTENT(IN)       :: z,a
    INTEGER(KIND=4)                  :: lib_index,m,i

    ! Check for valid z
    IF (z > MAX_Z .OR. z < 0) RETURN

    ! Get library index i riso_list, return if library not found
    lib_index = find_rlib_iso_list(library)
    IF (lib_index < 1) RETURN
    
    ! Find index to next empty value in iso_list, return if isotope exists
    m = riso_list(lib_index)%iso_list(z,1)
    i = 1  ! counter through iso_list
    DO WHILE (m > 0)
       IF (m == a) THEN
          RETURN
       END IF
       i = i + 1
       IF (i > MAX_ISO) RETURN
       m = riso_list(lib_index)%iso_list(z,i)
    END DO
    
    ! Add isotope
    riso_list(lib_index)%iso_list(z,i) = a
    riso_list(lib_index)%saved = .FALSE.
  END SUBROUTINE add_isotope
  !***

  !-------------------------------------------------------------------------
  !****if* Isotope_Lists/isotope_exist
  ! NAME
  !  FUNCTION isotope_exist(library,z,a,z_exist)
  ! PURPOSE
  !  Check if isotope exists in library
  ! STATUS
  !  Complete
  ! CAUTION
  !  Isotope list must be loaded for library beforehand
  ! INPUTS
  !  library: Name of library to check if isotope exists in 
  !  z: Number of protons of isotope
  !  a: Number of protons and neutrons of isotope (atomic mass)
  ! OUTPUTS
  !  z_exist (OPTIONAL): .TRUE. if at least one isotope with value of z
  ! RETURN VALUE
  !  .TRUE. or .FALSE. LOGICAL(KIND=4) if isotope exists
  !  .FALSE. is returned if library was not found
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/23/2004 jpscott       Start function
  ! SOURCE

  FUNCTION isotope_exist(library,z,a,z_exist)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: library
    INTEGER(KIND=4),INTENT(IN)       :: z,a
    LOGICAL(KIND=4),INTENT(OUT)      :: z_exist
    LOGICAL(KIND=4)                  :: isotope_exist
    INTEGER(KIND=4)                  :: lib_index,i,m
    OPTIONAL                         :: z_exist

    isotope_exist = .FALSE.
    IF (PRESENT(z_exist)) z_exist = .FALSE.

    ! Check for valid z
    IF (z > MAX_Z .OR. z < 0) RETURN

    ! Get library index i riso_list, return if library not found
    lib_index = find_rlib_iso_list(library)
    IF (lib_index < 1) RETURN

    m = riso_list(lib_index)%iso_list(z,1)
    i = 1  ! counter through iso_list
    IF (PRESENT(z_exist) .AND. (m > 0)) z_exist = .TRUE.
    DO WHILE (m > 0)
       IF (m == a) THEN
          isotope_exist = .TRUE.
          RETURN
       END IF
       i = i + 1
       IF (i > MAX_ISO) RETURN
       m = riso_list(lib_index)%iso_list(z,i)
    END DO
  END FUNCTION isotope_exist
  !***

  !-------------------------------------------------------------------------
  !****if* Isotope_Lists/get_iso_list_number
  ! NAME
  !  FUNCTION get_iso_list_number()
  ! PURPOSE
  !  Return number of isotope lists in riso_list
  ! STATUS
  !  Complete
  ! RETURN VALUE
  !  INT(KIND=4) number of isotope lists in riso_list
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/24/2004 jpscott       Start subroutine
  ! SOURCE

  FUNCTION get_iso_list_number()
    IMPLICIT NONE
    INTEGER(KIND=4)               :: get_iso_list_number

    get_iso_list_number = 0
    DO WHILE (riso_list(get_iso_list_number+1)%library /= '')
       get_iso_list_number = get_iso_list_number + 1
       IF (get_iso_list_number >= MAX_RLIBS) RETURN
    END DO
  END FUNCTION get_iso_list_number
  !***

  !-------------------------------------------------------------------------
  !****if* Isotope_Lists/find_rlib_iso_list
  ! NAME
  !  FUNCTION find_rlib_iso_list(library)
  ! PURPOSE
  !  Return index in riso_list for a library
  ! STATUS
  !  Complete
  ! CAUTION
  !  This function may be used to determine if a rate library isotope list
  !  is loaded into riso_list; however, its value is useless outside this
  !  module because riso_list is private to this module.
  ! INPUTS
  !  library: Name of library to find in riso_list
  ! RETURN VALUE
  !  INT(KIND=4) index in riso_list for a library if library is found,
  !  otherwise -1 is returned
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/24/2004 jpscott       Start subroutine
  ! SOURCE

  FUNCTION find_rlib_iso_list(library)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: library
    INTEGER(KIND=4)               :: find_rlib_iso_list,i

    ! Default value to return if library is not found
    find_rlib_iso_list = -1

    DO i = 1, MAX_RLIBS
       IF (riso_list(i)%library == '') THEN
          ! Library not found
          RETURN
       ELSE IF (riso_list(i)%library == library) THEN
          find_rlib_iso_list = i
          RETURN
       END IF
    END DO
  END FUNCTION find_rlib_iso_list
  !***

  !-------------------------------------------------------------------------
  !****is* Isotope_Lists/clear_riso_list
  ! NAME
  !  SUBROUTINE clear_riso_list()
  ! PURPOSE
  !  Empty riso_list buffer
  ! STATUS
  !  Complete
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  10/27/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE clear_riso_list()
    INTEGER(KIND=4)               :: i

    DO i = 1, MAX_RLIBS
       riso_list(i)%library = ''
       riso_list(i)%iso_list = -1
       riso_list(i)%saved = .TRUE.
    END DO
  END SUBROUTINE clear_riso_list
  !***

  !-------------------------------------------------------------------------
  !****if* Isotope_Lists/get_sorted_a_list
  ! NAME
  !  FUNCTION get_sorted_a_list(lib_index,z)
  ! PURPOSE
  !  Return tab separated list of sorted a values for a given z in a library
  ! STATUS
  !  Complete
  ! CAUTION
  !  This function is not efficient
  ! INPUTS
  !  lib_index: Index to library in riso_list to use for sorting
  !  z: Value of z in library to sort atomic mass values for
  ! RETURN VALUE
  !  CHARACTER(LEN=5000) of tab separated of list of atomic mass values
  !  sorted from lowest to highest for a given z in a library
  ! TODO
  !  Replace sorting algorithm with more efficient one
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/19/2004 jpscott       Start function
  ! SOURCE
  
  FUNCTION get_sorted_a_list(lib_index,z)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)       :: lib_index,z
    CHARACTER(LEN=5000)              :: get_sorted_a_list
    INTEGER(KIND=4)                  :: a_count,a_min,a_min_old
    INTEGER(KIND=4)                  :: sorted_num,i,m
    LOGICAL(KIND=4)                  :: header

    ! Count the number of isotopes
    a_count = 0
    DO WHILE (riso_list(lib_index)%iso_list(z,a_count + 1) > 0)
       a_count = a_count + 1
    END DO

    a_min_old = 0
    get_sorted_a_list = ''
    header = .FALSE.
    
    ! sorted_num is the nth sorted isotope
    DO sorted_num = 1, a_count
       a_min = HUGE(a_min)
       
       ! i is a counter to find the next smallest value of a
       DO i = 1, a_count
          m = riso_list(lib_index)%iso_list(z,i)
          IF (m < a_min .AND. m > a_min_old) a_min = m
       END DO

       ! add a_min to return value
       IF (header) THEN
          WRITE(get_sorted_a_list,'(2A,I0)') TRIM(get_sorted_a_list), &
               ACHAR(9),a_min
       ELSE
          WRITE(get_sorted_a_list,'(I0)') a_min
          header = .TRUE.
       END IF

       a_min_old = a_min
    END DO
  END FUNCTION get_sorted_a_list
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_IDs/read_rid_list
  ! NAME
  !  SUBROUTINE read_rid_list(library,pcom,z,a,rids,num_rids,seek_pos,
  !                           buffer)
  ! PURPOSE
  !  Load/return list of rate IDs for an isotope of a rate library
  ! STATUS
  !  Complete
  ! CAUTION
  !  save_rid_list requires that rate IDs be in rid_list.  buffer should
  !  only be set to .FALSE. for situations where rate lists will not be 
  !  modified.  An example is when inverse parameters are needed for a rate
  !  but the non-inverse rate should remain in the buffers.
  !  
  !  Make sure rate library lists are loaded (read_rlib_list) before calling
  !  this subroutine.
  ! INPUTS
  !  library: Name of library to load rate ID list for
  !  pcom: cina_common variable needed for reporting errors
  !  z: Number of protons of isotope to load rate ID list for
  !  a: Atomic mass of isotope to load rate ID list for
  !  buffer (OPTIONAL): Set to .FALSE. when rate IDs should not be loaded
  !                     into rid_list
  ! OUTPUTS
  !  rids (OPTIONAL): character array of rate IDs for selected isotope
  !  num_rates (OPTIONAL): number of rate IDs for selected isotope
  !  seek_pos (OPTIONAL): int array of seek positions for getting rate info
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/27/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE read_rid_list(library,pcom,z,a,rids,num_rids,seek_pos, &
       buffer)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: library
    INTEGER(KIND=4),INTENT(IN)       :: z,a
    LOGICAL(KIND=4),INTENT(IN)       :: buffer
    CHARACTER(LEN=MAX_RID_LEN),INTENT(OUT) :: rids(:)
    INTEGER(KIND=4),INTENT(OUT)      :: num_rids,seek_pos(:)
    OPTIONAL                         :: rids,num_rids,seek_pos,buffer
    
    TYPE(rid_list_buffer)            :: local_rids
    LOGICAL(KIND=4)                  :: load_from_file = .FALSE.
    INTEGER(KIND=4)                  :: lib_index,status,i
    CHARACTER(LEN=MAX_PATH_LEN)      :: path
    

    ! Get index to library in rid_list
    lib_index = find_rlib_rid_list(library)
    
    ! Load rid list from file if 1) wrong isotope is in rid_list or 
    ! 2) library is not in rid_list
    ! Then set lib_index
    IF (lib_index > 0) THEN
       IF (rid_list(lib_index)%z /= z .OR. rid_list(lib_index)%a /= a) &
            load_from_file = .TRUE.
    ELSE IF (lib_index == -1) THEN
       load_from_file = .TRUE.

       lib_index = get_rid_list_number() + 1
       IF (lib_index > MAX_RLIBS) CALL report_error('rid_list is too ' // &
            'small to hold rid list for "' // TRIM(library) // '" library',&
            'Developer Reminder',pcom,1)
    END IF

    ! Load rid list from file or memory into local_rids
    IF (load_from_file) THEN
       path = get_riso_path(library,z,a,'rid',pcom%USER)
       IF (path == '') CALL report_error('Library "' // TRIM(library) // &
            '" was not found when obtaining path.','Improper usage',pcom,1)

       OPEN(RLIB_RID_LIST_UNIT, FILE=TRIM(path), IOSTAT=status, &
            ACTION='READ')

       IF (status /= 0) CALL report_error('Can not open rate list for ' // &
            TRIM(path),'File input/output',pcom,1)

       ! Read in the number of rates
       READ(RLIB_RID_LIST_UNIT,'(I)',IOSTAT=status) local_rids%rid_num
       IF (status /= 0) CALL report_error('Can not read in number of ' // &
            'rates for ' // TRIM(path),'File input/output',pcom,1)

       ! If all rates won't fit in buffer
       IF (local_rids%rid_num > MAX_RATES) CALL report_error('Isotope ' // &
            'has too many rates to fit in buffer for ' // TRIM(path), &
            'Improbable',pcom,1)

       ! Read in rid list into rid_list
       DO i = 1, local_rids%rid_num
          READ(RLIB_RID_LIST_UNIT,'(I9,A)',IOSTAT=status) &
               local_rids%seek_pos(i),local_rids%rid(i)
          IF (status > 0) CALL report_error('Can not read rate list ' // &
               'for ' // TRIM(path),'File input/output',pcom,1)
       END DO
       CLOSE(RLIB_RID_LIST_UNIT)

       local_rids%z = z
       local_rids%a = a
       local_rids%library = library
    ELSE
       ! Copy rid_list entry into local_rids
       local_rids = rid_list(lib_index)
    END IF

    ! Now local_rids has rate ID lists

    ! Use load_from_file as a temp variable that is TRUE when local_rids
    ! should be saved to rid_list
    IF (PRESENT(buffer)) THEN
       IF (.NOT. buffer) load_from_file = .FALSE.
    END IF

    ! Save local_rids to rid_list if buffer (load_from_file) is .TRUE.
    IF (load_from_file) rid_list(lib_index) = local_rids

    ! Now return optional output
    IF (PRESENT(num_rids)) num_rids = local_rids%rid_num

    IF (PRESENT(rids)) THEN
       IF (UBOUND(rids,1) < local_rids%rid_num) THEN
          CALL report_error('rid_list is too small to hold full list', &
               'Improbable',pcom,0)
          local_rids%rid_num = UBOUND(rids,1)
       END IF

       rids = local_rids%rid(:local_rids%rid_num)
    END IF

    IF (PRESENT(seek_pos)) THEN
       IF (UBOUND(seek_pos,1) < local_rids%rid_num) THEN
          CALL report_error('seek_pos is too small to hold full list', &
               'Improbable',pcom,0)
          local_rids%rid_num = UBOUND(seek_pos,1)
       END IF

       seek_pos = local_rids%seek_pos(:local_rids%rid_num)
    END IF
  END SUBROUTINE read_rid_list
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_IDs/save_rid_list
  ! NAME
  !  SUBROUTINE save_rid_list(library,pcom,z,a,path)
  ! PURPOSE
  !  Save list of rate IDs for an isotope of a rate library to disk
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) before calling
  !  this subroutine.
  ! INPUTS
  !  library: Name of library to save rate ID list for
  !  pcom: cina_common variable needed for reporting errors
  !  z: Number of protons of isotope to save rate ID list for
  !  a: Atomic mass of isotope to save rate ID list for
  !  path (OPTIONAL): path to library directory if saving rid list outside 
  !                   of suite
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE save_rid_list(library,pcom,z,a,path)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: library,path
    INTEGER(KIND=4),INTENT(IN)       :: z,a
    OPTIONAL                         :: path

    CHARACTER(LEN=MAX_PATH_LEN)      :: lib_path
    INTEGER(KIND=4)                  :: status,lib_index,rid_num,i

    ! Get index to library in rid_list
    lib_index = find_rlib_rid_list(library)
    IF (lib_index < 0) CALL report_error('Can not save rid list for "' // &
         TRIM(library) // '" that is not in buffer.','Improbable',pcom,1)

    ! Check that z and a are correct
    IF (rid_list(lib_index)%z /= z .OR. rid_list(lib_index)%a /= a) THEN
       WRITE(lib_path,'(4(A,I0),3A)') 'Isotope in rid_list (', &
            rid_list(lib_index)%z,',',rid_list(lib_index)%a, &
            ') does not match isotope (',z,',',a, &
            ') that should be saved for the "',TRIM(library),'" library'
       CALL report_error(TRIM(lib_path),'Improbable',pcom,1)
    END IF

    ! Get UNIX path to rid list file
    IF (PRESENT(path)) THEN
       lib_path = get_riso_path(library,z,a,'rid',pcom%USER,path)
    ELSE
       lib_path = get_riso_path(library,z,a,'rid',pcom%USER)
    END IF

    OPEN(RLIB_RID_LIST_UNIT, FILE=lib_path, IOSTAT=status, ACTION='WRITE')
    IF (status /= 0) CALL report_error('Can not create rate list ' // &
         TRIM(lib_path),'File input/output',pcom,1)
    
    rid_num = rid_list(lib_index)%rid_num

    ! Write rate_count to file
    WRITE(RLIB_RID_LIST_UNIT,'(I0)',IOSTAT=status) rid_num
    IF (status /= 0) CALL report_error('Can not write rate count to list', &
         'File input/output',pcom,1)

    ! Save file seek positions and rids to file
    DO i = 1, rid_num
       WRITE(RLIB_RID_LIST_UNIT,'(I9,A)',IOSTAT=status) &
            rid_list(lib_index)%seek_pos(i), &
            TRIM(rid_list(lib_index)%rid(i))
       IF (status /= 0) CALL report_error('Can not write rate list', &
            'File input/output',pcom,1)
    END DO

    CLOSE(RLIB_RID_LIST_UNIT)
  END SUBROUTINE save_rid_list
  !***

  !****is* Rate_IDs/decode_rid
  ! NAME
  !  SUBROUTINE decode_rid(rid,library,z,a,rtype,reac_str,unique,
  !                        unique_reac_str)
  ! PURPOSE
  !  Extract information from a rate id
  ! STATUS
  !  Complete
  ! INPUTS
  !  rid: rate id to extract information from
  ! OUTPUTS
  !  library (OPTIONAL): name of library rate id references
  !  z (OPTIONAL): number of protons of heaviest reactant in reaction
  !  a (OPTINOAL): atomic mass of heaviest reactant in reaction
  !  rtype (OPTIONAL): type of reaction (number between 1 and 8)
  !  reac_str (OPTIONAL): reaction string
  !  unique (OPTIONAL): unique identifier for atypical reactions
  !  unique_reac_str (OPTIONAL): reaction string with unique identifier in
  !                              brackets following the reaction string
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/19/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE decode_rid(rid,library,z,a,rtype,reac_str,unique, &
       unique_reac_str,success)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: rid
    CHARACTER(LEN=*),INTENT(OUT)  :: library,reac_str,unique,unique_reac_str
    INTEGER(KIND=4),INTENT(OUT)   :: z,a,rtype
    INTEGER(KIND=4)               :: tab,vtab
    LOGICAL(KIND=4),INTENT(OUT)   :: success
    OPTIONAL                      :: library,z,a,rtype,reac_str,unique
    OPTIONAL                      :: unique_reac_str,success

    tab = INDEX(rid,ACHAR(9))
    vtab = INDEX(rid,ACHAR(11))
    IF (PRESENT(success)) success = .FALSE.

    ! Check for invalid rate id
    IF (tab < 9 .OR. vtab <= tab) THEN
       IF (PRESENT(rtype)) rtype = -10
       IF (PRESENT(z)) z = -10
       IF (PRESENT(a)) a = -10
       IF (PRESENT(library)) library = 'Invalid rid'
       IF (PRESENT(reac_str)) reac_str = 'Invalid rid'
       IF (PRESENT(unique)) unique = 'Invalid rid'
       IF (PRESENT(unique_reac_str)) unique_reac_str = 'Invalid rid'
       RETURN
    END IF

    IF (PRESENT(rtype)) READ(rid(1:2),'(I)') rtype
    IF (PRESENT(z)) READ(rid(3:5),'(I)') z
    IF (PRESENT(a)) READ(rid(6:8),'(I)') a
    IF (PRESENT(library)) library = rid(9:tab-1)
    IF (PRESENT(reac_str)) reac_str = rid(tab+1:vtab-1)
    IF (PRESENT(unique)) unique = rid(vtab+1:)
    IF (PRESENT(unique_reac_str)) THEN
       IF (rid(vtab+1:) == '') THEN
          unique_reac_str = rid(tab+1:vtab-1)
       ELSE
          unique_reac_str = rid(tab+1:vtab-1) // ' [' // &
               TRIM(rid(vtab+1:)) // ']'
       END IF
    END IF

    IF (PRESENT(success)) success = .TRUE.
  END SUBROUTINE decode_rid
  !***

  !****if* Rate_IDs/encode_rid
  ! NAME
  !  FUNCTION encode_rid(library,z,a,rtype,reac_str,unique)
  ! PURPOSE
  !  Create rate id
  ! STATUS
  !  Complete
  ! INPUTS
  !  library: name of library rid refers to
  !  z: number of protons of heaviest reactant in reaction
  !  a: atomic mass of heaviest reactant in reaction
  !  rtype: type of reaction (number between 1 and 8)
  !  reac_str: reaction string
  !  unique: unique identifier for atypical reactions
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start function
  ! SOURCE

  FUNCTION encode_rid(library,z,a,rtype,reac_str,unique)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: library,reac_str,unique
    INTEGER(KIND=4),INTENT(IN)    :: z,a,rtype
    CHARACTER(LEN=MAX_RID_LEN)    :: encode_rid

    WRITE(encode_rid,'(I2.2,2I3.3,5A)') rtype,z,a,TRIM(library),ACHAR(9), &
         TRIM(reac_str),ACHAR(11),TRIM(unique)
  END FUNCTION encode_rid
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_IDs/find_rlib_rid_list
  ! NAME
  !  FUNCTION find_rlib_rid_list(library)
  ! PURPOSE
  !  Return index in rid_list for a library
  ! STATUS
  !  Complete
  ! INPUTS
  !  library: Name of library to find in rid_list
  ! RETURN VALUE
  !  INT(KIND=4) index in rid_list for a library if library is found,
  !  otherwise -1 is returned
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start function
  ! SOURCE

  FUNCTION find_rlib_rid_list(library)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: library
    INTEGER(KIND=4)                  :: find_rlib_rid_list,i

    ! Default value to return if library is not found
    find_rlib_rid_list = -1

    DO i = 1, MAX_RLIBS
       IF (rid_list(i)%library == '') THEN
          ! Library is not found
          RETURN
       ELSE IF (rid_list(i)%library == library) THEN
          find_rlib_rid_list = i
          RETURN
       END IF
    END DO
  END FUNCTION find_rlib_rid_list
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_IDs/get_rid_list_number
  ! NAME
  !  FUNCTION get_rid_list_number()
  ! PURPOSE
  !  Return number of rate ID lists (1 list per library) in rid_list
  ! STATUS
  !  Complete
  ! RETURN VALUE
  !  INT(KIND=4) of number of rate ID lists in rid_list
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/20/2004 jpscott       Start function
  ! SOURCE

  FUNCTION get_rid_list_number()
    IMPLICIT NONE
    INTEGER(KIND=4)                  :: get_rid_list_number

    get_rid_list_number = 0
    DO WHILE (rid_list(get_rid_list_number + 1)%library /= '')
       get_rid_list_number = get_rid_list_number + 1
       IF (get_rid_list_number > MAX_RLIBS) RETURN
    END DO
  END FUNCTION get_rid_list_number
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_IDs/correct_rid
  ! NAME
  !  SUBROUTINE correct_rid(rid,library,link)
  ! PURPOSE
  !  Correct rate id so that links only point to PUBLIC libraries
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) before calling
  !  this subroutine.
  ! INPUTS
  !  rid: rate id to correct
  !  library: Name of library rid will be saved to
  ! OUTPUTS
  !  rid: corrected rate id
  !  link (OPTIONAL): .TRUE. is rid is a link to a PUBLIC library
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE correct_rid(rid,library,link)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT)   :: rid
    CHARACTER(LEN=*),INTENT(IN)      :: library
    LOGICAL(KIND=4),INTENT(OUT)      :: link
    OPTIONAL                         :: link

    INTEGER(KIND=4)                  :: tab
    CHARACTER(LEN=MAX_RLIB_LEN)      :: rid_lib
    CHARACTER(LEN=MAX_RGRP_LEN)      :: rid_group

    IF (PRESENT(link)) link = .FALSE.

    tab = INDEX(rid,ACHAR(9))
    rid_lib = rid(9:tab-1)

    ! Return if library pointed to in rate id is the same as the library 
    ! name it will be stored in
    IF (rid_lib == library) RETURN

    ! Get rate id library's group
    CALL find_rlib_list_index(rid_lib,rid_group)

    SELECT CASE (rid_group)
    CASE ('PUBLIC ')
       ! Allow this link to a different but public library
       IF (PRESENT(link)) link = .TRUE.
       
    CASE DEFAULT
       ! Don't allow links, replace library name in rid with library name
       rid = rid(:8) // TRIM(library) // ACHAR(9) // rid(tab+1:)
    END SELECT
  END SUBROUTINE correct_rid
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_IDs/rid_exist
  ! NAME
  !  FUNCTION rid_exist(rid,pcom,src_lib,properties)
  ! PURPOSE
  !  Return T or F if rid currently is valid.  Optionally return properties
  ! STATUS
  !  Complete
  ! CAUTION
  !  This doesn't modify any buffers except to call read_rlib_list
  ! INPUTS
  !  rid: rid to check if it exists
  !  pcom: cina_common variable needed for reporting errors
  !  src_lib (OPTIONAL): library to search for rid in
  !                      Needed if rid is a link but want to check if link
  !                      is in src_lib.  Otherwise, src_lib is obtained
  !                      from library name in rid.
  ! OUTPUTS
  !  properties (OPTIONAL): properties of rid
  ! RETURN VALUE:
  !  .TRUE. if rid is valid and exists, .FALSE. otherwise
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start function
  !  11/18/2004 jpscott       Source lib can be specified now
  !  11/19/2004 jpscott       Properties of rid can be returned
  !  10/26/2005 jpscott       Now gets properties for linked rid
  ! SOURCE

  FUNCTION rid_exist(rid,pcom,src_lib,properties)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: rid,src_lib
    CHARACTER(LEN=*),INTENT(OUT)  :: properties
    OPTIONAL                      :: src_lib,properties

    LOGICAL(KIND=4)               :: rid_exist,flag
    INTEGER(KIND=4)               :: i,z,a,rid_num,seek,seek_pos(MAX_RATES)
    CHARACTER(LEN=MAX_RID_LEN)    :: rids(MAX_RATES),tmprid
    CHARACTER(LEN=MAX_RLIB_LEN)   :: library
    CHARACTER(LEN=10240)          :: lib_list(3),tmps

    ! Put list of libraries into lib_list
    CALL read_rlib_list('PUBLIC',pcom,lib_list(1))
    CALL read_rlib_list('SHARED',pcom,lib_list(2))
    CALL read_rlib_list('USER',pcom,lib_list(3))

    ! Put tabs at beginning and end to ease library searches
    tmps = ACHAR(9) // TRIM(lib_list(1)) // ACHAR(9) // &
         TRIM(lib_list(2)) // ACHAR(9) // TRIM(lib_list(3)) // ACHAR(9)

    rid_exist = .TRUE.
    IF (PRESENT(properties)) properties = ''

    ! Check for reasonable rate id
    i = INDEX(rid,ACHAR(11))
    IF (i < 1) rid_exist = .FALSE.
    
    ! Check if library in rid exists
    IF (rid_exist) THEN
       CALL decode_rid(rid,library,z,a)
       IF (PRESENT(src_lib)) library = src_lib

       ! Check if library exists
       i = INDEX(tmps,ACHAR(9) // TRIM(library) // ACHAR(9))
       IF (i < 1) rid_exist = .FALSE.
    END IF
    
    ! Check if isotope in library exists
    IF (rid_exist) THEN
       ! Read in isotope list
       CALL read_rlib_iso_list(library,pcom)
       ! Check if isotope exists
       rid_exist = isotope_exist(library,z,a)
    END IF
    
    ! Check if rid exists
    IF (rid_exist) THEN
       seek = 0
       ! Load rate list for this isotope
       CALL read_rid_list(library,pcom,z,a,rids,rid_num,seek_pos, &
            buffer=.FALSE.)
       
       flag = .FALSE.    ! Becomes true if rate id found
       i = 1
       DO WHILE (.NOT. flag .AND. (i <= rid_num))
          IF (rid == rids(i)) THEN
             flag = .TRUE.
             seek = seek_pos(i)
          END IF
          i = i + 1
       END DO
       rid_exist = flag
    END IF

    IF (PRESENT(properties) .AND. rid_exist) THEN
       ! Determine if rid is a link
       tmprid = rid
       CALL correct_rid(tmprid,library,flag)

       ! Save reac_str for comparision of rids
       CALL decode_rid(rid,unique_reac_str=tmps)
       IF (flag) THEN
          CALL decode_rid(rid,library)
          CALL read_rid_list(library,pcom,z,a,rids,rid_num, &
               seek_pos,buffer=.FALSE.)
          
          flag = .FALSE.  ! Becomes true if rate id found
          i = 1
          DO WHILE (.NOT. flag .AND. (i <= rid_num))
             CALL decode_rid(rids(i),unique_reac_str=tmprid)
             IF (tmprid == tmps) THEN
                flag = .TRUE.
                seek = seek_pos(i)
             END IF
             i = i + 1
          END DO

          IF (.NOT. flag) CALL report_error('Unable to find seek for ' // &
               TRIM(rid),'Improbable',pcom,1)
       END IF

       properties = read_rate_info(rid,pcom,'',seek, &
            RLIB_RATE_INFO_UNIT_NO_BUF)
      END IF
  END FUNCTION rid_exist
  !***

  !---------------------------------------------------------------------
  !****if* Rate_IDs/make_rid
  ! NAME
  !  FUNCTION make_rid(prop_list,pcom)
  ! PURPOSE
  !  Generate rate id from property list (for new rates)
  ! STATUS
  !  Complete
  ! CAUTION
  !  The library name in the rate id is blank and should be updated before 
  !  saving.
  !  
  !  This function uses the "Reaction String" and "Reaction Type" properties
  !  in prop_list.
  ! INPUTS
  !  prop_list: list of rate properties to obtain reaction info from
  !  pcom: cina_common variable needed for reporting errors
  ! RETURN VALUE
  !  rate id with an empty library name
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start function
  ! SOURCE

  FUNCTION make_rid(prop_list,pcom)
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: prop_list
    CHARACTER(LEN=MAX_RID_LEN)    :: make_rid
    CHARACTER(LEN=100)            :: reac_str,message,unique
    INTEGER(KIND=4)               :: i,t,z,a
    TYPE(reactionparticles)       :: r

    reac_str = get_prop_value('Reaction String',prop_list, &
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
    CALL read_reac_str(r,reac_str,message)
    IF (message /= '') CALL report_error('Problem interpreting ' // &
         'reaction string "' // TRIM(reac_str) // '" (' // &
         TRIM(message) // ')','Improbable',pcom,1)

    !Find reactant with largest z (if z is same, largest a)
    t = getreac_num(r)
    z = getreac_z(r,1)
    a = getreac_a(r,1)
    DO i = 2, t
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
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)) // ','

    IF (INDEX(message,',ec,') > 0) unique = 'ec'
    IF (INDEX(message,',bet+,') > 0) unique = 'bet+'
    IF (INDEX(message,',bet-,') > 0) unique = 'bet-'

    WRITE(make_rid,'(I2.2,2I3.3,5A)') t,z,a,' ',ACHAR(9),TRIM(reac_str), &
         ACHAR(11),TRIM(unique)

  END FUNCTION make_rid
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_IDs/inv_rid
  ! NAME
  !  SUBROUTINE inv_rid(invrid,rid,pcom,inv_lib,rid_r,inv_r)
  ! PURPOSE
  !  Return rid for inverse rate
  ! STATUS
  !  Complete
  ! INPUTS
  !  rid: rid of forward rate
  !  pcom: cina_common variable needed for reporting errors
  !  inv_lib (OPTIONAL): library to use in inverse rid.  
  !                      Otherwise, obtained copied from rid
  !  rid_r (OPTIONAL): reactionparticles for forward rate
  ! OUTPUTS
  !  invrid: rate id for inverse rate
  !  inv_lib (OPTIONAL): Library used in inverse rid
  !  rid_r (OPTIONAL): reactionparticles for forward rate
  !  inv_r (OPTIONAL): reactionparticles for inverse rate
  ! RETURN VALUE:
  !  rid for inverse rate
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  10/26/2005 jpscott       Start function
  ! SOURCE

  SUBROUTINE inv_rid(pcom,invrid,rid,inv_lib,rid_r,inv_r)
    USE reactionstrings
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: rid
    CHARACTER(LEN=*),INTENT(OUT)  :: invrid
    CHARACTER(LEN=*),INTENT(IN)   :: inv_lib
    TYPE(reactionparticles),INTENT(OUT) :: rid_r,inv_r
    OPTIONAL                      :: inv_lib,rid_r,inv_r
    CHARACTER(LEN=MAX_RLIB_LEN)   :: library
    TYPE(reactionparticles)       :: r,rv
    CHARACTER(LEN=200)            :: reac_str,unique,tmps
    INTEGER(KIND=4)               :: rtype,i,t,z,a

    ! Get r for reaction string
    CALL decode_rid(rid,reac_str=reac_str,unique=unique)
    CALL read_reac_str(r,reac_str,tmps,unique)
    IF (tmps /= '') THEN
       CALL decode_rid(rid,unique_reac_str=unique)
       CALL report_error('Unable to parse reaction string: ' // &
            TRIM(unique) // ': ' // TRIM(tmps),'Improbable',pcom,1)
    END IF

    IF (PRESENT(rid_r)) rid_r = r
    IF (PRESENT(inv_lib)) THEN
       library = inv_lib
    ELSE
       CALL decode_rid(rid,library)
    END IF

    rv = getinverse(r)
    IF (PRESENT(inv_r)) inv_r = rv
    reac_str = getreac_str(rv,1)
    rtype = getreactype(rv,unique)

    ! Find reactant with largest z (if z is same, largest a)
    t = getreac_num(rv)
    z = getreac_z(rv,1)
    a = getreac_a(rv,1)
    DO i = 2, t
       IF (getreac_z(rv,i) > z) THEN
          z = getreac_z(rv,i)
          a = getreac_a(rv,i)
       ELSE IF (getreac_z(rv,i) == z) THEN
          ! Check if a is greater
          IF (getreac_a(rv,i) > a) THEN
             z = getreac_z(rv,i)
             a = getreac_a(rv,i)
          END IF
       END IF
    END DO

    invrid = encode_rid(library,z,a,rtype,reac_str,unique)
  END SUBROUTINE inv_rid
  !***

  !****if* Rate_IDs/change_rid
  ! NAME
  !  FUNCTION change_rid(rid,library,z,a,rtype,reac_str,unique,success)
  ! PURPOSE
  !  Modify parts of a rid
  ! STATUS
  !  Complete
  ! INPUTS
  !  rid: rate id to extract information from
  !  library (OPTIONAL): name of library rate id references
  !  z (OPTIONAL): number of protons of heaviest reactant in reaction
  !  a (OPTINOAL): atomic mass of heaviest reactant in reaction
  !  rtype (OPTIONAL): type of reaction (number between 1 and 8)
  !  reac_str (OPTIONAL): reaction string
  !  unique (OPTIONAL): unique identifier for atypical reactions
  ! OUTPUTS
  !  success (OPTIONAL): T/F if successful
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  01/19/2006 jpscott       Start function
  ! SOURCE

  FUNCTION change_rid(rid,library,z,a,rtype,reac_str,unique, &
       success)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: rid
    CHARACTER(LEN=*),INTENT(IN)   :: library,reac_str,unique
    INTEGER(KIND=4),INTENT(IN)    :: z,a,rtype
    INTEGER(KIND=4)               :: tab,vtab,Ca,Cz,Crtype
    LOGICAL(KIND=4),INTENT(OUT)   :: success
    CHARACTER(LEN=100)            :: Clibrary,Creac_str,Cunique
    CHARACTER(LEN=MAX_RID_LEN)    :: change_rid
    OPTIONAL                      :: library,z,a,rtype,reac_str,unique
    OPTIONAL                      :: success

    IF (PRESENT(success)) THEN
       CALL decode_rid(rid,Clibrary,Cz,Ca,Crtype,Creac_str,Cunique,success=success)
    ELSE
       CALL decode_rid(rid,Clibrary,Cz,Ca,Crtype,Creac_str,Cunique)
    END IF

    IF (PRESENT(rtype)) Crtype = rtype
    IF (PRESENT(z)) Cz = z
    IF (PRESENT(a)) Ca = a
    IF (PRESENT(library)) Clibrary = library
    IF (PRESENT(reac_str)) Creac_str = reac_str
    IF (PRESENT(unique)) Cunique = unique

    change_rid = encode_rid(Clibrary,Cz,Ca,Crtype,Creac_str,Cunique)
  END FUNCTION change_rid
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_IDs/get_rid_seek
  ! NAME
  !  FUNCTION get_rid_seek(rid)
  ! PURPOSE
  !  Return seek position for a rate id.  This is needed for Rate_Info.
  ! STATUS
  !  Complete
  ! INPUTS
  !  rid: rate id to return seek position for
  ! RETURN VALUE
  !  INT(KIND=4) that is seek position for a rate id if found, otherwise
  !  -1 is returned.
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start function
  ! SOURCE

  FUNCTION get_rid_seek(rid)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: rid
    INTEGER(KIND=4)                  :: get_rid_seek
    CHARACTER(LEN=MAX_RLIB_LEN)      :: library
    INTEGER(KIND=4)                  :: z,a,lib_index,i

    ! Default value returned when seek position is not found
    get_rid_seek = -1

    ! Extract library name, z, and a from rate id
    CALL decode_rid(rid,library,z,a)

    ! Return -1 if library is not found
    lib_index = find_rlib_rid_list(library)
    IF (lib_index < 0) RETURN
    
    ! Return -1 if isotope is not found
    IF (rid_list(lib_index)%z /= z .OR. rid_list(lib_index)%a /= a) RETURN

    ! Find rid
    DO i = 1, rid_list(lib_index)%rid_num
       IF (rid_list(lib_index)%rid(i) == rid) THEN
          get_rid_seek = rid_list(lib_index)%seek_pos(i)
          RETURN
       END IF
    END DO
  END FUNCTION get_rid_seek
  !***

  !-------------------------------------------------------------------------
  !****is* Library_Info/read_rlib_info
  ! NAME
  !  SUBROUTINE read_rlib_info(library,pcom,lib_info)
  ! PURPOSE
  !  Read rate library info properties from disk into character array
  ! STATUS
  !  Complete
  ! CAUTION
  !  Properties and values should not contain any of the rate library info
  !  deliminators or get_prop_value will not work correctly.
  ! USAGE
  !  Use this subroutine to load library info properties from disk into a 
  !  character array.  Then use get_prop_value to retrieve individual
  !  properties.
  ! INPUTS
  !  library: Name of library to retrieve info for
  !  pcom: cina_common variable needed for reporting errors
  ! OUTPUTS
  !  lib_info: Deliminated list of library info properties
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/23/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE read_rlib_info(library,pcom,lib_info)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: library
    CHARACTER(LEN=*),INTENT(OUT)  :: lib_info
    CHARACTER(LEN=MAX_RATE_LEN)   :: line
    INTEGER(KIND=4)               :: status

    line = get_rlib_path(library,pcom%USER)
    IF (line == '') CALL report_error('Rate Library ' // TRIM(library) // &
         ' was not found','Improper usage',pcom,1)
    OPEN(RLIB_INFO_UNIT, FILE=TRIM(line) // 'Library_Info', IOSTAT=status, &
         ACTION='READ')
    IF (status /= 0) CALL report_error('Error opening library info for' // &
         ' library ' // TRIM(library),'File input/output',pcom,1)

    lib_info = ''
    DO WHILE (status == 0)
       READ(RLIB_INFO_UNIT,'(A)',IOSTAT=status) line
       IF (status > 0) THEN
          CALL report_error('Error reading library info for library ' // &
               TRIM(library),'File input/output',pcom,1)
       ELSE IF (status == 0) THEN
          lib_info = TRIM(lib_info) // RLIB_INFO_PROP_SEP // &
               line2str(TRIM(line))
       END IF
    END DO
    ! Remove initial property separator
    lib_info = lib_info(LEN(RLIB_INFO_PROP_SEP)+1:)

    CLOSE(RLIB_INFO_UNIT)
  END SUBROUTINE read_rlib_info
  !***

  !-------------------------------------------------------------------------
  !****is* Library_Info/save_rlib_info
  ! NAME
  !  SUBROUTINE save_rlib_info(library,pcom,lib_info,path)
  ! PURPOSE
  !  Save rate library info properties from character array to disk
  ! STATUS
  !  Complete
  ! USAGE
  !  Use read_rlib_info to load library info properties from disk into a 
  !  character array.  Then use get_prop_value and set_prop_value to 
  !  retrieve and modify individual properties.
  ! INPUTS
  !  library: Name of library to save info for
  !  pcom: cina_common variable needed for reporting errors
  !  lib_info: Deliminated list of library info properties
  !  path (OPTIONAL): path to library directory if not in suite
  ! OUTPUTS
  !  None. This subroutine returns if successful otherwise reports an error.
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/23/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE save_rlib_info(library,pcom,lib_info,path)
    USE io
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN) :: pcom
    CHARACTER(LEN=*),INTENT(IN)   :: library,lib_info,path
    CHARACTER(LEN=MAX_RATE_LEN)   :: line,lib_info_copy
    INTEGER(KIND=4)               :: status
    LOGICAL(KIND=4)               :: loop
    OPTIONAL                      :: path

    ! Don't want to erase lib_info (when using next_in_list) so make a copy
    lib_info_copy = lib_info    

    IF (PRESENT(path)) THEN
       line = path
    ELSE
       line = get_rlib_path(library,pcom%USER)
    END IF
    IF (line == '') CALL report_error('Rate Library ' // TRIM(library) // &
         ' was not found','Improper usage',pcom,1)
    OPEN(RLIB_INFO_UNIT, FILE=TRIM(line) // 'Library_Info', IOSTAT=status, &
         ACTION='WRITE')
    IF (status /= 0) CALL report_error('Error opening library info for' // &
         ' library ' // TRIM(library),'File input/output',pcom,1)

    loop = .TRUE.
    DO WHILE (loop)
       loop = next_in_list(line,lib_info_copy,RLIB_INFO_PROP_SEP)

       IF (INDEX(line,RLIB_INFO_VAL_SEP) > 0) THEN
          CALL print_long_string(TRIM(str2line(TRIM(line))), &
               unit=RLIB_INFO_UNIT,IOSTAT=status)
          IF (status /= 0) CALL report_error('Error writing library ' // &
               'info for library ' // TRIM(library),'File input/output', &
               pcom,1)
       END IF
    END DO

    CLOSE(RLIB_INFO_UNIT)
  END SUBROUTINE save_rlib_info
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_Info/read_rate_info
  ! NAME
  !  FUNCTION read_rate_info(rid,pcom,prop_list,seek_pos,unit)
  ! PURPOSE
  !  Return rate properties for a rate ID
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) becore calling
  !  this subroutine.  File seek positions should be loaded into rid_list
  !  (read_rid_list) before calling this subroutine unless seek_pos is
  !  supplied.  Of course, invalid seek positions or rids signal an error 
  !  and cause the program to exit.
  ! INPUTS
  !  rid: rate ID to retrieve properties for
  !  pcom: cina_common variable needed for reporting errors
  !  prop_list: list of properties to return.  All properties are returned
  !             if empty.  List should be separated with RATE_INFO_PROP_SEP
  !  seek_pos (OPTIONAL): file seek position to use instead of one in 
  !                       rid_list
  !  unit (OPTIONAL): unit number to use when opening file.  This is needed
  !                   for read_rate_info_no_buf
  ! RETURN VALUE
  !  List of rate properties for a rate ID.  If library in rid is empty then
  !  an empty string is returned and no error is generated.  This prevents
  !  errors when reading rate info for a new rate.
  ! TODO
  !  Create new rid file if it is out of sync with iso file?
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/31/2004 jpscott       Start function
  ! SOURCE

  FUNCTION read_rate_info(rid,pcom,prop_list,seek_pos,unit)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: rid,prop_list
    INTEGER(KIND=4),INTENT(IN)       :: seek_pos,unit
    CHARACTER(LEN=MAX_RATE_LEN)      :: read_rate_info
    OPTIONAL                         :: seek_pos,unit

    INTEGER(KIND=4)                  :: z,a,seek,status,i,unit_num
    CHARACTER(LEN=MAX_RLIB_LEN)      :: library
    CHARACTER(LEN=MAX_PATH_LEN)      :: path
    CHARACTER(LEN=MAX_RATE_LEN)      :: line,properties,prop_list2,name
    LOGICAL(KIND=4)                  :: loop

    ! Default value to return if an error
    read_rate_info = ''

    CALL decode_rid(rid,library,z,a)
    ! If library name is empty, new rate, then return
    IF (library == '') RETURN

    ! Get seek position
    IF (PRESENT(seek_pos)) THEN
       seek = seek_pos
    ELSE
       seek = get_rid_seek(rid)
    END IF
    IF (seek < 0) CALL report_error('Unable to find seek_pos for ' // &
         TRIM(rid),'Improper usage',pcom,1)

    ! Get unit number
    IF (PRESENT(unit)) THEN
       unit_num = unit
    ELSE
       unit_num = RLIB_RATE_INFO_UNIT
    END IF

    ! Get path to iso file
    path = get_riso_path(library,z,a,'iso',pcom%USER)

    OPEN(unit_num, FILE=path, IOSTAT=status, ACTION='READ')
    IF (status /= 0) CALL report_error('Can not open rate info for ' // &
         TRIM(rid),'File input/output',pcom,1)

    ! Position file pointer
    status = FSEEK(unit_num,seek,0)
    IF (status /= 0) CALL report_error('Can not seek in rate info file' // &
         ' for ' // TRIM(rid),'File input/output',pcom,1)

    ! Make sure rid is correct
    READ(unit_num,'(A)',IOSTAT=status) line
    IF (status /= 0) CALL report_error('Can not read rate id for ' // &
         TRIM(rid),'File input/output',pcom,1)

    IF (line /= rid) THEN
       CLOSE(unit_num)
       CALL report_error('seek is incorrect for rid ' // rid // &
            ':' // line,'Improbable',pcom,1)
    END IF

    ! Read in properties into properties
    properties = ''
    DO WHILE (status == 0)
       READ (unit_num,'(A)',IOSTAT=status) line
       IF (status == 0) THEN
          ! If NOTES, replace '\0A' with '\0C'
          IF (line(1:17) == 'Reaction Notes = ') &
               line = replaceall(line,ACHAR(92)//'0A',ACHAR(92)//'0C')

          ! Search for RATE_INFO_VAL_SEP to tell if its a property
          i = INDEX(line,RATE_INFO_VAL_SEP)
          IF (i > 0) THEN
             properties = TRIM(properties) // RATE_INFO_PROP_SEP // &
                  TRIM(line2str(TRIM(line)))
          ELSE
             ! Stop reading in if another rid is reached
             status = -1
          END IF
       ELSE IF (status > 0) THEN
          CALL report_error('Can not read rate properties for ' // &
               TRIM(rid),'File input/output',pcom,1)
       END IF
    END DO
    CLOSE(unit_num)

    ! Remove initial RATE_INFO_PROP_SEP
    properties = properties(LEN(RATE_INFO_PROP_SEP)+1:)

    ! Return desired properties
    IF (prop_list == '') THEN
       read_rate_info = properties
    ELSE
       ! Store prop_list into prop_list2 because prop_list can't be modified
       prop_list2 = prop_list

       ! read_rate_info should be empty here
       loop = .TRUE.
       DO WHILE (loop)
          ! Get the next property name in prop_list2
          loop = next_in_list(name,prop_list2,ACHAR(9))
          ! Get the value of this property
          line = get_prop_value(name,properties,RATE_INFO_PROP_SEP, &
               RATE_INFO_VAL_SEP)
          ! Add name and value to prop to return to caller
          read_rate_info = TRIM(read_rate_info) // RATE_INFO_PROP_SEP // &
               TRIM(name) // RATE_INFO_VAL_SEP // line
       END DO
       ! Remove initial RATE_INFO_PROP_SEP
       read_rate_info = read_rate_info(LEN(RATE_INFO_PROP_SEP)+1:)
    END IF
  END FUNCTION read_rate_info
 !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/save_rate_info
  ! NAME
  !  SUBROUTINE save_rate_info(pcom,report,lib_info,path,plevel,inv_rates)
  ! PURPOSE
  !  Save rate_file buffer to disk
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library and isotope lists are loaded (read_rlib_list and
  !  read_rlib_iso_list) before calling this subroutine (unless path is 
  !  used).
  !  
  !  Rates that have never been in the suite should have an empty library
  !  name in the rate ID.
  !  
  !  Note that rid_list will be overwritten with the rid_list for the 
  !  isotope and library being saved. Then save_rid_list will be called
  !  internally.  
  !  
  !  riso_list will also be updated to include any new isotopes but this
  !  list is not saved.  Call save_rlib_iso_list after all isotopes have
  !  been modified.
  !
  !  When saving, the properties for a rate in rate_file will overwrite
  !  any pre-existing properties.  To modify only a few properties and keep
  !  all remaining properties, call read_rate_info or read_rate_info_no_buf
  !  to retrieve old properties, then call update_rate_prop to update the
  !  old properties, then put this property list in rate_file.
  !  
  !  If a rate ID is a link to another library, all properties will be 
  !  obtained from the other library.  For this reason, any properties
  !  for rate IDs that are links WILL NOT be saved.
  !  
  !  Only rates with a rate ID in rate_file will be modified and all others 
  !  will remain without being erased or modified.
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  !  report: string containing existing report
  !  lib_info: string containing existing lib_info
  !  path (OPTIONAL): use for saving rate info outside of suite
  !  plevel (OPTIONAL): controls the level of detail in the report and 
  !                     lib_info.  Should be 2 for library management
  !                     or 4 for rate management.
  !  inv_rates (OPTIONAL): 0 if inverse flag should not be checked (DEFAULT)
  !                        1 if saving forward rates now and inverses next
  !                        2 if saving inverse rates now and forward before
  ! OUTPUTS
  !  report: string with new report appended to existing report
  !  lib_info: string with new lib_info appended to existing lib_info
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE save_rate_info(pcom,report,lib_info,path,plevel,inv_rates)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(INOUT)   :: report,lib_info
    CHARACTER(LEN=*),INTENT(IN)      :: path
    INTEGER(KIND=4),INTENT(IN)       :: plevel,inv_rates
    OPTIONAL                         :: path,plevel,inv_rates
    CHARACTER(LEN=MAX_RGRP_LEN)      :: group
    CHARACTER(LEN=MAX_PATH_LEN)      :: file_path,dir_path
    CHARACTER(LEN=2000)              :: tmps
    INTEGER(KIND=4)                  :: status,z,a,log_level,inv
    LOGICAL(KIND=4)                  :: loop,exist

    ! Set log_level
    IF (PRESENT(plevel)) THEN
       log_level = plevel
    ELSE
       log_level = 1
    END IF

    ! Set inv
    inv = 0
    IF (PRESENT(inv_rates)) inv = inv_rates

    ! This converts from INT(KIND=2) to INT(KIND=4)
    z = rate_file%z
    a = rate_file%a

    ! Get library group and file_path
    IF (PRESENT(PATH)) THEN
       file_path = get_riso_path(rate_file%library, z, a, 'iso', &
            pcom%USER, path)
       dir_path = path
       group = 'USER'
    ELSE
       ! Get rate library group
       CALL find_rlib_list_index(rate_file%library, group)

       ! Check permissions
       SELECT CASE (group)
       CASE ('PUBLIC ')
          CALL report_error('PUBLIC rate libraries such as "' // &
               TRIM(rate_file%library) // '" may not be modified.', &
               'Improper usage',pcom,1)
       CASE ('SHARED ')
          CALL report_error('SHARED rate libraries such as "' // &
               TRIM(rate_file%library) // '" may not be modified.', &
               'Improper usage',pcom,1)
       CASE ('USER ')
          ! Permission granted
       CASE (' ')
          CALL report_error('Rate library "' // TRIM(rate_file%library) // &
               '" does not exist.','Improper usage',pcom,1)
       CASE DEFAULT
          CALL report_error('Unknown rate library group "' // TRIM(group) &
               // '" for saving libraries.','Improper usage',pcom,1)
       END SELECT
       
       file_path = get_riso_path(rate_file%library, z, a, 'iso', &
            pcom%USER)
       dir_path = get_rlib_path(rate_file%library, pcom%USER)
    END IF

    ! Make sure isotope list is in memory
    status = find_rlib_iso_list(rate_file%library)
    IF (status < 1) CALL report_error('Isotope list should be loaded ' // &
         'before saving "' // TRIM(rate_file%library) // '" library.', &
         'Developer Reminder',pcom,1)

    ! exist is T if isotope exist, loop is T if z exists
    exist = isotope_exist(rate_file%library, z, a, loop)

    ! Make a directory for this value of z if necessary
    IF (.NOT. loop) THEN
       WRITE(tmps,'(2A,I0,A)') '/bin/mkdir -p ''',TRIM(dir_path),z, &
            ''' &> /dev/null'
       status = safe_shell(tmps)
       IF (status /= 0) CALL report_error('Could not make directory ' // &
            'for z in library ' // TRIM(rate_file%library), &
            'External fileio',pcom,1)
    END IF

    ! Make a backup of iso file if isotope exists, and open it
    IF (exist) THEN
       status = DELFILESQQ(TRIM(file_path) // '.old')
       loop = RENAMEFILEQQ(file_path, TRIM(file_path) // '.old')
       IF (.NOT. loop) CALL report_error('Could not rename old .rate ' // &
            'file for library ' // TRIM(rate_file%library), &
            'External fileio',pcom,1)
       OPEN(RLIB_RATE_INFO_OLD_UNIT, FILE=TRIM(file_path) // '.old', &
            IOSTAT=status, ACTION='READ')
       IF (status /= 0) THEN
          WRITE(tmps,'(3A,I0)') 'Could not open old .rate file for ' // &
               'library '//TRIM(rate_file%library),' Error ',status
          CALL report_error(tmps,'File input/output',pcom,1)
       END IF
    ELSE
       ! Add isotope if needed
       CALL add_isotope(rate_file%library, z, a)
    END IF

    ! Open new iso file
    OPEN(RLIB_RATE_INFO_UNIT, FILE=file_path, IOSTAT=status, ACTION='WRITE')
    IF (status /= 0) THEN
       WRITE(tmps,'(3A,I0)') 'Could not open new .rate file for ' // &
            'library ' // TRIM(rate_file%library),' Error ',status
       CALL report_error(tmps,'File input/output',pcom,1)
    END IF

    ! Most of the work is performed in save_rate_info_internal to simplify
    ! the save process
    CALL save_rate_info_internal(pcom,exist,report,lib_info,log_level,inv)
    ! Set All Inverses Present
    IF (inv == 0) CALL set_prop_value('All Inverses Present','NO', &
         lib_info, RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)

    IF (exist) CLOSE(RLIB_RATE_INFO_OLD_UNIT)
    CLOSE(RLIB_RATE_INFO_UNIT)

    ! Save rid list
    IF (PRESENT(path)) THEN
       CALL save_rid_list(rate_file%library, pcom, z, a, path)
    ELSE
       CALL save_rid_list(rate_file%library, pcom, z, a)
    END IF

    ! Mark rate_file buffer as cleared
    rate_file%library = ''
  END SUBROUTINE save_rate_info
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/save_rate_info_inv
  ! NAME
  !  SUBROUTINE save_rate_info_inv(pcom,report,lib_info,path,plevel)
  ! PURPOSE
  !  Save rate_file buffer and inverse rates to disk
  ! STATUS
  !  Complete
  ! CAUTION
  !  This subroutine acts just like save_rate_info except that inverses are 
  !  saved for all rates in rate_file buffer
  !  
  !  Make sure rate library and isotope lists are loaded (read_rlib_list and
  !  read_rlib_iso_list) before calling this subroutine (unless path is 
  !  used).
  !  
  !  Rates that have never been in the suite should have an empty library
  !  name in the rate ID.
  !  
  !  Note that rid_list will be overwritten with the rid_list for the 
  !  isotope and library being saved. Then save_rid_list will be called
  !  internally.  
  !  
  !  riso_list will also be updated to include any new isotopes but this
  !  list is not saved.  Call save_rlib_iso_list after all isotopes have
  !  been modified.
  !
  !  When saving, the properties for a rate in rate_file will overwrite
  !  any pre-existing properties.  To modify only a few properties and keep
  !  all remaining properties, call read_rate_info or read_rate_info_no_buf
  !  to retrieve old properties, then call update_rate_prop to update the
  !  old properties, then put this property list in rate_file.
  !  
  !  If a rate ID is a link to another library, all properties will be 
  !  obtained from the other library.  For this reason, any properties
  !  for rate IDs that are links WILL NOT be saved.
  !  
  !  Only rates with a rate ID in rate_file will be modified and all others 
  !  will remain without being erased or modified.
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  !  report: string containing existing report
  !  lib_info: string containing existing lib_info
  !  path (OPTIONAL): use for saving rate info outside of suite
  !  plevel (OPTIONAL): controls the level of detail in the report and 
  !                     lib_info.  Should be 2 for library management
  !                     or 4 for rate management.
  ! OUTPUTS
  !  report: string with new report appended to existing report
  !  lib_info: string with new lib_info appended to existing lib_info
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE save_rate_info_inv(pcom,report,lib_info,path,plevel)
    USE inv_parm
    USE constants
    USE reactionstrings
    USE convert
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(INOUT)   :: report,lib_info
    CHARACTER(LEN=*),INTENT(IN)      :: path
    INTEGER(KIND=4)                  :: plevel
    OPTIONAL                         :: path,plevel
    TYPE(save_rate_info_buffer)      :: rate_file_copy
    TYPE(reactionparticles)          :: r(rate_file%rid_num), r_inv
    INTEGER(KIND=4)                  :: log_level,i,a_num,status,z,m
    CHARACTER(LEN=MAX_PATH_LEN)      :: dir_path
    CHARACTER(LEN=200)               :: reac_str,unique,reason
    REAL(KIND=8)                     :: a(MAX_A),a_inv(MAX_A),del_a(MAX_A)
    LOGICAL                          :: all_inverses
    CHARACTER(LEN=400)               :: tmps

    ! Set log_level and dir_path
    IF (PRESENT(plevel)) THEN
       log_level = plevel
    ELSE
       log_level = 1
    END IF
    IF (PRESENT(path)) THEN
       dir_path = path
    ELSE
       dir_path = get_rlib_path(rate_file%library, pcom%USER)
    END IF

    rate_file_copy = rate_file
    all_inverses = .TRUE.

    ! Obtaining reactionparticles for each reaction in ratefile
    ! Then set Q-value, Number of Reactants, Number of Products
    DO i = 1, rate_file%rid_num
       CALL decode_rid(rate_file_copy%rids(i), rtype=m, reac_str=reac_str, &
            unique=unique)
       CALL read_reac_str(r(i),reac_str,reason,unique)
       IF (reason /= '') THEN
          CALL decode_rid(rate_file_copy%rids(i), unique_reac_str=unique)
          CALL report_error('Unable to parse reaction string: ' // &
               TRIM(unique) // ': ' // TRIM(reason),'Improbable',pcom,1)
       END IF

       ! Check reaction type
       tmps = get_prop_value('Reaction Type',rate_file%prop(i), &
            RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
       READ(tmps,'(I)',IOSTAT=status) z
       IF (status /= 0 .OR. z /= m) THEN
          CALL decode_rid(rate_file_copy%rids(i), unique_reac_str=unique)
          CALL report_error('Problem with reaction type "' // TRIM(tmps) // &
               '" for ' // TRIM(unique),'Improbable',pcom,1)
       END IF

       ! Set qvalue
       WRITE(tmps,'(1P,G13.6)') get_qvalue(r(i))
       tmps = ADJUSTL(tmps)
       !WRITE(*,'(2A)') 'qvalue=',TRIM(tmps)
       CALL set_prop_value('Q-value',tmps, &
            rate_file%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       ! Set reactant num
       WRITE(tmps,'(1P,G13.6)') getreac_num(r(i))
       tmps = ADJUSTL(tmps)
       !WRITE(*,'(2A)') 'reacnum=',TRIM(tmps)
       CALL set_prop_value('Number of Reactants',tmps, &
            rate_file%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

       ! Set product num
       WRITE(tmps,'(1P,G13.6)') getprod_num(r(i))
       tmps = ADJUSTL(tmps)
       !WRITE(*,'(2A)') 'prodnum=',TRIM(tmps)
       CALL set_prop_value('Number of Products',tmps, &
            rate_file%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
    END DO

    !print *,'saving forward rates'
    ! Save all forward rates
    CALL save_rate_info(pcom,report,lib_info,dir_path,log_level,inv_rates=1)

    ! Save inverse rates
    DO i = 1, rate_file%rid_num
       !print *,'saving inverse rate for ',TRIM(rate_file_copy%rids(i))
       ! Get parameters
       CALL get_parm_array(rate_file_copy%prop(i), a, a_num)
       IF (a_num < 1) reason = 'parameter parse error'

       ! got parameters
       IF (reason == '') THEN
          CALL get_inv_parm(a,a_num,a_inv,r(i),del_a,status,reason)
          IF (status /= 0) reason = 'inverse parameter error: ' // reason
       END IF
       
       ! got inverse parameters
       IF (reason == '') THEN
          !WRITE(*,'(2A)') 'props=',TRIM(rate_file_copy%prop(i))
          r_inv = getinverse(r(i))
          reac_str = getreac_str(r_inv,1)
          CALL set_prop_value('Reaction String',reac_str, &
               rate_file_copy%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

          ! Set reaction type
          z = getreactype(r_inv,unique)
          IF (unique == '') THEN
             WRITE(tmps,'(I0,A)') z,',v'
          ELSE
             WRITE(tmps,'(I0,2A)') z,',v,',unique
          END IF
          CALL set_prop_value('Reaction Type',tmps, &
               rate_file_copy%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
          
          ! Set qvalue
          WRITE(tmps,'(1P,G13.6)') get_qvalue(r_inv)
          tmps = ADJUSTL(tmps)
          !WRITE(*,'(2A)') 'qvalue=',TRIM(tmps)
          CALL set_prop_value('Q-value',tmps, &
               rate_file_copy%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

          ! Set reactant num
          WRITE(tmps,'(1P,G13.6)') getreac_num(r_inv)
          tmps = ADJUSTL(tmps)
          !WRITE(*,'(2A)') 'reacnum=',TRIM(tmps)
          CALL set_prop_value('Number of Reactants',tmps, &
               rate_file_copy%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

          ! Set product num
          WRITE(tmps,'(1P,G13.6)') getprod_num(r_inv)
          tmps = ADJUSTL(tmps)
          !WRITE(*,'(2A)') 'prodnum=',TRIM(tmps)
          CALL set_prop_value('Number of Products',tmps, &
               rate_file_copy%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

          ! update props with inverse parameters
          WRITE(unique,'(A,I0,A)') '(1P,',a_num-1,'(E12.5,'',''),E12.5)'
          WRITE(tmps,unique) a_inv(:a_num)
          CALL set_prop_value('Parameters',tmps, &
               rate_file_copy%prop(i),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
          !WRITE(*,'(2A)') 'props=',TRIM(rate_file_copy%prop(i))

          ! make rid for inverse rate
          tmps = make_rid(rate_file_copy%prop(i),pcom)
          !print '(2A)','new inverse rid=',TRIM(tmps)
          CALL decode_rid(tmps,z=z,a=m)

          CALL open_rate_info(rate_file_copy%library, pcom, z, m)
          CALL change_rate_info(tmps,pcom,rate_file_copy%prop(i))
          CALL save_rate_info(pcom,report,lib_info,dir_path,log_level,inv_rates=2)
       END IF

       IF (reason /= '') THEN
          CALL decode_rid(rate_file_copy%rids(i), unique_reac_str=unique)
          report = TRIM(report) // 'UNABLE TO UPDATE INVERSE RATE for ' // &
               TRIM(unique) // ', ' // TRIM(reason) // ACHAR(8)
          all_inverses = .FALSE.
       END IF
    END DO

    ! Set All Inverses Present to NO if any problems
    IF (.NOT. all_inverses) CALL set_prop_value('All Inverses Present', &
         'NO', lib_info, RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
  END SUBROUTINE save_rate_info_inv
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/save_rate_info_no_buf
  ! NAME
  !  SUBROUTINE save_rate_info_no_buf(pcom,report,lib_info,library,rid,
  !                                   props,path,plevel,inv_rates)
  ! PURPOSE
  !  Save rate_file buffer to disk without changing rid_list or rate_file
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library and isotope lists are loaded (read_rlib_list and
  !  read_rlib_iso_list) before calling this subroutine (unless path is 
  !  used).
  !  
  !  Rates that have never been in the suite should have an empty library
  !  name in the rate ID.
  !  
  !  Note that save_rid_list will be called internally.  
  !  
  !  riso_list will also be updated to include any new isotopes but this
  !  list is not saved.  Call save_rlib_iso_list after all isotopes have
  !  been modified.
  !
  !  When saving, the properties for a rate in rate_file will overwrite
  !  any pre-existing properties.  To modify only a few properties and keep
  !  all remaining properties, call read_rate_info or read_rate_info_no_buf
  !  to retrieve old properties, then call update_rate_prop to update the
  !  old properties, then put this property list in rate_file.
  !  
  !  If a rate ID is a link to another library, all properties will be 
  !  obtained from the other library.  For this reason, any properties
  !  for rate IDs that are links WILL NOT be saved.
  !  
  !  Only rates with a rate ID in rate_file will be modified and all others 
  !  will remain without being erased or modified.
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  !  report: string containing existing report
  !  lib_info: string containing existing lib_info
  !  library: name of library to save rate in
  !  rid: string containing rate id of rate to save
  !  props: string containing properties of rate to save
  !  path (OPTIONAL): use for saving rate info outside of suite
  !  plevel (OPTIONAL): controls the level of detail in the report and 
  !                     lib_info.  Should be 2 for library management
  !                     or 4 for rate management.
  !  inv_rates (OPTIONAL): 0 if inverse flag should not be checked (DEFAULT)
  !                        1 if saving forward rates now and inverses next
  !                        2 if saving inverse rates now and forward before
  ! OUTPUTS
  !  report: string with new report appended to existing report
  !  lib_info: string with new lib_info appended to existing lib_info
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE save_rate_info_no_buf(pcom,report,lib_info,library,rid,props, &
       path,plevel,inv_rates)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(INOUT)   :: report,lib_info
    CHARACTER(LEN=*),INTENT(IN)      :: path,rid,props,library
    INTEGER(KIND=4),INTENT(IN)       :: plevel,inv_rates
    OPTIONAL                         :: path,plevel,inv_rates
    INTEGER(KIND=4)                  :: int_plevel,int_inv_rates
    INTEGER(KIND=4)                  :: z,a,i
    TYPE(rid_list_buffer)            :: rid_list_copy
    TYPE(save_rate_info_buffer)      :: rate_file_copy

    int_plevel = 1
    IF (PRESENT(plevel)) int_plevel = plevel
    int_inv_rates = 0
    IF (PRESENT(inv_rates)) int_inv_rates = inv_rates

    ! Save rid_list and rate_file
    i = find_rlib_rid_list(library)
    IF (i < 1) CALL report_error('Unable to find "' // TRIM(library) // '" in list', &
         'Improbable',pcom,1)
    rid_list_copy = rid_list(i)
    rate_file_copy = rate_file

    CALL decode_rid(rid,z=z,a=a)
    CALL open_rate_info(library,pcom,z,a)
    CALL change_rate_info(rid,pcom,props)

    IF (PRESENT(path)) THEN
       CALL save_rate_info(pcom,report,lib_info,path,int_plevel,int_inv_rates)
    ELSE
       CALL save_rate_info(pcom,report,lib_info, &
            plevel=int_plevel, inv_rates=int_inv_rates)
    END IF

    ! Restore rid_list and rate_file
    rid_list(i) = rid_list_copy
    rate_file = rate_file_copy
  END SUBROUTINE save_rate_info_no_buf
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/open_rate_info
  ! NAME
  !  SUBROUTINE open_rate_info(library,pcom,z,a)
  ! PURPOSE
  !  Initialize rate_file buffer for modifing rate properties
  ! STATUS
  !  Complete
  ! CAUTION
  !  Only one isotope from one library may be opened at a time
  ! INPUTS
  !  library: Name of library to open for modifing rate info
  !  pcom: cina_common variable needed for reporting errors
  !  z: Number of protons of isotope to open for modifing rate info
  !  a: Atomic mass of isotope to open for modifing rate info
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/20/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE open_rate_info(library,pcom,z,a)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: library
    INTEGER(KIND=4),INTENT(IN)       :: z,a
    
    ! Check that rate_file buffer is not being used
    IF (rate_file%library /= '') CALL report_error('Attempt to open a ' // &
         'used rate_file buffer','Improbable',pcom,1)

    rate_file%library = library
    rate_file%z = z
    rate_file%a = a
    rate_file%rid_num = 0
    rate_file%rids = ''
    rate_file%prop = ''
  END SUBROUTINE open_rate_info
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/change_rate_info
  ! NAME
  !  SUBROUTINE change_rate_info(rid,pcom,properties)
  ! PURPOSE
  !  Modify rate info properties in rate_file buffer for an rate ID
  ! STATUS
  !  Complete
  ! CAUTION
  !  The list of properties in rate_file will be overwritten with the list
  !  of properties given to this subroutine.  This also applies when saving
  !  rate info.  See save_rate_info for more information.  Thus properties 
  !  that are not modified WILL BE ERASED.
  !  
  !  If a rate ID is a link to another library, all properties will be 
  !  obtained from the other library.  For this reason, any properties
  !  for rate IDs that are links WILL NOT be saved.
  ! INPUTS
  !  rid: rate ID to modify properties for
  !  pcom: cina_common variable needed for reporting errors
  !  properties: list of properties and values to modify.  List should be
  !              separated using RATE_INFO_PROP_SEP and RATE_INFO_VAL_SEP
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE change_rate_info(rid,pcom,properties)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: rid,properties

    LOGICAL(KIND=4)                  :: loop
    INTEGER(KIND=4)                  :: i,rid_index,z,a

    ! Check z and a from rate id
    CALL decode_rid(rid,z=z,a=a)
    IF ((z /= rate_file%z) .AND. (a /= rate_file%a)) CALL report_error( &
         'z and a in rate id are invalid.','Improper usage',pcom,1)

    ! See if rid is in rate_file buffer
    rid_index = -1
    DO i = 1, rate_file%rid_num
       IF (rate_file%rids(i) == rid) rid_index = i
    END DO
    IF (rid_index == -1) THEN
       rid_index = rate_file%rid_num + 1
       IF (rid_index > MAX_RATE_INFO) CALL report_error('rate_file ' // &
            'buffer is too small to add '//TRIM(rid),'Developer Reminder', &
            pcom,1)

       rate_file%rids(rid_index) = rid
       rate_file%rid_num = rid_index
    END IF

    ! Now that rid_index is set, save properties to buffer
    rate_file%prop(rid_index) = properties
  END SUBROUTINE change_rate_info
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_Info/read_rate_info_no_buf
  ! NAME
  !  FUNCTION read_rate_info_no_buf(rid,pcom,prop_list)
  ! PURPOSE
  !  Return rate properties for a rate ID without buffering
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) becore calling
  !  this subroutine.  This function should be used in situations where
  !  buffers are in use and should not be modified, but properties for one
  !  rate id are still needed, such as when checking inverse rates or 
  !  modifing a rate with properties from a different library.
  ! INPUTS
  !  rid: rate ID to retrieve properties for
  !  pcom: cina_common variable needed for reporting errors
  !  prop_list: list of properties to return.  All properties are returned
  !             if empty.  List should be separated with RATE_INFO_PROP_SEP
  ! RETURN VALUE
  !  List of rate properties for a rate ID.  If library in rid is empty then
  !  an empty string is returned and no error is generated.  This prevents
  !  errors when reading rate info for a new rate.
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/15/2004 jpscott       Start function
  ! SOURCE

  FUNCTION read_rate_info_no_buf(rid,pcom,prop_list)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: rid,prop_list
    CHARACTER(LEN=MAX_RATE_LEN)      :: read_rate_info_no_buf
    OPTIONAL                         :: prop_list

    CHARACTER(LEN=MAX_RLIB_LEN)      :: library
    CHARACTER(LEN=MAX_RID_LEN)       :: rids(MAX_RATE_INFO)
    INTEGER(KIND=4)                  :: num_rids,i,z,a,seek
    INTEGER(KIND=4)                  :: seek_pos(MAX_RATE_INFO)

    CALL decode_rid(rid,library,z,a)

    CALL read_rid_list(library,pcom,z,a,rids,num_rids,seek_pos, &
         buffer=.FALSE.)

    ! Find seek_pos for rid by searching in rids
    seek = -1
    i = 1
    DO WHILE (seek < 0)
       IF (rids(i) == rid) seek = seek_pos(i)
       i = i + 1
       IF (i > num_rids .AND. seek < 0) CALL report_error( &
            'Can not find "' // TRIM(rid) // '" in read_rate_info_no_buf.',&
            'Improbable',pcom,1)
    END DO

    IF (PRESENT(prop_list)) THEN
       read_rate_info_no_buf = read_rate_info(rid,pcom,prop_list,seek, &
            unit=RLIB_RATE_INFO_UNIT_NO_BUF)
    ELSE
       read_rate_info_no_buf = read_rate_info(rid,pcom,'',seek, &
            unit=RLIB_RATE_INFO_UNIT_NO_BUF)
    END IF
  END FUNCTION read_rate_info_no_buf
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/clear_rate_info
  ! NAME
  !  SUBROUTINE clear_rate_info()
  ! PURPOSE
  !  Clear rate_file buffer
  ! STATUS
  !  Complete
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  11/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE clear_rate_info()
    IMPLICIT NONE
    rate_file%library = ''
    rate_file%z = -1
    rate_file%a = -1
    rate_file%rid_num = 0
    rate_file%rids = ''
    rate_file%prop = ''
  END SUBROUTINE clear_rate_info
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/update_rate_prop
  ! NAME
  !  SUBROUTINE update_rate_prop(prop_list,prop_mod_list)
  ! PURPOSE
  !  Update prop_list with values from prop_mod_list
  ! STATUS
  !  Complete
  ! INPUTS
  !  prop_list: List of rate info properties to update
  !  prop_mod_list: List of new values of rate info properties
  ! OUTPUTS
  !  prop_list: Updated list of rate info properties
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE update_rate_prop(prop_list,prop_mod_list)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: prop_mod_list
    CHARACTER(LEN=*),INTENT(INOUT)   :: prop_list
    CHARACTER(LEN=MAX_RATE_LEN)      :: prop,mod_list
    INTEGER(KIND=4)                  :: name_end,value_start
    LOGICAL(KIND=4)                  :: loop

    ! Copy prop_mod_list to mod_list because prop_mod_list can't be modified
    mod_list = prop_mod_list

    loop = .TRUE.
    DO WHILE (loop)
       ! Get next prop name/value pair
       loop = next_in_list(prop,mod_list,RATE_INFO_PROP_SEP)
       ! Break prop into a name and value
       name_end = INDEX(prop,RATE_INFO_VAL_SEP) - 1

       IF (name_end > 0) THEN
          value_start = name_end + LEN(RATE_INFO_VAL_SEP) + 1
          CALL set_prop_value(prop(:name_end),prop(value_start:), &
               prop_list,RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
       END IF
    END DO
  END SUBROUTINE update_rate_prop
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/save_rate_info_internal
  ! NAME
  !  SUBROUTINE save_rate_info_internal(pcom,exist,report,lib_info,
  !                                     log_level,inv_rates)
  ! PURPOSE
  !  Save changes in rate_file to isotope file on disk
  ! STATUS
  !  Complete
  ! CAUTION
  !  This should only be called from save_rate_info because it performs all
  !  error checking and preparation tasks.
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  !  exist: TRUE if iso file existed before calling save_rate_info
  !  report: string containing existing report
  !  lib_info: string containing existing lib_info
  !  log_level: controls the level of detail in the report and lib_info.  
  !             Should be 2 for library management, otherwise rate
  !             management detail is assumed.
  !  inv_rates: 0 if inverse flag should not be checked (DEFAULT)
  !             1 if saving forward rates now and inverses next
  !             2 if saving inverse rates now and forward before
  ! OUTPUTS
  !  report: string with new report appended to existing report
  !  lib_info: string with new lib_info appended to existing lib_info
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE save_rate_info_internal(pcom,exist,report,lib_info,log_level,inv_rates)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(INOUT)   :: report,lib_info
    LOGICAL(KIND=4),INTENT(IN)       :: exist
    INTEGER(KIND=4),INTENT(IN)       :: log_level,inv_rates
    CHARACTER(LEN=MAX_RLIB_LEN)      :: lib,old_lib
    CHARACTER(LEN=MAX_RID_LEN)       :: rid,next_rid,old_rid
    CHARACTER(LEN=MAX_RATE_LEN)      :: properties,new_lib_info
    CHARACTER(LEN=80)                :: reac_str,inv_str
    INTEGER(KIND=4)                  :: status,rid_index,rid_list_index
    LOGICAL(KIND=4)                  :: link

    inv_str = ''
    IF (inv_rates == 2) THEN
       inv_str = ' inverse'
       ! Make sure Reaction Type has v flag
       DO rid_index = 1, rate_file%rid_num
          ! Flag this reaction as an inverse rate
          reac_str = get_prop_value('Reaction Type',rate_file%prop(rid_index), &
               RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
          status = INDEX(','//TRIM(reac_str)//',',',v,')
          IF (status < 1) THEN
             !print *,'flagging forward as inverse'
             !print *,TRIM(reac_str)
             reac_str = TRIM(reac_str) // ',v'
             !print *,TRIM(reac_str)
             CALL set_prop_value('Reaction Type',reac_str, &
                  rate_file%prop(rid_index),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
             !print '(2A)','props=',TRIM(rate_file%prop(rid_index))
          END IF
       END DO
    ELSE IF (inv_rates == 1) THEN
       ! Remove all v flags from Reaction Type
       DO rid_index = 1, rate_file%rid_num
          ! Flag this reaction as a forward rate
          reac_str = get_prop_value('Reaction Type',rate_file%prop(rid_index), &
               RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
          status = INDEX(','//TRIM(reac_str)//',',',v,')
          IF (status > 0) THEN
             !print *,'flagging inverse as forward'
             !print *,TRIM(reac_str)
             WRITE(reac_str,'(A)') reac_str(:status-2)//reac_str(status+1:)
             !print *,TRIM(reac_str)
             CALL set_prop_value('Reaction Type',reac_str, &
                  rate_file%prop(rid_index),RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
             !print '(2A)','props=',TRIM(rate_file%prop(rid_index))
          END IF
       END DO
    END IF

    ! rid_index is an index into rate_file%rids(rid_index)
    ! rid_list_index is an index into rid_list(rid_list_index)

    ! Initializations
    IF (exist) THEN
       status = 0
       rid = ''
    ELSE
       status = -1  ! If file doesn't exist don't try to read it in
    END IF

    ! Setup rid_list for this isotope and library
    rid_list_index = find_rlib_rid_list(rate_file%library)
    IF (rid_list_index <= 0) THEN
       ! Make new entry in rid_list
       rid_list_index = get_rid_list_number() + 1
       IF (rid_list_index > MAX_RLIBS) CALL report_error('rid_list ' // &
            ' is too full to hold new entry when saving rate info for ' // &
            TRIM(rate_file%library),'Improbable',pcom,1)
    END IF
    ! Fill rid_list entry values
    rid_list(rid_list_index)%library = rate_file%library
    rid_list(rid_list_index)%z = rate_file%z
    rid_list(rid_list_index)%a = rate_file%a
    rid_list(rid_list_index)%rid_num = 0
    rid_list(rid_list_index)%rid = ''
    rid_list(rid_list_index)%seek_pos = -1

    ! Initialize for read_rate_prop
    next_rid = ''
    new_lib_info = ''

    ! Read old .rate file, make proper changes, and copy it to the new one
    DO WHILE (status == 0)
       CALL read_rate_prop(pcom,rid,next_rid,properties,status)
!!$       print '(/A,I0,6A)','status=',status,' " rid=',TRIM(rid), &
!!$            ' next_rid=',TRIM(next_rid),' properties=',TRIM(properties)

       ! Search for reaction in rate_file
       rid_index = find_reac_rate_file(rid)
       IF (rid_index > 0) THEN
!!$          print '(A,I0)','rid_index=',rid_index
          ! Update rid to new one (they will differ if one is a link)
          rid = rate_file%rids(rid_index)
          ! Mark this rid as saved
          rate_file%rids(rid_index) = ''

          CALL decode_rid(rid,old_lib)

          ! Verify that library name in rate id is either a PUBLIC library
          ! or this library
          ! link is TRUE if a link to a public library
          old_rid = rid
          CALL correct_rid(rid,rate_file%library,link)
          CALL decode_rid(rid,lib,unique_reac_str=reac_str)

          ! Erase properties if this library is a link
          IF (link) THEN
             properties = ''
             lib = rate_file%library
          ELSE
             properties = rate_file%prop(rid_index)
             CALL set_prop_value('Creation Date',get_date() // ' ' // &
                  get_time(),properties,RATE_INFO_PROP_SEP, &
                  RATE_INFO_VAL_SEP)
          END IF
!!$          print '(3A)','properties="',TRIM(properties),'"'

          ! Add to report and lib_info
          IF (old_lib == '' .AND. log_level < 2) THEN
             report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                  TRIM(lib) // ' was replaced with new' // &
                  TRIM(inv_str) // ' rate' // ACHAR(8)
             new_lib_info = TRIM(new_lib_info) // ', replaced ' // &
                  TRIM(reac_str) // ' with new' // TRIM(inv_str) // &
                  ' rate'
          ELSE IF (log_level < 2) THEN
             report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                  TRIM(lib) // ' was replaced with' // &
                  TRIM(inv_str) // ' rate from ' // TRIM(old_lib) // ACHAR(8)
             new_lib_info = TRIM(new_lib_info) // ', replaced ' // &
                  TRIM(reac_str) // ' with' // TRIM(inv_str) // &
                  ' rate from ' // TRIM(old_lib)
          END IF
       ELSE
          ! reaction is not in rate_file so just copy it to output file
          
          old_rid = rid

          ! Set the value of link needed for write_rate_info
          CALL correct_rid(rid,rate_file%library,link)

          ! Check if existing properties are about to be deleted because
          ! this reaction is now a link.  This might happen if a PUBLIC 
          ! library is created with the name of an old USER library.  This
          ! checks if a user's rate is about to be replaced with a link to a
          ! PUBLIC library.
          IF (link .AND. properties /= '') THEN
             CALL decode_rid(old_rid,old_lib)             
             CALL decode_rid(rid,lib,unique_reac_str=reac_str)
             CALL report_error(TRIM(reac_str) // 'in ' // TRIM(old_lib) // &
                  'has a minor problem and the operation you requested' // &
                  ' has been aborted to prevent any data loss.  The ' // &
                  'nucastrodata coordinator will fix the problem and ' // &
                  'should email you the next business day with more ' // &
                  'information.','Improbable',pcom,1)
          END IF
       END IF

       ! Write changes to disk and rid_list
       CALL write_rate_info(pcom,rid,properties,rid_list_index,link)
    END DO

!!$    print *,'Adding new rates'
    ! Add new rids to .rate file
    DO rid_index = 1, rate_file%rid_num
       rid = rate_file%rids(rid_index)
       IF (rid /= '') THEN
          properties = rate_file%prop(rid_index)
          CALL decode_rid(rid,old_lib)

          ! Verify that library name in rate id is either a PUBLIC library
          ! or this library
          ! link is TRUE if a link to a public library
          old_rid = rid
          CALL correct_rid(rid,rate_file%library,link)

          ! Add to report and lib_info
          CALL decode_rid(rid,lib,unique_reac_str=reac_str)

          ! Load properties from source lib in rid if necessary
          IF (link) THEN
             properties = ''
             lib = rate_file%library
          ELSE 
             CALL set_prop_value('Creation Date',get_date() // ' ' // &
                  get_time(),properties,RATE_INFO_PROP_SEP, &
                  RATE_INFO_VAL_SEP)
          END IF
!!$          print '(5A)','rid=',TRIM(rid),' new props="', &
!!$               TRIM(properties),'"'

          ! Add to report and new_lib_info
          IF (old_lib == '' .AND. log_level < 2) THEN
             report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                  TRIM(lib) // ' was added as a new' // TRIM(inv_str) // &
                  ' rate' // ACHAR(8)
             new_lib_info = TRIM(new_lib_info) // ', added' // &
                  TRIM(inv_str) // ' ' // TRIM(reac_str)
          ELSE IF (log_level < 2) THEN
             report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                  TRIM(lib) // ' was copied from ' // TRIM(old_lib)
             new_lib_info = TRIM(new_lib_info) // ', copied ' // &
                  TRIM(reac_str) // ' from ' // TRIM(old_lib)
             IF (inv_rates == 2) THEN
                report = TRIM(report) // ' as an inverse' // ACHAR(8)
                new_lib_info = TRIM(new_lib_info) // ' as an inverse'
             ELSE
                report = TRIM(report) // ACHAR(8)
             END IF
          END IF

          ! Write changes to disk and rid_list
          CALL write_rate_info(pcom,rid,properties,rid_list_index,link)
       END IF
    END DO

    ! Save new_lib_info to lib_info
    new_lib_info = TRIM(get_prop_value('Library Recipe',lib_info, &
         RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)) // new_lib_info

    CALL set_prop_value('Library Recipe',new_lib_info,lib_info, &
         RLIB_INFO_PROP_SEP,RLIB_INFO_VAL_SEP)
  END SUBROUTINE save_rate_info_internal
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/read_rate_prop
  ! NAME
  !  SUBROUTINE read_rate_prop(pcom,rid,next_rid,properties,status)
  ! PURPOSE
  !  Read rate properties from an old .rate file for one rate ID
  ! STATUS
  !  Complete
  ! CAUTION
  !  This should only be called from save_rate_info_internal because it
  !  performs all error checking and preparations tasks.
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  !  next_rid: storage of next rate ID in file.  This should not be modified
  !            by calling procedure and is used for files with multiple
  !            rates.  next_rid should be set to '' before calling this
  !            subroutine the first time.
  ! OUTPUTS
  !  rid: Rate ID that was read in
  !  next_rid: storage of next rate ID in file.  This should not be modified
  !            by calling procedure and is used for files with multiple
  !            rates.  This output is used as input for the next call to
  !            this subroutine.
  !  properties: Properties for rate ID that was read in
  !  status: 0 is returned if successful, -1 is returned if the rate 
  !          returned was the last one in the file.
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE read_rate_prop(pcom,rid,next_rid,properties,status)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(INOUT)   :: next_rid
    CHARACTER(LEN=*),INTENT(OUT)     :: rid,properties
    INTEGER(KIND=4),INTENT(OUT)      :: status
    CHARACTER(LEN=MAX_RATE_LEN)      :: line

    rid = next_rid
    status = 0
    properties = ''

    DO WHILE (status == 0)
       READ(RLIB_RATE_INFO_OLD_UNIT,'(A)',IOSTAT=status) line

       IF (status > 0) THEN
          CALL report_error('Error reading line from old .rate file.', &
               'File input/output',pcom,1)
       ELSE IF (status /= 0) THEN
          ! EOF reached
          status = -1
          next_rid = 'EOF'
          ! Remove initial property separator from properties
          properties = properties(LEN(RATE_INFO_PROP_SEP)+1:)
          RETURN
       END IF

       ! Check if a rate ID or a property
       IF ((INDEX(line,ACHAR(11)) > 0) .AND. &
            (INDEX(line,RATE_INFO_VAL_SEP) == 0)) THEN
          ! line is a rate ID
          ! If this is the first rate in a file, continue reading to get the
          ! properties, and the next rate ID
          IF (next_rid == '') THEN
             rid = line
             ! Set next_rid to something that isn't empty so that next case
             ! will be TRUE and subroutine will return.
             next_rid = line
          ELSE
             next_rid = line
             ! Remove initial property separator from properties
             properties = properties(LEN(RATE_INFO_PROP_SEP)+1:)
             RETURN
          END IF
       ELSE 
          ! line is a rate property
          properties = TRIM(properties) // RATE_INFO_PROP_SEP // &
          line2str(TRIM(line))
       END IF
    END DO
  END SUBROUTINE read_rate_prop
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_Info/find_reac_rate_file
  ! NAME
  !  FUNCTION find_reac_rate_file(rid)
  ! PURPOSE
  !  Search for a reaction in rate_file
  ! STATUS
  !  Complete
  ! INPUTS
  !  rid: rate ID to extract reaction and unique string from
  ! RETURN VALUE
  !  Index to a rate ID in rate_file with the same reaction and unique 
  !  string if found.  Otherwise -1 is returned
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/01/2004 jpscott       Start subroutine
  ! SOURCE

  FUNCTION find_reac_rate_file(rid)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: rid
    CHARACTER(LEN=MAX_RID_LEN)       :: rid2
    INTEGER(KIND=4)                  :: find_reac_rate_file
    INTEGER(KIND=4)                  :: reac_str_start,i,start2

    ! Default return value if error
    find_reac_rate_file = -1

    reac_str_start = INDEX(rid,ACHAR(9)) + 1

    ! Return if no reaction string found in rid (invalid rid)
    IF (reac_str_start < 9) RETURN

    DO i = 1, rate_file%rid_num
       rid2 = rate_file%rids(i)
       start2 = INDEX(rid2,ACHAR(9)) + 1
       IF (start2 >= 9) THEN
          ! Check if reaction and unique strings are the same
          IF (rid(reac_str_start:) == rid2(start2:)) THEN
             find_reac_rate_file = i
             RETURN
          END IF
       END IF
    END DO
  END FUNCTION find_reac_rate_file
  !***

  !-------------------------------------------------------------------------
  !****is* Rate_Info/write_rate_info
  ! NAME
  !  SUBROUTINE write_rate_info(pcom,rid,properties,rid_list_index,link)
  ! PURPOSE
  !  Write rate properties to disk and rid_list
  ! STATUS
  !  Complete
  ! CAUTION
  !  This should only be called from save_rate_info_internal because it
  !  performs all error checking and preparation tasks.
  ! INPUTS
  !  pcom: cina_common variable needed for reporting errors
  !  rid: rate ID to save to disk
  !  properties: properties of rate ID to save to disk
  !  rid_list_index: index into library entry in rid_list
  !  link: TRUE is rid is a link
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  09/02/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE write_rate_info(pcom,rid,properties,rid_list_index,link)
    USE io
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN)    :: pcom
    CHARACTER(LEN=*),INTENT(IN)      :: rid,properties
    INTEGER(KIND=4),INTENT(IN)       :: rid_list_index
    LOGICAL(KIND=4),INTENT(IN)       :: link
    CHARACTER(LEN=MAX_RATE_LEN)      :: prop,prop_copy
    INTEGER(KIND=4)                  :: seek_pos,status,rid_index
    LOGICAL(KIND=4)                  :: loop

    ! Get seek position needed for saving rid_list
    seek_pos = FTELL(RLIB_RATE_INFO_UNIT)
    ! Write rid to new .rate file
    WRITE(RLIB_RATE_INFO_UNIT,'(A)',IOSTAT=status) TRIM(rid)
    IF (status /= 0) CALL report_error('Can not write id to new .rate ' // &
         'file','File input/output',pcom,1)

    ! Add rid to rid_list
    rid_index = rid_list(rid_list_index)%rid_num + 1
    IF (rid_index > MAX_RATES) CALL report_error('MAX_RATES is too ' // &
         'small to hold all rates.','Developer Reminder',pcom,1)
    rid_list(rid_list_index)%rid(rid_index) = rid
    rid_list(rid_list_index)%seek_pos(rid_index) = seek_pos
    rid_list(rid_list_index)%rid_num = rid_index

    ! Make copy of properties because properties can't be modified
    prop_copy = properties

    ! Write properties to new .rate file
    loop = .NOT. link
    DO WHILE (loop)
       loop = next_in_list(prop,prop_copy,RATE_INFO_PROP_SEP)
       prop = str2line(TRIM(prop))
       CALL print_long_string(TRIM(prop),IOSTAT=status, &
            unit=RLIB_RATE_INFO_UNIT)
       IF (status /= 0) CALL report_error('Can not write property ' // &
            'to new .rate file','File input/output',pcom,1)
    END DO
  END SUBROUTINE write_rate_info
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_Man_Core_Misc/get_rlib_path
  ! NAME
  !  FUNCTION get_rlib_path(library,user,group_name)
  ! PURPOSE
  !  Return a UNIX path and library group name of a rate library
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) before calling
  !  this subroutine.
  ! INPUTS
  !  library: Library name
  !  user: Username (needed for accessing correct users libraries)
  ! OUTPUTS
  !  group_name (OPTIONAL): Name of library group that library belongs to if
  !                         library was found, otherwise an empty string
  ! RETURN VALUE
  !  UNIX path to library root if found, otherwise empty.  path will always
  !  end with '/'
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/20/2004 jpscott       Start function
  ! SOURCE

  FUNCTION get_rlib_path(library,user,group_name)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: library,user
    CHARACTER(LEN=*),INTENT(OUT)  :: group_name
    CHARACTER(LEN=MAX_PATH_LEN)   :: get_rlib_path
    CHARACTER(LEN=MAX_RGRP_LEN)   :: group
    OPTIONAL                      :: group_name

    ! Get library group
    CALL find_rlib_list_index(library,group)

    ! Check if library was found
    IF (group == '') THEN
       IF (PRESENT(group_name)) group_name = ''
       get_rlib_path = ''
       RETURN
    END IF

    SELECT CASE (group)
    CASE ('USER ')
       get_rlib_path = TRIM(CINA_PATH) // 'USER/' // &
            TRIM(user) // '/rate_libs/' // TRIM(library) // '/'
    CASE DEFAULT
       get_rlib_path = TRIM(CINA_PATH) // TRIM(group) // &
            '/rate_libs/' // TRIM(library) // '/'
    END SELECT
    IF (PRESENT(group_name)) group_name = group
  END FUNCTION get_rlib_path
  !***

  !-------------------------------------------------------------------------
  !****if* Rate_Man_Core_Misc/get_riso_path
  ! NAME
  !  FUNCTION get_riso_path(library,z,a,type,user,lib_path)
  ! PURPOSE
  !  Return a UNIX path to an isotope file (rid list or rate info) for a 
  !  rate library
  ! STATUS
  !  Complete
  ! CAUTION
  !  Make sure rate library lists are loaded (read_rlib_list) before calling
  !  this subroutine (unless lib_path is used).  If lib_path is present, it
  !  should end with '/'
  ! INPUTS
  !  library: Library name
  !  z: Number of protons of isotope
  !  a: Atomic mass of isotope
  !  type: Type of file to use and should be 'rid' or 'iso'
  !  user: Username (needed for accessing correct users libraries)
  !  lib_path (OPTIONAL): Path to library instead of using get_rlib_path
  ! RETURN VALUE
  !  UNIX path to library isotope file if library is found in rlib_list, 
  !  otherwise empty
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start function
  ! SOURCE

  FUNCTION get_riso_path(library,z,a,type,user,lib_path)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: library,type,lib_path,user
    INTEGER(KIND=4),INTENT(IN)    :: z,a
    CHARACTER(LEN=MAX_PATH_LEN)   :: get_riso_path
    OPTIONAL                      :: lib_path

    IF (PRESENT(lib_path)) THEN
       WRITE(get_riso_path,'(A,I0,2A,I0,A,I0,A)') TRIM(lib_path),z,'/', &
            TRIM(type),z,'_',a,'.rate'
    ELSE
       ! Get library path
       get_riso_path = get_rlib_path(library,user)
       ! Return blank string if library was not found
       IF (get_riso_path == '') RETURN

       ! Add suffix
       WRITE(get_riso_path,'(A,I0,2A,I0,A,I0,A)') &
            TRIM(get_riso_path),z,'/',TRIM(type),z,'_',a,'.rate'
    END IF
  END FUNCTION get_riso_path
  !***

  !---------------------------------------------------------------------
  !****is* Rate_Man_Core_Misc/get_parm_array
  ! NAME
  !  SUBROUTINE get_parm_array(properties,a,a_num,a_num_actual)
  ! PURPOSE
  !  Put the parameters in properties into an array
  ! STATUS
  !  Complete
  ! CAUTION
  !  a_num will be negative if an error occurs.  It's value indicates which
  !  parameter had a problem.  0 is returned if "Number of Parameters" can
  !  not be read as an integer.
  !  
  !  This function uses the "Parameters" and "Number of Parameters" 
  !  properties in properties.
  ! INPUTS
  !  properties: list of rate properties to obtain parameters from
  ! OUTPUTS
  !  a: array of parameters
  !  a_num: value of "Number of Parameters" property, 0 if invalid, or 
  !         negative if an error occurs reading the Parameters property
  !  a_num_actual (OPTIONAL): number of parameters found in Parameters
  !                           property regardless of the 
  !                           "Number of Parameters" property
  ! AUTHORS
  !  Jason Scott (jpscott@mail.phy.ornl.gov)
  ! MODIFICATION HISTORY
  !  08/30/2004 jpscott       Start subroutine
  ! SOURCE

  SUBROUTINE get_parm_array(properties,a,a_num,a_num_actual)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: properties
    REAL(KIND=8),INTENT(OUT)      :: a(:)
    INTEGER(KIND=4),INTENT(OUT)   :: a_num,a_num_actual
    OPTIONAL                      :: a_num_actual
    CHARACTER(LEN=MAX_RATE_LEN)   :: value,parm
    INTEGER(KIND=4)               :: status,a_num2
    LOGICAL(KIND=4)               :: loop

    ! Default value returned
    a = 0d0

    value = get_prop_value('Parameters',properties, &
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)

    IF (value == '') THEN
       a_num = 0
       RETURN
    END IF

    loop = .TRUE.
    a_num = 0
    DO WHILE (loop)
       loop = next_in_list(parm,value,',')
       a_num = a_num + 1
       IF (a_num > UBOUND(a,1)) THEN
          a_num = -a_num
          RETURN
       END IF
       READ(parm,'(G)',IOSTAT=status) a(a_num)
       IF (status /= 0) THEN
          a_num = -a_num
          RETURN
       END IF
    END DO

    value = get_prop_value('Number of Parameters',properties, &
         RATE_INFO_PROP_SEP,RATE_INFO_VAL_SEP)
    READ(value,'(I)',IOSTAT=status) a_num2
    IF (status /= 0) THEN
       a_num = 0
    ELSE
       IF (PRESENT(a_num_actual)) a_num_actual = a_num
       a_num = a_num2
    END IF
  END SUBROUTINE get_parm_array
  !***

END MODULE rate_man_core
