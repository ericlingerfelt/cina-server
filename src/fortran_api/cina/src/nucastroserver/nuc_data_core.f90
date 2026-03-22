       MODULE nuc_data_core
! PURPOSE
!  This module contains fundamental procedures, data types, and buffers for 
!  nuclear data management.
! USES
!  cina_core, nucastrolib
! SEE ALSO
!  nuc_data_man module
!  
!  nuc_data_core sections:
!    Nuc_Data_Set_Lists, Nuc_Data_Isotope_Lists, Nuc_Data_IDs, 
!    Nuc_Data_Set_Info, Nuc_Data_Info, 
!    Nuc_Data_Core_Misc
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
      
! Public Nuc_Data_Set_Lists procedures
        PUBLIC :: read_NDS_list, save_NDS_list, add_NDS, rm_NDS
        PUBLIC :: get_NDS_group_number, find_NDS_group_index
        PUBLIC :: find_NDS_list_index
! Public Nuc_Data_Isotope_Lists procedures
        PUBLIC :: read_NDS_iso_list, save_NDS_iso_list, NDS_add_isotope
        PUBLIC :: NDS_isotope_exist, get_NDS_iso_list_number, find_NDS_iso_list
! Public Nuc_Data_IDs procedures
        PUBLIC :: read_NDid_list, save_NDid_list, decode_NDid, encode_NDid
        PUBLIC :: find_NDS_NDid_list, get_NDS_list_number, correct_NDid
        PUBLIC :: NDid_exist
! Public Nuc_Data_Set_Info procedures
        PUBLIC :: read_NDS_info, save_NDS_info
! Public Nuc_Data_Info procedures
        PUBLIC :: read_ND_info, save_ND_info, open_ND_info, change_ND_info
        PUBLIC :: read_ND_info_no_buf, update_ND_prop
! Public Nuc_Data_Core_Misc procedures
        PUBLIC :: nuc_data_core_ver, get_NDS_path, get_NDS_iso_path
      
! Parameters defining maximums for rate management
! Max # of rate groups in buffer
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_NDS_GROUPS = 4
!!$
!!$  ! Max # of libraries in a group
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_NDS_IN_GRP = 50
!!$
!!$  ! Max length of rate group name
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_NDS_GRP_LEN = 10
!!$
!!$  ! Max length of rate library name
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_NDS_LEN = 40
!!$
!!$  ! Max # of open rate libraries
!!$  INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_NDS = 8
!!$
!!$  ! Max # of rates per library to buffer in rate list
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND = 120
!!$
!!$  ! Max length of rate id
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND_ID_LEN = 200
!!$
!!$  ! Max length of rate property string
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND_LEN = 32767
!!$
!!$  ! Max # of rate property pairs in buffer
!!$  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND_INFO = MAX_ND
!!$
!!$  !----------------------------------------------------------------------
!!$  ! DEVELOPMENT Parameters defining maximums for rate management
        INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_NDS_GROUPS = 3
        INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_NDS_IN_GRP = 100
        INTEGER(KIND=1),PARAMETER,PUBLIC   :: MAX_NDS_GRP_LEN = 10
        INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_NDS_LEN = 100
        INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_NDS = 100
        INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND = 200
        INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND_ID_LEN = 200
        INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND_LEN = 20480
        INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ND_INFO = MAX_ND
        
! Unit numbers used for files in this module
        INTEGER(KIND=4),PARAMETER          :: NDS_LIST_UNIT = 56
        INTEGER(KIND=4),PARAMETER          :: NDS_INFO_UNIT = 57
        INTEGER(KIND=4),PARAMETER          :: NDS_ISO_LIST_UNIT = 58
        INTEGER(KIND=4),PARAMETER          :: ND_ID_LIST_UNIT = 59
        INTEGER(KIND=4),PARAMETER          :: ND_INFO_UNIT = 55
        INTEGER(KIND=4),PARAMETER          :: ND_INFO_OLD_UNIT = 54
        INTEGER(KIND=4),PARAMETER          :: ND_INFO_UNIT_NO_BUF = 53
      
! Deliminators for Rate Library Info Properties
        CHARACTER(LEN=1),PARAMETER,PUBLIC  :: NDS_INFO_PROP_SEP = ACHAR(9)
        CHARACTER(LEN=1),PARAMETER,PUBLIC  :: NDS_INFO_VAL_SEP = '='
        CHARACTER(LEN=1),PARAMETER,PUBLIC  :: ND_INFO_PROP_SEP = ACHAR(9)
        CHARACTER(LEN=3),PARAMETER,PUBLIC  :: ND_INFO_VAL_SEP = ' = '
        
      
!***
!****in* nuc_data_core/Nuc_Data_Set_Lists
! PURPOSE
!  Group procedures and data types for managing nuclear data set lists and
!  groups into one section
! SEE ALSO
!  DATA TYPES: NDS_list_buffer
!  
!  VARIABLES: NDS_list
!  
!  PUBLIC PROCEDURES: read_NDS_list, save_NDS_list, add_NDS, rm_NDS
!                     get_NDS_group_number, find_NDS_group_index
!                     find_NDS_list_index
!  
!  Note: NDS_list is a nuc_data_core variable of type NDS_list_buffer
!***
      
!****in* nuc_data_core/Nuc_Data_Isotope_Lists
! PURPOSE
!  Group procedures and data types for managing nuclear data set isotope
!  lists into one section
! SEE ALSO
!  DATA TYPES: NDS_iso_list_buffer
!  
!  VARIABLES: NDS_iso_list
!  
!  PUBLIC PROCEDURES: read_NDS_iso_list, save_NDS_iso_list, 
!                     NDS_add_isotope, NDS_isotope_exist, 
!                     get_NDS_iso_list_number, find_NDS_iso_list
!  
!  PRIVATE PROCEDURES: get_NDS_sorted_a_list
!  
!  Note: NDS_iso_list is a nuc_data_core variable of type 
!        NDS_iso_list_buffer
!***
        
!****in* nuc_data_core/Nuc_Data_IDs
! PURPOSE
!  Group procedures and data types for managing nuclear data IDs into one
!  section
! SEE ALSO
!  DATA TYPES: NDid_list_buffer
!  
!  VARIABLES: NDid_list
!  
!  PUBLIC PROCEDURS: read_NDid_list, save_NDid_list, decode_NDid,
!                    encode_NDid, find_NDS_NDid_list, get_NDS_list_number,
!                    correct_NDid, NDid_exist
!  
!  PRIVATE PROCEDURES: get_NDid_seek
!  
!  Note: rid_list is a nuc_data_core variable of type NDid_list_buffer
!  save_rid_list should not be called because it is called from 
!  save_rate_info
!***
      
!****in* nuc_data_core/Nuc_Data_Set_Info
! PURPOSE
!  Group procedures for managing nuclear data set info into one section
! SEE ALSO
!  PUBLIC PROCEDURES: read_NDS_info, save_NDS_info
!  
!  Use get_prop_value and set_prop_value from convert module for reading
!  and modifying nuclear data set info properties.
!***
        
!****in* nuc_data_core/Nuc_Data_Info
! PURPOSE
!  Group procedures for managing nuclear data info into one section
! SEE ALSO
!  DATA TYPES: save_ND_info_buffer
!  
!  VARIABLES: ND_file
!  
!  PUBLIC PROCEDURES: read_ND_info, save_ND_info, open_ND_info,
!                     change_ND_info, read_ND_info_no_buf, update_ND_prop
!  
!  PRIVATE PROCEDURES: save_ND_info_internal, read_ND_prop,
!                      find_reac_ND_file, write_ND_info
!  
!  Note that ND_file is a nuc_data_core variable of type
!  save_ND_info_buffer.
!  
!  Note that open_ND_info and change_ND_info are used before 
!  save_ND_info for saving multiple rates for one isotope at one time
!  
!  Use get_prop_value and set_prop_value from convert module for reading
!  and modifying ND info properties.
!***
      
!****in* nuc_data_core/Nuc_Data_Core_Misc
! PURPOSE
!  Group all other core nuclear data management procedures into one
!  section
! SEE ALSO
!  PUBLIC PROCEDURES: nuc_data_core_ver, get_NDS_path, get_NDS_iso_path
!***
      
!****it* Nuc_Data_Set_Lists/NDS_list_buffer
! NAME
!  TYPE NDS_list_buffer
! PURPOSE
!  Stores lists of libraries for one rate library group
! SOURCE
        
        TYPE,PRIVATE                       :: NDS_list_buffer
! Nothing outside this module can access these types or components
           PRIVATE 
! Holds the library group name
           CHARACTER(LEN=MAX_NDS_GRP_LEN)  :: group = ''
      
! Holds library names
           CHARACTER(LEN=MAX_NDS_LEN)      :: set(MAX_NDS_IN_GRP) = ''
      
! Holds the number of libraries
           INTEGER(KIND=1)                 :: num_sets = -1
        END TYPE NDS_list_buffer
!***
      
!****it* Nuc_Data_Isotope_Lists/NDS_iso_list_buffer
! NAME
!  TYPE riso_list_buffer
! PURPOSE
!  Stores lists of isotopes for one rate library
! SOURCE
        
        TYPE,PRIVATE                       :: NDS_iso_list_buffer
! Nothing outside this module can access these types or components
! CAUTION, if KIND=1 then Z_MAX may not be higher than 127
! 1st dim is Z, 2nd dim selects the nth isotope, value is atomic mass
           PRIVATE
! Library name
           CHARACTER(LEN=MAX_NDS_LEN)      :: set = ''
      
! isotope list
           INTEGER(KIND=2)                 :: iso_list(0:MAX_Z,MAX_ISO) = -1
      
! T if buffer is same as in isotope file
           LOGICAL(KIND=1)                 :: saved = .TRUE.
        END TYPE NDS_iso_list_buffer
!***
      
!****it* Nuc_Data_IDs/NDid_list_buffer
! NAME
!  TYPE rid_list_buffer
! PURPOSE
!  Stores lists of rates for one isotope in a rate library
! SOURCE
        
        TYPE,PRIVATE                       :: NDid_list_buffer
! Nothing outside this module can access these types or components
           PRIVATE
! Library name
           CHARACTER(LEN=MAX_NDS_LEN)      :: set = ''
      
! Holds Z of isotope
           INTEGER(KIND=2)                 :: z = -1
      
! Holds A of isotope
           INTEGER(KIND=2)                 :: a = -1
      
! Holds number of rate ids
           INTEGER(KIND=2)                 :: NDid_num = -1
      
! Holds rate ids
           CHARACTER(LEN=MAX_ND_ID_LEN)    :: NDid(MAX_ND) = ''
      
! File seek position for rate
           INTEGER(KIND=2)                 :: seek_pos(MAX_ND) = -1
        END TYPE NDid_list_buffer
!***
      
!****it* Nuc_Data_Info/save_ND_info_buffer
! NAME
!  TYPE save_rate_info_buffer
! PURPOSE
!  Stores info for using save_rate_info subroutine
! SOURCE
        
        TYPE,PRIVATE                       :: save_ND_info_buffer
! Nothing outside this module can access these types or components
           PRIVATE
! Library name
           CHARACTER(LEN=MAX_NDS_LEN)      :: set = ''
      
! Holds Z of isotope
           INTEGER(KIND=2)                 :: z = -1
      
! Holds A of isotope
           INTEGER(KIND=2)                 :: a = -1
      
! Holds number of rids in buffer
           INTEGER(KIND=2)                 :: NDid_num = 0
      
! Holds rate ids
           CHARACTER(LEN=MAX_ND_ID_LEN)    :: NDids(MAX_ND_INFO) = ''
      
! Holds rate properties
           CHARACTER(LEN=MAX_ND_LEN)       :: prop(MAX_ND_INFO) = ''
        END TYPE save_ND_info_buffer
!***
      
! Create buffers global to this module
      
!****iv* Nuc_Data_Set_Lists/NDS_list
! NAME
!  VARIABLE rlib_list
! PURPOSE
!  Store list of libraries for multiple rate library groups
! SOURCE
        
        TYPE(NDS_list_buffer)              :: NDS_list(MAX_NDS_GROUPS)
!***
      
!****iv* Nuc_Data_Isotope_Lists/NDS_iso_list
! NAME
!  VARIABLE riso_list
! PURPOSE
!  Store isotope lists for multiple rate libraries
! SOURCE
        
        TYPE(NDS_iso_list_buffer)          :: NDS_iso_list(MAX_NDS)
!***
      
!****iv* Nuc_Data_IDs/NDid_list
! NAME
!  VARIABLE rid_list
! PURPOSE
!  Store rate lists for multiple rate libraries
! SOURCE
        
        TYPE(NDid_list_buffer)             :: NDid_list(MAX_NDS)
!***
      
!****iv* Nuc_Data_Info/ND_file
! NAME
!  VARIABLE rate_file
! PURPOSE
!  Rate info buffer for modifing an isotope
! SOURCE
        
        TYPE(save_ND_info_buffer)          :: ND_file
!***
      
      CONTAINS
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Core_Misc/nuc_data_core_ver
! NAME
!  FUNCTION nuc_data_core_ver()
! PURPOSE
!  Return the cvs revision number for this file
! STATUS
!  Complete
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2004 jpscott       Start function
! SOURCE
      
        FUNCTION nuc_data_core_ver()
          IMPLICIT NONE
          CHARACTER(LEN=10)           :: nuc_data_core_ver
          CHARACTER(LEN=20),PARAMETER :: ND_MAN_C_VER = '$Revision: 1.1.1.1 $'
          
          nuc_data_core_ver = ND_MAN_C_VER(12:LEN_TRIM(ND_MAN_C_VER)-2)
          
        END FUNCTION nuc_data_core_ver
!***
        
!-------------------------------------------------------------------------
!****is* Nuc_Data_Set_Lists/read_NDS_list
! NAME
!  SUBROUTINE read_NDS_list(group,pcom,list_out)
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
      
        SUBROUTINE read_NDS_list(group,pcom,list_out)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN) :: pcom
          CHARACTER(LEN=*),INTENT(IN)   :: group
          CHARACTER(LEN=*),INTENT(OUT)  :: list_out
          CHARACTER(LEN=MAX_PATH_LEN)   :: line
          INTEGER(KIND=4)               :: counter,group_index,status
          OPTIONAL                      :: list_out
      
! See if nuclear data set list is in memory
          group_index = find_NDS_group_index(group)
      
          IF (group_index == -1) THEN
! Find next available group_index
             group_index = get_NDS_group_number() + 1
             IF (group_index > MAX_NDS_GROUPS) CALL report_error('NDS_list ' // &
                  'is full','Developer Reminder',pcom,1)
      
! Set pathname to Nuc_Data_Set_List file
             line = TRIM(CINA_PATH) // TRIM(group) // '/'
      
             SELECT CASE (group)
             CASE ('USER ')
                line = TRIM(line) // TRIM(pcom%USER) // &
                     '/nuc_data/Nuc_Data_Set_List'
      
                OPEN(NDS_LIST_UNIT, FILE=line, IOSTAT=status, ACTION='READ')
                IF (status /= 0) THEN
! Report no sets for user group instead of an error
                   IF (PRESENT(list_out)) list_out = ''
! Save into NDS_list
                   NDS_list(group_index)%num_sets = 0
                   NDS_list(group_index)%group = 'USER'
                   NDS_list(group_index)%set = ''
                   RETURN
                END IF
             CASE DEFAULT
                line = TRIM(line) // '/nuc_data/Nuc_Data_Set_List'
      
                OPEN(NDS_LIST_UNIT, FILE=line, IOSTAT=status, ACTION='READ')
                IF (status /= 0) THEN
                   WRITE(line,'(I0)') status
                   line = TRIM(line) // '(' // TRIM(group) // ')'
                   CALL report_error('Error ' // TRIM(line) // ' opening ' // &
                        'nuclear data set List','File input/output',pcom,1)
                END IF
             END SELECT
             
! Read file and put into NDS_list
             counter = 0
             DO WHILE (status == 0)
                READ(NDS_LIST_UNIT, '(A)', IOSTAT=status) line
                IF (status > 0) THEN
                   CLOSE(NDS_LIST_UNIT)
                   CALL report_error('Error reading nuclear data set list', &
                        'File input/output',pcom,1)
                ELSE IF (status == 0 .AND. line /= '') THEN
                   counter = counter + 1
                   NDS_list(group_index)%set(counter) = line
                END IF
             END DO
             NDS_list(group_index)%num_sets = counter
             NDS_list(group_index)%group = group
             CLOSE(NDS_LIST_UNIT)
          END IF
      
          IF (PRESENT(list_out)) THEN
             list_out = ''
             DO counter = 1, NDS_list(group_index)%num_sets
                list_out = TRIM(list_out) // ACHAR(9) // &
                     NDS_list(group_index)%set(counter)
             END DO
! Remove initial tab
             list_out = list_out(2:)
          END IF
      
        END SUBROUTINE read_NDS_list
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Set_Lists/save_NDS_list
! NAME
!  SUBROUTINE save_NDS_list(group,pcom)
! PURPOSE
!  Save to disk the list of rate libraries for one library group
! STATUS
!  Complete
! INPUTS
!  group: string with group name to save to disk
!  pcom: cina_common derived type needed for reporting errors
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/20/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE save_NDS_list(group,pcom)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: group
          CHARACTER(LEN=MAX_PATH_LEN)      :: path
          INTEGER(KIND=4)                  :: status,group_index,set_index
      
          path = TRIM(CINA_PATH) // TRIM(group) // '/'
      
! Check for permission and set path
          SELECT CASE (group)
          CASE ('USER')
             path = TRIM(path) // TRIM(pcom%USER) // '/'
          CASE ('SHARED')
! Don't modify path
          CASE DEFAULT
             CALL report_error('Permission denied saving ' // TRIM(group) // &
                  ' nuclear data set list','Improper usage',pcom,1)
          END SELECT
          
          path = TRIM(path) // 'nuc_data/Nuc_Data_Set_List'
          group_index = find_NDS_group_index(group)
      
          IF (group_index < 1) CALL report_error('Group ' // TRIM(group) // &
               ' is not in NDS_list','Improper usage',pcom,1)
      
          OPEN(NDS_LIST_UNIT, FILE=path, IOSTAT=status, ACTION='WRITE')
          IF (status /= 0) CALL report_error('Error opening nuclear data ' // &
               'set list for group  ' // TRIM(group),'File input/output',pcom,1)
      
! If file has no sets, print empty line
          IF (NDS_list(group_index)%num_sets < 1) THEN
             WRITE(NDS_LIST_UNIT, '(A)', IOSTAT=status) ''
             IF (status /= 0) CALL report_error('Error writing empty ' // &
                  'nuclear data set list for group ' // TRIM(group), &
                  'File input/output',pcom,1)
          ELSE
             DO set_index = 1, NDS_list(group_index)%num_sets
                WRITE(NDS_LIST_UNIT, '(A)', IOSTAT=status)  &
                     TRIM(NDS_list(group_index)%set(set_index))
                IF (status /= 0) CALL report_error('Error writing nuclear ' // &
                     'data set list for group ' // TRIM(group), &
                     'File input/output',pcom,1)
             END DO
          END IF
          CLOSE(NDS_LIST_UNIT)
        END SUBROUTINE save_NDS_list
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Set_Lists/add_NDS
! NAME
!  SUBROUTINE add_NDS(set,pcom,group,override,mkdir)
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
      
        SUBROUTINE add_NDS(set,pcom,group,override,mkdir)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN) :: pcom
          CHARACTER(LEN=*),INTENT(IN)   :: set,group
          LOGICAL(KIND=4),INTENT(IN)    :: override,mkdir
          INTEGER(KIND=4)               :: set_index,group_index
          LOGICAL(KIND=4)               :: check
          OPTIONAL                      :: override,mkdir
      
          IF (PRESENT(override)) THEN
! check is T when permissions should be checked
             check = .NOT. override
! Log this
             CALL report_error('Override set when adding nuclear data set "' // &
                  TRIM(set) // '" to group ' // TRIM(group),'Warning',pcom,3)
          ELSE
             check = .TRUE.
          END IF
      
          IF (check) THEN
! Check for permissions
             SELECT CASE (group)
             CASE ('USER ')
! Do nothing
             CASE DEFAULT
                CALL report_error('New nuclear data sets may not be saved in' // &
                     ' the rate set group "' // TRIM(group) // '"', &
                     'Improper usage',pcom,1)
             END SELECT
          END IF
      
          group_index = find_NDS_group_index(group)
      
          IF (group_index < 1) THEN
! Group is not in rlib_list
! Find next empty group_index
             group_index = get_NDS_group_number() + 1
             IF (group_index > MAX_NDS_GROUPS) CALL report_error('NDS_list ' // &
                  'is too small to hold new group.','Developer Reminder',pcom,1)
      
! Add empty group to rlib_list
             NDS_list(group_index)%group = 'USER'
             NDS_list(group_index)%num_sets = 0
          END IF
      
! Check if set is already in NDS_list.  If so, skip this section
          CALL find_NDS_list_index(set,SET_INDEX=set_index)
          IF (set_index <= 0) THEN
! Add user set entry
             set_index = NDS_list(group_index)%num_sets + 1
             IF (set_index > MAX_NDS_IN_GRP) CALL report_error( &
                  'MAX_NDS_IN_GRP is too small to hold new set', &
                  'Developer reminder',pcom,1)
             NDS_list(group_index)%num_sets = set_index
             NDS_list(group_index)%set(set_index) = set
          END IF
          
          IF (PRESENT(mkdir)) THEN
             IF (mkdir) THEN
! Make directory for new set
                SELECT CASE (group)
                CASE ('USER')
                   set_index = safe_shell('/bin/mkdir -p ''' // &
                        TRIM(CINA_PATH) // 'USER/' // TRIM(pcom%USER) // &
                        '/nuc_data/' // TRIM(set) // ''' &> /dev/null')
                   IF (set_index /= 0) CALL report_error('Unable to make ' // &
                        'user directory on server.','External fileio',pcom,1)
                CASE DEFAULT
                   CALL report_error('Missing path for making dir for "' // &
                        TRIM(set) // '" set in ' // TRIM(group) // &
                        ' group.','Developer Reminder',pcom,1)
                END SELECT
             END IF
          END IF
      
! Now add entry to riso_list for this set
! Check if set is already in riso_list.  If so, return
          set_index = find_NDS_iso_list(set)
          IF (set_index >= 1) RETURN
      
! Add user set entry
          set_index = get_NDS_iso_list_number() + 1
          IF (set_index > MAX_NDS)  CALL report_error('NDS_iso_list is full' // &
               ' and isotope list for "' // TRIM(set) // '" can not ' // &
               ' be added.','Improper usage',pcom,1)
      
          NDS_iso_list(set_index)%iso_list = -1
          NDS_iso_list(set_index)%saved = .FALSE.
          NDS_iso_list(set_index)%set = set
      
        END SUBROUTINE add_NDS
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Set_Lists/rm_NDS
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
      
        SUBROUTINE rm_NDS(set,pcom,move_to_trash)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN) :: pcom
          CHARACTER(LEN=*),INTENT(IN)   :: set
          LOGICAL(KIND=4),INTENT(IN)    :: move_to_trash
          CHARACTER(LEN=MAX_NDS_GRP_LEN):: group
          CHARACTER(LEN=MAX_PATH_LEN)   :: set_path
          INTEGER(KIND=4)               :: set_index,group_index,num_sets,i
          LOGICAL(KIND=4)               :: loop,move
          OPTIONAL                      :: move_to_trash
      
! Get path to set
          set_path = get_NDS_path(set,pcom%USER,group)
          IF (set_path == '') CALL report_error('Nuclear data set "' // &
               TRIM(set) // '" was not found.','Improper usage',pcom,1)
      
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
             CALL report_error('Nuclear data sets may not be removed from ' // &
                  'the "' // TRIM(group) // '" group.', &
                  'Improper usage',pcom,1)
          END SELECT
      
! Get set and group indicies
          CALL find_NDS_list_index(set,group_index=group_index, &
               set_index=set_index)
          IF (group_index < 1 .OR. set_index < 1) CALL report_error( &
               'Error finding indicies for removing set "' // TRIM(set) &
               // '"','Improbable',pcom,1)
      
          num_sets = NDS_list(group_index)%num_sets
      
! Remove set from NDS_list
          NDS_list(group_index)%set(set_index) = ''
      
! Move remaining set entries down one position in NDS_list array
! BE SURE TO TEST THIS
          NDS_list(group_index)%set(set_index : num_sets-1) = &
               NDS_list(group_index)%set(set_index+1 : num_sets)
          NDS_list(group_index)%num_sets = num_sets - 1
      
! Clear last entry
          NDS_list(group_index)%set(num_sets) = ''
      
! Check if set is in riso_list.  If so, remove
          set_index = find_NDS_iso_list(set)
          IF (set_index >= 1) THEN
! Remove set from riso_list
      
             num_sets = get_NDS_iso_list_number()
      
! Move remaining set entries down one position in NDS_iso_list array
! BE SURE TO TEST THIS
             NDS_iso_list(set_index : num_sets - 1) = &
                  NDS_iso_list(set_index + 1 : num_sets)
      
! Then erase last entry in NDS_iso_list
             NDS_iso_list(num_sets)%set = ''
             NDS_iso_list(num_sets)%iso_list = -1
          END IF
          
! Save new rate set list
          CALL save_NDS_list(group,pcom)
      
          IF (move) THEN
! Make sure trash has directory for user
             i = safe_shell('/bin/mkdir -p ' // TRIM(CINA_PATH) // 'trash/' // &
                  TRIM(pcom%USER) // '/nuc_data')
             IF (i /= 0) THEN
                WRITE(set_path,'(A,I0,A)') 'Error (',i, &
                     ') making user''s trash directory'
                CALL report_error(TRIM(set_path),'External program',pcom,1)
             END IF
                         
! Delete any set with the same name in user's trash
             i = safe_shell('/bin/rm -fr ''' // TRIM(CINA_PATH) // 'trash/' // &
                  TRIM(pcom%USER) // '/nuc_data/' // TRIM(set) // '''')
             IF (i /= 0) THEN
                WRITE(set_path,'(A,I0,3A)') 'Error (',i,') erasing nuclear ' // &
                     'data set in user''s trash"',TRIM(set),'" to trash'
                CALL report_error(TRIM(set_path),'External program',pcom,1)
             END IF
                         
! Move set to trash
             i = safe_shell('/bin/mv -f ''' // TRIM(set_path) // ''' ''' // &
                  TRIM(CINA_PATH) // 'trash/' // TRIM(pcom%USER) // &
                  '/nuc_data''')
             IF (i /= 0) THEN
                WRITE(set_path,'(A,I0,3A)') 'Error (',i, &
                     ') moving nuclear data set "',TRIM(set),'" to trash'
                CALL report_error(TRIM(set_path),'External program',pcom,1)
             END IF
          END IF
        END SUBROUTINE rm_NDS
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Set_Lists/get_NDS_group_number
! NAME
!  FUNCTION get_NDS_group_number()
! PURPOSE
!  Return the number of rate set groups in NDS_list
! STATUS
!  Complete
! RETURN VALUE
!  INT(KIND=4) with the number of rate set groups in NDS_list
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/27/2004 jpscott       Start function
! SOURCE
      
        FUNCTION get_NDS_group_number()
          IMPLICIT NONE
          INTEGER(KIND=4)                  :: get_NDS_group_number
      
          get_NDS_group_number = 0
          DO WHILE (NDS_list(get_NDS_group_number + 1)%group /= '')
             get_NDS_group_number = get_NDS_group_number + 1
             IF (get_NDS_group_number > MAX_NDS_GROUPS) RETURN
          END DO
        END FUNCTION get_NDS_group_number
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Set_Lists/find_NDS_group_index
! NAME
!  FUNCTION find_NDS_group_index(group)
! PURPOSE
!  Return rate set group index in NDS_list for a group name
! STATUS
!  Complete
! CAUTION
!  Make sure rate set lists are loaded (read_NDS_list) before calling
!  this subroutine.  This function may be used outside this module to 
!  determine if a rate set group is loaded into NDS_list; however,
!  its value is useless because NDS_list is private to this module.
! INPUTS
!  group: Name of rate set group to find
! RETURN VALUE
!  INT(KIND=4) of index into NDS_list for rate set group.  If group 
!  name is not found, -1 will be returned.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/20/2004 jpscott       Start function
! SOURCE
      
        FUNCTION find_NDS_group_index(group)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)   :: group
          INTEGER(KIND=4)               :: find_NDS_group_index,i
      
! Default value in case group is not found
          find_NDS_group_index = -1
      
          DO i = 1, MAX_NDS_GROUPS
             IF (NDS_list(i)%group == '') THEN
                RETURN
             ELSE IF (NDS_list(i)%group == group) THEN
                find_NDS_group_index = i
                RETURN
             END IF
          END DO
        END FUNCTION find_NDS_group_index
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Set_Lists/find_NDS_list_index
! NAME
!  SUBROUTINE find_NDS_list_index(set,group_name,group_index,
!                                 set_index)
! PURPOSE
!  Find set in NDS_list and return rate set and group indicies
! STATUS
!  Complete
! CAUTION
!  Make sure rate set lists are loaded (read_NDS_list) before calling
!  this subroutine.  This function may be used outside this module to
!  determine if a rate set is loaded into NDS_list; however, its
!  value is useless because NDS_list is private to this module.
!  
!  This subroutine finds the first set in NDS_list with the correct 
!  name.  If libraries with the same name are in different groups, only 
!  indicies to one set will be returned.  Different users may use the
!  same set name.
! INPUTS
!  set: Set name to find in NDS_list
! OUTPUTS
!  group_index (OPTIONAL): Index into NDS_list to rate set group
!  group_name (OPTIONAL): Name of rate group set belongs to
!  set_index (OPTIONAL): Index into NDS_list to correct rate set
!  
!  If set name was not found in NDS_list then group_name will be 
!  empty and indicies will be -1
! EXAMPLES
!  CALL find_NDS_list_index(set,group_name,group_index,set_index)
!  
!  ! Print group name
!  print *,group_name
!  print *,NDS_list(group_index)%group
!  
!  ! Print set name
!  print *,set
!  print *,NDS_list(group_index)%set(set_index)
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/20/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE find_NDS_list_index(set,group_name,group_index,set_index)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)   :: set
          CHARACTER(LEN=*),INTENT(OUT)  :: group_name
          INTEGER(KIND=4),INTENT(OUT)   :: set_index,group_index
          INTEGER(KIND=4)               :: grp,i
          OPTIONAL                      :: group_name,group_index,set_index
      
! Set return values if error occurs
          IF (PRESENT(group_name)) group_name = ''
          IF (PRESENT(group_index)) group_index = -1
          IF (PRESENT(set_index)) set_index = -1
      
! Just in case set is empty, exit with error
          IF (set == '') RETURN
      
! Loop through all rate set groups
          DO grp = 1, MAX_NDS_GROUPS
             IF (NDS_list(grp)%group /= '') THEN
      
! Loop through all libraries in group
                DO i = 1, NDS_list(grp)%num_sets
      
                   IF (NDS_list(grp)%set(i) == set) THEN
                      IF (PRESENT(group_name)) group_name = &
                           NDS_list(grp)%group
                      IF (PRESENT(group_index)) group_index = grp
                      IF (PRESENT(set_index)) set_index = i
                      RETURN
                   END IF
                END DO
             ELSE
! All rate set groups have been searched so exit with error
                RETURN
             END IF
          END DO
      
! Set not found so exit with error
        END SUBROUTINE find_NDS_list_index
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Isotope_Lists/read_NDS_iso_list
! NAME
!  SUBROUTINE read_NDS_iso_list(set,pcom,iso_list)
! PURPOSE
!  Load/return the isotope list for a rate set into NDS_iso_list
! STATUS
!  Complete
! INPUTS
!  set: Name of set to retrieve isotope list for
!  pcom: cina_common variable needed for reporting errors
! OUTPUTS
!  iso_list (OPTIONAL): array containing isotope list for set
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/23/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE read_NDS_iso_list(set,pcom,iso_list,FORCE_LOAD)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN) :: pcom
          CHARACTER(LEN=*),INTENT(IN)   :: set
          INTEGER(KIND=2),INTENT(OUT)   :: iso_list(0:MAX_Z,MAX_ISO)
          LOGICAL(KIND=4),INTENT(IN)    :: FORCE_LOAD
          CHARACTER(LEN=5000)           :: line
          INTEGER(KIND=4)               :: set_index,status,counter,tab_index,z,a
          LOGICAL(KIND=4)               :: load_from_file
          OPTIONAL                      :: iso_list,FORCE_LOAD
      
          IF (PRESENT(FORCE_LOAD)) THEN
             load_from_file = FORCE_LOAD
          ELSE
             load_from_file = .FALSE.
          END IF
      
! Find set index into NDS_iso_list
          set_index = find_NDS_iso_list(set)
      
          IF (set_index < 1 .OR. load_from_file) THEN
! set is not is NDS_iso_list
      
             line = get_NDS_path(set,pcom%USER)
             IF (line == '') CALL report_error('Nuclear data set "' // TRIM(set) &
                  // '" was not found.','Improper usage',pcom,1)
      
             IF (set_index < 1) THEN
! Get next empty set_index in NDS_iso_list
                set_index = get_NDS_iso_list_number() + 1
                IF (set_index > MAX_NDS) CALL report_error('NDS_iso_list is full' &
                     // ' and isotope list for "' // TRIM(set) // '" can not ' // &
                     ' be loaded.','Improper usage',pcom,1)
             END IF
      
             OPEN(NDS_ISO_LIST_UNIT, FILE=TRIM(line) // 'Rate_Isotopes', &
                  IOSTAT=status, ACTION='READ')
      
             IF (status /= 0) CALL report_error('Error opening rate isotope ' // &
                  'list for nuclear data set ' // TRIM(set),'File input/output', &
                  pcom,1)
      
             NDS_iso_list(set_index)%iso_list = -1
      
             DO WHILE (status == 0)
                READ(NDS_ISO_LIST_UNIT,'(A)',IOSTAT=status) line
      
                IF (status > 0) THEN
                   CALL report_error('Error reading rate isotope list for ' // &
                        'nuclear data set ' // TRIM(set),'File input/output', &
                        pcom,1)
      
                ELSE IF (status == 0) THEN
                   counter = INDEX(line,'=')
                   IF (counter > 0) THEN
                      READ(line(1:counter-1),'(I)') z
                      IF (z > MAX_Z) CALL report_error('NDS_iso_list(1) is too ' &
                           // 'small','Developer Reminder',pcom,1)
      
! Now that z is found, read tab separated list of masses
                      line = line(counter+1:)
                      counter = 0
                      DO WHILE (LEN_TRIM(line) > 0)
                         tab_index = INDEX(line,ACHAR(9))
                         IF (tab_index < 1) tab_index = LEN_TRIM(line) + 1
                         READ(line(1:tab_index-1),'(I)') a
! Now that z and a are found
                         counter = counter + 1
                         IF (counter > MAX_ISO) CALL report_error('NDS_iso_list' &
                              // '(2) is too small','Developer Reminder',pcom,1)
      
                         NDS_iso_list(set_index)%iso_list(z,counter) = a
                         line = line(tab_index+1:)
                      END DO
                   END IF
                END IF
             END DO
             CLOSE(NDS_ISO_LIST_UNIT)
      
             NDS_iso_list(set_index)%set = set
             NDS_iso_list(set_index)%saved = .TRUE.
          END IF
      
! Return iso_list
          IF (PRESENT(iso_list)) iso_list = NDS_iso_list(set_index)%iso_list
      
        END SUBROUTINE read_NDS_iso_list
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Isotope_Lists/save_NDS_iso_list
! NAME
!  SUBROUTINE save_NDS_iso_list(set,pcom,path)
! PURPOSE
!  Save isotope list for a set to disk
! STATUS
!  Complete
! CAUTION
!  This subroutine returns without saving if isotope list on disk is same
!  as one in NDS_iso_list.
! INPUTS
!  set: Name of set to save isotope list for
!  pcom: cina_common variable needed for reporting errors
!  path (OPTIONAL): need for saving isotope list outside of suite
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/24/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE save_NDS_iso_list(set,pcom,path)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: set,path
          INTEGER(KIND=4)                  :: set_index,status,z
          CHARACTER(LEN=MAX_PATH_LEN)      :: s_path
          CHARACTER(LEN=5000)              :: a_list
          OPTIONAL                         :: path
      
          set_index = find_NDS_iso_list(set)
          IF (set_index < 1) CALL report_error('Nuclear data set "' // TRIM(set) &
               // '" was not found in NDS_iso_list when trying to save ' // &
               'isotope list.','Improper usage',pcom,1)
      
          IF (NDS_iso_list(set_index)%saved) RETURN  ! Return if saved already
      
          IF (PRESENT(path)) THEN
             s_path = path
          ELSE
             s_path = get_NDS_path(set,pcom%USER)
          END IF
      
          OPEN(NDS_ISO_LIST_UNIT, FILE=TRIM(s_path) // 'Rate_Isotopes', &
               IOSTAT=status, ACTION='WRITE')
          IF (status /= 0) CALL report_error('Error opening isotope list for' // &
               ' nuclear data set ' // TRIM(set),'File input/output',pcom,1)
      
          DO z = 0, MAX_Z
             a_list = get_NDS_sorted_a_list(set_index,z)
      
             IF (a_list /= '') WRITE(NDS_ISO_LIST_UNIT,'(I0,2A)',IOSTAT=status) &
                  z,'=',TRIM(a_list)
          END DO
      
          CLOSE(NDS_ISO_LIST_UNIT)
          NDS_iso_list(set_index)%saved = .TRUE.
        END SUBROUTINE save_NDS_iso_list
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Isotope_Lists/NDS_add_isotope
! NAME
!  SUBROUTINE NDS_add_isotope(set,z,a)
! PURPOSE
!  Add isotope to NDS_iso_list for a set
! STATUS
!  Complete
! CAUTION
!  Isotope list should be loaded before calling this subroutine, otherwise
!  this subroutine simply returns.  It also returns without changing 
!  NDS_iso_list if MAX_ISO is too small to hold new isotope.
! INPUTS
!  set: Name of set to add isotope to
!  z: Number of protons of isotope to add
!  a: Number of protons and neutrons (atomic mass) of isotope to add
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/23/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE NDS_add_isotope(set,z,a)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)      :: set
          INTEGER(KIND=4),INTENT(IN)       :: z,a
          INTEGER(KIND=4)                  :: set_index,m,i
      
! Check for valid z
          IF (z > MAX_Z .OR. z < 0) RETURN
      
! Get set index i NDS_iso_list, return if set not found
          set_index = find_NDS_iso_list(set)
          IF (set_index < 1) RETURN
          
! Find index to next empty value in iso_list, return if isotope exists
          m = NDS_iso_list(set_index)%iso_list(z,1)
          i = 1  ! counter through iso_list
          DO WHILE (m > 0)
             IF (m == a) THEN
                RETURN
             END IF
             i = i + 1
             IF (i > MAX_ISO) RETURN
             m = NDS_iso_list(set_index)%iso_list(z,i)
          END DO
          
! Add isotope
          NDS_iso_list(set_index)%iso_list(z,i) = a
          NDS_iso_list(set_index)%saved = .FALSE.
        END SUBROUTINE NDS_add_isotope
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Isotope_Lists/NDS_isotope_exist
! NAME
!  FUNCTION NDS_isotope_exist(set,z,a,z_exist)
! PURPOSE
!  Check if isotope exists in set
! STATUS
!  Complete
! CAUTION
!  Isotope list must be loaded for set beforehand
! INPUTS
!  set: Name of set to check if isotope exists in 
!  z: Number of protons of isotope
!  a: Number of protons and neutrons of isotope (atomic mass)
! OUTPUTS
!  z_exist (OPTIONAL): .TRUE. if at least one isotope with value of z
! RETURN VALUE
!  .TRUE. or .FALSE. LOGICAL(KIND=4) if isotope exists
!  .FALSE. is returned if set was not found
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/23/2004 jpscott       Start function
! SOURCE
      
        FUNCTION NDS_isotope_exist(set,z,a,z_exist)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)      :: set
          INTEGER(KIND=4),INTENT(IN)       :: z,a
          LOGICAL(KIND=4),INTENT(OUT)      :: z_exist
          LOGICAL(KIND=4)                  :: NDS_isotope_exist
          INTEGER(KIND=4)                  :: set_index,i,m
          OPTIONAL                         :: z_exist
      
          NDS_isotope_exist = .FALSE.
          IF (PRESENT(z_exist)) z_exist = .FALSE.
      
! Check for valid z
          IF (z > MAX_Z .OR. z < 0) RETURN
      
! Get set index i NDS_iso_list, return if set not found
          set_index = find_NDS_iso_list(set)
          IF (set_index < 1) RETURN
      
          m = NDS_iso_list(set_index)%iso_list(z,1)
          i = 1  ! counter through iso_list
          IF (PRESENT(z_exist) .AND. (m > 0)) z_exist = .TRUE.
          DO WHILE (m > 0)
             IF (m == a) THEN
                NDS_isotope_exist = .TRUE.
                RETURN
             END IF
             i = i + 1
             IF (i > MAX_ISO) RETURN
             m = NDS_iso_list(set_index)%iso_list(z,i)
          END DO
        END FUNCTION NDS_isotope_exist
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Isotope_Lists/get_NDS_iso_list_number
! NAME
!  FUNCTION get_NDS_iso_list_number()
! PURPOSE
!  Return number of isotope lists in NDS_iso_list
! STATUS
!  Complete
! RETURN VALUE
!  INT(KIND=4) number of isotope lists in NDS_iso_list
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/24/2004 jpscott       Start subroutine
! SOURCE
      
        FUNCTION get_NDS_iso_list_number()
          IMPLICIT NONE
          INTEGER(KIND=4)               :: get_NDS_iso_list_number
      
          get_NDS_iso_list_number = 0
          DO WHILE (NDS_iso_list(get_NDS_iso_list_number+1)%set /= '')
             get_NDS_iso_list_number = get_NDS_iso_list_number + 1
             IF (get_NDS_iso_list_number >= MAX_NDS) RETURN
          END DO
        END FUNCTION get_NDS_iso_list_number
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Isotope_Lists/find_NDS_iso_list
! NAME
!  FUNCTION find_NDS_iso_list(set)
! PURPOSE
!  Return index in NDS_iso_list for a set
! STATUS
!  Complete
! CAUTION
!  This function may be used to determine if a rate set isotope list
!  is loaded into NDS_iso_list; however, its value is useless outside this
!  module because NDS_iso_list is private to this module.
! INPUTS
!  set: Name of set to find in NDS_iso_list
! RETURN VALUE
!  INT(KIND=4) index in NDS_iso_list for a set if set is found,
!  otherwise -1 is returned
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/24/2004 jpscott       Start subroutine
! SOURCE
      
        FUNCTION find_NDS_iso_list(set)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)   :: set
          INTEGER(KIND=4)               :: find_NDS_iso_list,i
      
! Default value to return if set is not found
          find_NDS_iso_list = -1
      
          DO i = 1, MAX_NDS
             IF (NDS_iso_list(i)%set == '') THEN
! Set not found
                RETURN
             ELSE IF (NDS_iso_list(i)%set == set) THEN
                find_NDS_iso_list = i
                RETURN
             END IF
          END DO
        END FUNCTION find_NDS_iso_list
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Isotope_Lists/get_NDS_sorted_a_list
! NAME
!  FUNCTION get_NDS_sorted_a_list(set_index,z)
! PURPOSE
!  Return tab separated list of sorted a values for a given z in a library
! STATUS
!  Complete
! CAUTION
!  This function is not efficient
! INPUTS
!  set_index: Index to library in NDS_iso_list to use for sorting
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
        
        FUNCTION get_NDS_sorted_a_list(set_index,z)
          IMPLICIT NONE
          INTEGER(KIND=4),INTENT(IN)       :: set_index,z
          CHARACTER(LEN=5000)              :: get_NDS_sorted_a_list
          INTEGER(KIND=4)                  :: a_count,a_min,a_min_old
          INTEGER(KIND=4)                  :: sorted_num,i,m
          LOGICAL(KIND=4)                  :: header
      
! Count the number of isotopes
          a_count = 0
          DO WHILE (NDS_iso_list(set_index)%iso_list(z,a_count + 1) > 0)
             a_count = a_count + 1
          END DO
      
          a_min_old = 0
          get_NDS_sorted_a_list = ''
          header = .FALSE.
          
! sorted_num is the nth sorted isotope
          DO sorted_num = 1, a_count
             a_min = HUGE(a_min)
             
! i is a counter to find the next smallest value of a
             DO i = 1, a_count
                m = NDS_iso_list(set_index)%iso_list(z,i)
                IF (m < a_min .AND. m > a_min_old) a_min = m
             END DO
      
! add a_min to return value
             IF (header) THEN
                WRITE(get_NDS_sorted_a_list,'(2A,I0)') &
                     TRIM(get_NDS_sorted_a_list),ACHAR(9),a_min
             ELSE
                WRITE(get_NDS_sorted_a_list,'(I0)') a_min
                header = .TRUE.
             END IF
      
             a_min_old = a_min
          END DO
        END FUNCTION get_NDS_sorted_a_list
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_IDs/read_NDid_list
! NAME
!  SUBROUTINE read_NDid_list(set,pcom,z,a,NDids,num_NDids,seek_pos,
!                           buffer)
! PURPOSE
!  Load/return list of rate IDs for an isotope of a rate set
! STATUS
!  Complete
! CAUTION
!  save_NDid_list requires that rate IDs be in NDid_list.  buffer should
!  only be set to .FALSE. for situations where rate lists will not be 
!  modified.  An example is when inverse parameters are needed for a rate
!  but the non-inverse rate should remain in the buffers.
!  
!  Make sure rate set lists are loaded (read_rlib_list) before calling
!  this subroutine.
! INPUTS
!  set: Name of set to load rate ID list for
!  pcom: cina_common variable needed for reporting errors
!  z: Number of protons of isotope to load rate ID list for
!  a: Atomic mass of isotope to load rate ID list for
!  buffer (OPTIONAL): Set to .FALSE. when rate IDs should not be loaded
!                     into NDid_list
! OUTPUTS
!  NDids (OPTIONAL): character array of rate IDs for selected isotope
!  num_rates (OPTIONAL): number of rate IDs for selected isotope
!  seek_pos (OPTIONAL): int array of seek positions for getting rate info
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/27/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE read_NDid_list(set,pcom,z,a,NDids,num_NDids,seek_pos, &
             buffer)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: set
          INTEGER(KIND=4),INTENT(IN)       :: z,a
          LOGICAL(KIND=4),INTENT(IN)       :: buffer
          CHARACTER(LEN=MAX_ND_ID_LEN),INTENT(OUT) :: NDids(:)
          INTEGER(KIND=4),INTENT(OUT)      :: num_NDids,seek_pos(:)
          OPTIONAL                         :: NDids,num_NDids,seek_pos,buffer
          
          TYPE(NDid_list_buffer)            :: local_NDids
          LOGICAL(KIND=4)                  :: load_from_file = .FALSE.
          INTEGER(KIND=4)                  :: set_index,status,i
          CHARACTER(LEN=MAX_PATH_LEN)      :: path
          
      
! Get index to set in NDid_list
          set_index = find_NDS_NDid_list(set)
          
! Load NDid list from file if 1) wrong isotope is in NDid_list or 
! 2) set is not in NDid_list
! Then set set_index
          IF (set_index > 0) THEN
             IF (NDid_list(set_index)%z /= z .OR. NDid_list(set_index)%a /= a) &
                  load_from_file = .TRUE.
          ELSE IF (set_index == -1) THEN
             load_from_file = .TRUE.
      
             set_index = get_NDS_list_number() + 1
             IF (set_index > MAX_NDS) CALL report_error('NDid_list is too ' // &
                  'small to hold NDid list for "' // TRIM(set) // '" set',&
                  'Developer Reminder',pcom,1)
          END IF
      
! Load NDid list from file or memory into local_NDids
          IF (load_from_file) THEN
             path = get_NDS_iso_path(set,z,a,'NDid',pcom%USER)
             IF (path == '') CALL report_error('Nuclear data set "' // TRIM(set) &
                  // '" was not found when obtaining path.','Improper usage', &
                  pcom,1)
      
             OPEN(ND_ID_LIST_UNIT, FILE=TRIM(path), IOSTAT=status, &
                  ACTION='READ')
      
             IF (status /= 0) CALL report_error('Can not open rate list for ' // &
                  TRIM(path),'File input/output',pcom,1)
      
! Read in the number of rates
             READ(ND_ID_LIST_UNIT,'(I)',IOSTAT=status) local_NDids%NDid_num
             IF (status /= 0) CALL report_error('Can not read in number of ' // &
                  'rates for ' // TRIM(path),'File input/output',pcom,1)
      
! If all rates won't fit in buffer
             IF (local_NDids%NDid_num > MAX_ND) CALL report_error('Isotope ' // &
                  'has too many rates to fit in buffer for ' // TRIM(path), &
                  'Improbable',pcom,1)
      
! Read in NDid list into NDid_list
             DO i = 1, local_NDids%NDid_num
                READ(ND_ID_LIST_UNIT,'(I9,A)',IOSTAT=status) &
                     local_NDids%seek_pos(i),local_NDids%NDid(i)
                IF (status > 0) CALL report_error('Can not read rate list ' // &
                     'for ' // TRIM(path),'File input/output',pcom,1)
             END DO
             CLOSE(ND_ID_LIST_UNIT)
      
             local_NDids%z = z
             local_NDids%a = a
             local_NDids%set = set
          ELSE
! Copy NDid_list entry into local_NDids
             local_NDids = NDid_list(set_index)
          END IF
      
! Now local_NDids has rate ID lists
      
! Use load_from_file as a temp variable with a value of buffer
          load_from_file = .TRUE.
          IF (PRESENT(buffer)) THEN
             IF (.NOT. buffer) load_from_file = .FALSE.
          END IF
      
! Save local_NDids to NDid_list if buffer (load_from_file) is .TRUE.
          IF (load_from_file) NDid_list(set_index) = local_NDids
      
! Now return optional output
          IF (PRESENT(num_NDids)) num_NDids = local_NDids%NDid_num
      
          IF (PRESENT(NDids)) THEN
             IF (UBOUND(NDids,1) < local_NDids%NDid_num) THEN
                CALL report_error('NDid_list is too small to hold full list', &
                     'Improbable',pcom,0)
                local_NDids%NDid_num = UBOUND(NDids,1)
             END IF
      
             NDids = local_NDids%NDid(:local_NDids%NDid_num)
          END IF
      
          IF (PRESENT(seek_pos)) THEN
             IF (UBOUND(seek_pos,1) < local_NDids%NDid_num) THEN
                CALL report_error('seek_pos is too small to hold full list', &
                     'Improbable',pcom,0)
                local_NDids%NDid_num = UBOUND(seek_pos,1)
             END IF
      
             seek_pos = local_NDids%seek_pos(:local_NDids%NDid_num)
          END IF
        END SUBROUTINE read_NDid_list
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_IDs/save_NDid_list
! NAME
!  SUBROUTINE save_NDid_list(set,pcom,z,a,path)
! PURPOSE
!  Save list of rate IDs for an isotope of a rate set to disk
! STATUS
!  Complete
! CAUTION
!  Make sure rate set lists are loaded (read_NDS_list) before calling
!  this subroutine.
! INPUTS
!  set: Name of set to save rate ID list for
!  pcom: cina_common variable needed for reporting errors
!  z: Number of protons of isotope to save rate ID list for
!  a: Atomic mass of isotope to save rate ID list for
!  path (OPTIONAL): path to set directory if saving NDid list outside 
!                   of suite
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/30/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE save_NDid_list(set,pcom,z,a,path)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: set,path
          INTEGER(KIND=4),INTENT(IN)       :: z,a
          OPTIONAL                         :: path
      
          CHARACTER(LEN=MAX_PATH_LEN)      :: set_path
          INTEGER(KIND=4)                  :: status,set_index,NDid_num,i
      
! Get index to set in NDid_list
          set_index = find_NDS_NDid_list(set)
          IF (set_index < 0) CALL report_error('Can not save NDid list for "' // &
               TRIM(set) // '" that is not in buffer.','Improbable',pcom,1)
      
! Check that z and a are correct
          IF (NDid_list(set_index)%z /= z .OR. NDid_list(set_index)%a /= a) THEN
             WRITE(set_path,'(4(A,I0),3A)') 'Isotope in NDid_list (', &
                  NDid_list(set_index)%z,',',NDid_list(set_index)%a, &
                  ') does not match isotope (',z,',',a, &
                  ') that should be saved for the "',TRIM(set),'" set'
             CALL report_error(TRIM(set_path),'Improbable',pcom,1)
          END IF
      
! Get UNIX path to NDid list file
          IF (PRESENT(path)) THEN
             set_path = get_NDS_iso_path(set,z,a,'NDid',pcom%USER,path)
          ELSE
             set_path = get_NDS_iso_path(set,z,a,'NDid',pcom%USER)
          END IF
      
          OPEN(ND_ID_LIST_UNIT, FILE=set_path, IOSTAT=status, ACTION='WRITE')
          IF (status /= 0) CALL report_error('Can not create rate list ' // &
               TRIM(set_path),'File input/output',pcom,1)
          
          NDid_num = NDid_list(set_index)%NDid_num
      
! Write rate_count to file
          WRITE(ND_ID_LIST_UNIT,'(I0)',IOSTAT=status) NDid_num
          IF (status /= 0) CALL report_error('Can not write rate count to list', &
               'File input/output',pcom,1)
      
! Save file seek positions and NDids to file
          DO i = 1, NDid_num
             WRITE(ND_ID_LIST_UNIT,'(I9,A)',IOSTAT=status) &
                  NDid_list(set_index)%seek_pos(i), &
                  TRIM(NDid_list(set_index)%NDid(i))
             IF (status /= 0) CALL report_error('Can not write rate list', &
                  'File input/output',pcom,1)
          END DO
      
          CLOSE(ND_ID_LIST_UNIT)
        END SUBROUTINE save_NDid_list
!***
      
!****is* Nuc_Data_IDs/decode_NDid
! NAME
!  SUBROUTINE decode_NDid(NDid,set,z,a,rtype,reac_str,unique,
!                        unique_reac_str,dtype,name)
! PURPOSE
!  Extract information from a rate id
! STATUS
!  Complete
! INPUTS
!  NDid: rate id to extract information from
! OUTPUTS
!  set (OPTIONAL): name of set rate id references
!  z (OPTIONAL): number of protons of heaviest reactant in reaction
!  a (OPTINOAL): atomic mass of heaviest reactant in reaction
!  rtype (OPTIONAL): type of reaction (number between 1 and 8)
!  reac_str (OPTIONAL): reaction string
!  unique (OPTIONAL): unique identifier for atypical reactions
!  unique_reac_str (OPTIONAL): reaction string with unique identifier in
!                              brackets following the reaction string
!  dtype: data type
!  name: data entry name
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE decode_NDid(NDid,set,z,a,rtype,reac_str,unique, &
             unique_reac_str,dtype,name)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)   :: NDid
          CHARACTER(LEN=*),INTENT(OUT)  :: set,reac_str,unique,unique_reac_str
          CHARACTER(LEN=*),INTENT(OUT)  :: dtype,name
          INTEGER(KIND=4),INTENT(OUT)   :: z,a,rtype
          INTEGER(KIND=4)               :: tab,vtab,tab2,vtab2
          OPTIONAL                      :: set,z,a,rtype,reac_str,unique
          OPTIONAL                      :: unique_reac_str,dtype,name
      
          tab = INDEX(NDid,ACHAR(9))
          vtab = INDEX(NDid,ACHAR(11))
          tab2 = INDEX(NDid,ACHAR(9),BACK=.TRUE.)
          vtab2 = INDEX(NDid,ACHAR(11),BACK=.TRUE.)
!!$    print *,tab,vtab,tab2,vtab2
      
! Check for invalid rate id
          IF (tab < 9 .OR. vtab <= tab .OR. tab2 <= vtab .OR. vtab2 <= tab2) THEN
             IF (PRESENT(rtype)) rtype = -10
             IF (PRESENT(z)) z = -10
             IF (PRESENT(a)) a = -10
             IF (PRESENT(set)) set = 'Invalid NDid'
             IF (PRESENT(reac_str)) reac_str = 'Invalid NDid'
             IF (PRESENT(unique)) unique = 'Invalid NDid'
             IF (PRESENT(unique_reac_str)) unique_reac_str = 'Invalid NDid'
             IF (PRESENT(dtype)) dtype = 'Invalid NDid'
             IF (PRESENT(name)) name = 'Invalid NDid'
             RETURN
          END IF
      
          IF (PRESENT(rtype)) READ(NDid(1:2),'(I)') rtype
          IF (PRESENT(z)) READ(NDid(3:5),'(I)') z
          IF (PRESENT(a)) READ(NDid(6:8),'(I)') a
          IF (PRESENT(set)) set = NDid(9:tab-1)
          IF (PRESENT(reac_str)) reac_str = NDid(tab+1:vtab-1)
          IF (PRESENT(unique)) unique = NDid(vtab+1:tab2-1)
          IF (PRESENT(unique_reac_str)) THEN
             IF (NDid(vtab+1:tab2-1) == '') THEN
                unique_reac_str = NDid(tab+1:vtab-1)
             ELSE
                unique_reac_str = NDid(tab+1:vtab-1) // ' [' // &
                     TRIM(NDid(vtab+1:tab2-1)) // ']'
             END IF
          END IF
          IF (PRESENT(dtype)) dtype = NDid(tab2+1:vtab2-1)
          IF (PRESENT(name)) name = NDid(vtab2+1:)
        END SUBROUTINE decode_NDid
!***
      
!****if* Nuc_Data_IDs/encode_NDid
! NAME
!  FUNCTION encode_NDid(set,z,a,rtype,reac_str,unique,dtype,name)
! PURPOSE
!  Create rate id
! STATUS
!  Complete
! INPUTS
!  set: name of set NDid refers to
!  z: number of protons of heaviest reactant in reaction
!  a: atomic mass of heaviest reactant in reaction
!  rtype: type of reaction (number between 1 and 8)
!  reac_str: reaction string
!  unique: unique identifier for atypical reactions
!  dtype: data type
!  name: data entry name
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/30/2004 jpscott       Start function
! SOURCE
      
        FUNCTION encode_NDid(set,z,a,rtype,reac_str,unique,dtype,name)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)   :: set,reac_str,unique,dtype,name
          INTEGER(KIND=4),INTENT(IN)    :: z,a,rtype
          CHARACTER(LEN=MAX_ND_ID_LEN)  :: encode_NDid
      
          WRITE(encode_NDid,'(I2.2,2I3.3,9A)') rtype,z,a,TRIM(set),ACHAR(9), &
               TRIM(reac_str),ACHAR(11),TRIM(unique),ACHAR(9),TRIM(dtype), &
               ACHAR(11),TRIM(name)
        END FUNCTION encode_NDid
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_IDs/find_NDS_NDid_list
! NAME
!  FUNCTION find_NDS_NDid_list(set)
! PURPOSE
!  Return index in NDid_list for a set
! STATUS
!  Complete
! INPUTS
!  set: Name of set to find in NDid_list
! RETURN VALUE
!  INT(KIND=4) index in NDid_list for a set if set is found,
!  otherwise -1 is returned
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/30/2004 jpscott       Start function
! SOURCE
      
        FUNCTION find_NDS_NDid_list(set)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)      :: set
          INTEGER(KIND=4)                  :: find_NDS_NDid_list,i
      
! Default value to return if set is not found
          find_NDS_NDid_list = -1
      
          DO i = 1, MAX_NDS
             IF (NDid_list(i)%set == '') THEN
! Set is not found
                RETURN
             ELSE IF (NDid_list(i)%set == set) THEN
                find_NDS_NDid_list = i
                RETURN
             END IF
          END DO
        END FUNCTION find_NDS_NDid_list
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_IDs/get_NDS_list_number
! NAME
!  FUNCTION get_NDS_list_number()
! PURPOSE
!  Return number of rate ID lists (1 list per set) in NDid_list
! STATUS
!  Complete
! RETURN VALUE
!  INT(KIND=4) of number of rate ID lists in NDid_list
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/20/2004 jpscott       Start function
! SOURCE
      
        FUNCTION get_NDS_list_number()
          IMPLICIT NONE
          INTEGER(KIND=4)                  :: get_NDS_list_number
      
          get_NDS_list_number = 0
          DO WHILE (NDid_list(get_NDS_list_number + 1)%set /= '')
             get_NDS_list_number = get_NDS_list_number + 1
             IF (get_NDS_list_number > MAX_NDS) RETURN
          END DO
        END FUNCTION get_NDS_list_number
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_IDs/correct_NDid
! NAME
!  SUBROUTINE correct_NDid(NDid,set,link)
! PURPOSE
!  Correct rate id so that links only point to PUBLIC libraries
! STATUS
!  Complete
! CAUTION
!  Make sure rate set lists are loaded (read_rlib_list) before calling
!  this subroutine.
! INPUTS
!  NDid: rate id to correct
!  set: Name of set NDid will be saved to
! OUTPUTS
!  NDid: corrected rate id
!  link (OPTIONAL): .TRUE. is NDid is a link to a PUBLIC set
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/30/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE correct_NDid(NDid,set,link)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(INOUT)   :: NDid
          CHARACTER(LEN=*),INTENT(IN)      :: set
          LOGICAL(KIND=4),INTENT(OUT)      :: link
          OPTIONAL                         :: link
      
          INTEGER(KIND=4)                  :: tab
          CHARACTER(LEN=MAX_NDS_LEN)      :: NDid_set
          CHARACTER(LEN=MAX_NDS_GRP_LEN)      :: NDid_group
      
          IF (PRESENT(link)) link = .FALSE.
          tab = INDEX(NDid,ACHAR(9))
          NDid_set = NDid(9:tab-1)
! Return if set pointed to in rate id is the same as the set 
! name it will be stored in
          IF (NDid_set == set) RETURN
      
! Get rate id set's group
          CALL find_NDS_list_index(NDid_set,NDid_group)
      
          SELECT CASE (NDid_group)
          CASE ('PUBLIC ')
! Allow this link to a different but public set
             IF (PRESENT(link)) link = .TRUE.
             
          CASE DEFAULT
! Don't allow links, replace set name in NDid with set name
             NDid = NDid(:8) // TRIM(set) // ACHAR(9) // NDid(tab+1:)
          END SELECT
        END SUBROUTINE correct_NDid
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_IDs/NDid_exist
! NAME
!  FUNCTION NDid_exist(NDid,pcom)
! CAUTION
!  This doesn't modify any buffers except to call read_NDS_list
! SOURCE
      
        FUNCTION NDid_exist(NDid,pcom)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN) :: pcom
          CHARACTER(LEN=*),INTENT(IN)   :: NDid
          LOGICAL(KIND=4)               :: NDid_exist,flag
          INTEGER(KIND=4)               :: i,z,a,NDid_num
          CHARACTER(LEN=MAX_ND_ID_LEN)  :: NDids(MAX_ND)
          CHARACTER(LEN=MAX_NDS_LEN)    :: set
          CHARACTER(LEN=10240)          :: set_list(3),tmps
      
! Put list of libraries into set_list
          CALL read_NDS_list('PUBLIC',pcom,set_list(1))
          CALL read_NDS_list('SHARED',pcom,set_list(2))
          CALL read_NDS_list('USER',pcom,set_list(3))
! Put tabs at beginning and end to ease set searches
          tmps = ACHAR(9) // TRIM(set_list(1)) // ACHAR(9) // &
               TRIM(set_list(2)) // ACHAR(9) // TRIM(set_list(3)) // ACHAR(9)
      
          NDid_exist = .TRUE.
!print *,'NDid = "'//TRIM(NDid)//'" '
! Check for reasonable rate id
          i = INDEX(NDid,ACHAR(11))
          IF (i < 1) NDid_exist = .FALSE.
          
! Check if set in NDid exists
          IF (NDid_exist) THEN
!print *,'found ACHAR(11)'
             CALL decode_NDid(NDid,set,z,a)
! Check if set exists
             i = INDEX(tmps,ACHAR(9) // TRIM(set) // ACHAR(9))
             IF (i < 1) NDid_exist = .FALSE.
          END IF
          
! Check if isotope in set exists
          IF (NDid_exist) THEN
!print *,'found set',z,a
! Read in isotope list
             CALL read_NDS_iso_list(set,pcom)
! Check if isotope exists
             NDid_exist = NDS_isotope_exist(set,z,a)
          END IF
          
! Check if NDid exists
          IF (NDid_exist) THEN
!print *,'found isotope'
! Load rate list for this isotope
             CALL read_NDid_list(set,pcom,z,a,NDids,NDid_num,buffer=.FALSE.)
             
             flag = .FALSE.    ! Becomes true if rate id found
             i = 1
!print *,'NDid_num = ',NDid_num
             DO WHILE (.NOT. flag .AND. (i <= NDid_num))
!print '(A)',NDids(i)
                IF (NDid == NDids(i)) flag = .TRUE.
                i = i + 1
             END DO
             NDid_exist = flag
          END IF
        END FUNCTION NDid_exist
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_IDs/get_NDid_seek
! NAME
!  FUNCTION get_NDid_seek(NDid)
! PURPOSE
!  Return seek position for a rate id.  This is needed for Rate_Info.
! STATUS
!  Complete
! INPUTS
!  NDid: rate id to return seek position for
! RETURN VALUE
!  INT(KIND=4) that is seek position for a rate id if found, otherwise
!  -1 is returned.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/30/2004 jpscott       Start function
! SOURCE
      
        FUNCTION get_NDid_seek(NDid)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)      :: NDid
          INTEGER(KIND=4)                  :: get_NDid_seek
          CHARACTER(LEN=MAX_NDS_LEN)       :: set
          INTEGER(KIND=4)                  :: z,a,set_index,i
      
! Default value returned when seek position is not found
          get_NDid_seek = -1
      
! Extract set name, z, and a from rate id
          CALL decode_NDid(NDid,set,z,a)
      
! Return -1 if set is not found
          set_index = find_NDS_NDid_list(set)
          IF (set_index < 0) RETURN
          
! Return -1 if isotope is not found
          IF (NDid_list(set_index)%z /= z .OR. NDid_list(set_index)%a /= a) RETURN
      
! Find NDid
          DO i = 1, NDid_list(set_index)%NDid_num
             IF (NDid_list(set_index)%NDid(i) == NDid) THEN
                get_NDid_seek = NDid_list(set_index)%seek_pos(i)
                RETURN
             END IF
          END DO
        END FUNCTION get_NDid_seek
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Info/read_ND_info
! NAME
!  FUNCTION read_ND_info(NDid,pcom,prop_list,seek_pos,unit)
! PURPOSE
!  Return rate properties for a rate ID
! STATUS
!  Complete
! CAUTION
!  Make sure rate library lists are loaded (read_rlib_list) becore calling
!  this subroutine.  File seek positions should be loaded into NDid_list
!  (read_NDid_list) before calling this subroutine unless seek_pos is
!  supplied.  Of course, invalid seek positions or NDids signal an error 
!  and cause the program to exit.
! INPUTS
!  NDid: rate ID to retrieve properties for
!  pcom: cina_common variable needed for reporting errors
!  prop_list: list of properties to return.  All properties are returned
!             if empty.  List should be separated with RATE_INFO_PROP_SEP
!  seek_pos (OPTIONAL): file seek position to use instead of one in 
!                       NDid_list
!  unit (OPTIONAL): unit number to use when opening file.  This is needed
!                   for read_ND_info_no_buf
! RETURN VALUE
!  List of rate properties for a rate ID.  If library in NDid is empty
!  then an empty string is returned and no error is generated.  This 
!  prevents errors when reading rate info for a new rate.
! TODO
!  Create new NDid file if it is out of sync with iso file?
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/31/2004 jpscott       Start function
! SOURCE
      
        FUNCTION read_ND_info(NDid,pcom,prop_list,seek_pos,unit)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: NDid,prop_list
          INTEGER(KIND=4),INTENT(IN)       :: seek_pos,unit
          CHARACTER(LEN=MAX_ND_LEN)        :: read_ND_info
          OPTIONAL                         :: seek_pos,unit
      
          INTEGER(KIND=4)                  :: z,a,seek,status,i,unit_num
          CHARACTER(LEN=MAX_NDS_LEN)       :: set
          CHARACTER(LEN=MAX_PATH_LEN)      :: path
          CHARACTER(LEN=MAX_ND_LEN)        :: line,properties,prop_list2,name
          LOGICAL(KIND=4)                  :: loop
      
! Default value to return if an error
          read_ND_info = ''
      
          CALL decode_NDid(NDid,set,z,a)
! If set name is empty, new rate, then return
          IF (set == '') RETURN
      
! Get seek position
          IF (PRESENT(seek_pos)) THEN
             seek = seek_pos
          ELSE
             seek = get_NDid_seek(NDid)
          END IF
          IF (seek < 0) CALL report_error('Unable to find seek_pos for ' // &
               TRIM(NDid),'Improper usage',pcom,1)
      
! Get unit number
          IF (PRESENT(unit)) THEN
             unit_num = unit
          ELSE
             unit_num = ND_INFO_UNIT
          END IF
      
! Get path to iso file
          path = get_NDS_iso_path(set,z,a,'iso',pcom%USER)
          IF (path == '') CALL report_error('Can not find nuclear data ' // &
               'set "' // TRIM(set) // '"','Improper usage',pcom,1)
      
          OPEN(unit_num, FILE=path, IOSTAT=status, ACTION='READ')
          IF (status /= 0) CALL report_error('Can not open nuclear data info '  &
               // 'for ' // TRIM(NDid),'File input/output',pcom,1)
      
! Position file pointer
          status = FSEEK(unit_num,seek,0)
          IF (status /= 0) CALL report_error('Can not seek in nuclear data ' // &
               'info file for ' // TRIM(NDid),'File input/output',pcom,1)
      
! Make sure NDid is correct
          READ(unit_num,'(A)',IOSTAT=status) line
          IF (status /= 0) CALL report_error('Can not read nuclear data id ' // &
               'for ' // TRIM(NDid),'File input/output',pcom,1)
      
          IF (line /= NDid) THEN
             CLOSE(unit_num)
             CALL report_error('seek is incorrect for NDid ' // TRIM(NDid), &
                  'Improbable',pcom,1)
          END IF
      
! Read in properties into properties
          properties = ''
          DO WHILE (status == 0)
             READ (unit_num,'(A)',IOSTAT=status) line
             IF (status == 0) THEN
! If NOTES, replace '\0A' with '\0C'
                IF (line(1:17) == 'Nuc Data Notes = ') &
                     line = replaceall(line,ACHAR(92)//'0A',ACHAR(92)//'20')
      
! Search for ND_INFO_VAL_SEP to tell if its a property
                i = INDEX(line,ND_INFO_VAL_SEP)
                IF (i > 0) THEN
                   properties = TRIM(properties) // ND_INFO_PROP_SEP // &
                        TRIM(line2str(TRIM(line)))
                ELSE
! Stop reading in if another NDid is reached
                   status = -1
                END IF
             ELSE IF (status > 0) THEN
                CALL report_error('Can not read nuclear data properties for ' // &
                     TRIM(NDid),'File input/output',pcom,1)
             END IF
          END DO
          CLOSE(unit_num)
      
! Remove initial ND_INFO_PROP_SEP
          properties = properties(LEN(ND_INFO_PROP_SEP)+1:)
! Return desired properties
          IF (prop_list == '') THEN
             read_ND_info = properties
          ELSE
! Store prop_list into prop_list2 because prop_list can't be modified
             prop_list2 = prop_list
      
! read_ND_info should be empty here
             loop = .TRUE.
             DO WHILE (loop)
! Get the next property name in prop_list2
                loop = next_in_list(name,prop_list2,ACHAR(9))
! Get the value of this property
                line = get_prop_value(name,properties,ND_INFO_PROP_SEP, &
                     ND_INFO_VAL_SEP)
! Add name and value to prop to return to caller
                read_ND_info = TRIM(read_ND_info) // ND_INFO_PROP_SEP // &
                     TRIM(name) // ND_INFO_VAL_SEP // line
             END DO
! Remove initial ND_INFO_PROP_SEP
             read_ND_info = read_ND_info(LEN(ND_INFO_PROP_SEP)+1:)
          END IF
        END FUNCTION read_ND_info
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Info/save_ND_info
! NAME
!  SUBROUTINE save_ND_info(pcom,report,set_info,path,plevel)
! PURPOSE
!  Save ND_file buffer to disk
! STATUS
!  Complete but untested
! CAUTION
!  Make sure rate set and isotope lists are loaded (read_NDS_list and
!  read_NDS_iso_list) before calling this subroutine (unless path is 
!  used).
!  
!  Rates that have never been in the suite should have an empty set
!  name in the rate ID.
!  
!  Note that NDid_list will be overwritten with the NDid_list for the 
!  isotope and set being saved. Then save_NDid_list will be called
!  internally.  
!  
!  NDS_iso_list will also be updated to include any new isotopes but this
!  list is not saved.  Call save_rlib_iso_list after all isotopes have
!  been modified.
!
!  When saving, the properties for a rate in ND_file will overwrite
!  any pre-existing properties.  To modify only a few properties and keep
!  all remaining properties, call read_rate_info or read_rate_info_no_buf
!  to retrieve old properties, then call update_rate_prop to update the
!  old properties, then put this property list in ND_file.
!  
!  Only rates with a rate ID in ND_file will be modified and all others 
!  will remain without being erased or modified.
! INPUTS
!  pcom: cina_common variable needed for reporting errors
!  report: string containing existing report
!  set_info: string containing existing set_info
!  path (OPTIONAL): use for saving rate info outside of suite
!  plevel (OPTIONAL): controls the level of detail in the report and 
!                     set_info.  Should be 2 for set management,
!                     otherwise rate management detail is assumed.
! OUTPUTS
!  report: string with new report appended to existing report
!  set_info: string with new set_info appended to existing set_info
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/01/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE save_ND_info(pcom,report,set_info,path,plevel)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(INOUT)   :: report,set_info
          CHARACTER(LEN=*),INTENT(IN)      :: path
          INTEGER(KIND=4)                  :: plevel
          OPTIONAL                         :: path,plevel
          CHARACTER(LEN=MAX_NDS_GRP_LEN)   :: group
          CHARACTER(LEN=MAX_PATH_LEN)      :: file_path,dir_path
          CHARACTER(LEN=2000)              :: tmps
          INTEGER(KIND=4)                  :: status,z,a,log_level
          LOGICAL(KIND=4)                  :: loop,exist
      
! Set log_level
          IF (PRESENT(plevel)) THEN
             log_level = plevel
          ELSE
             log_level = 1
          END IF
      
! This converts from INT(KIND=2) to INT(KIND=4)
          z = ND_file%z
          a = ND_file%a
      
! Get set group and file_path
          IF (PRESENT(PATH)) THEN
             file_path = get_NDS_iso_path(ND_file%set, z, a, 'iso', &
                  pcom%USER, path)
             dir_path = path
             group = 'USER'
          ELSE
! Get rate set group
             CALL find_NDS_list_index(ND_file%set, group)
      
! Check permissions
             SELECT CASE (group)
             CASE ('PUBLIC ')
                CALL report_error('PUBLIC nuclear data sets such as "' // &
                     TRIM(ND_file%set) // '" may not be modified.', &
                     'Improper usage',pcom,1)
             CASE ('SHARED ')
                CALL report_error('SHARED nuclear data sets such as "' // &
                     TRIM(ND_file%set) // '" may not be modified.', &
                     'Improper usage',pcom,1)
             CASE ('USER ')
! Permission granted
             CASE (' ')
                CALL report_error('Nuclear data set "' // TRIM(ND_file%set) // &
                     '" does not exist.','Improper usage',pcom,1)
             CASE DEFAULT
                CALL report_error('Unknown nuclear data set group "' // &
                     TRIM(group) // '" for saving libraries.','Improper usage', &
                     pcom,1)
             END SELECT
             
             file_path = get_NDS_iso_path(ND_file%set, z, a, 'iso', &
                  pcom%USER)
             dir_path = get_NDS_path(ND_file%set, pcom%USER)
          END IF
      
! Make sure isotope list is in memory
          status = find_NDS_iso_list(ND_file%set)
          IF (status < 1) CALL report_error('Isotope list should be loaded ' // &
               'before saving "' // TRIM(ND_file%set) // '" set.', &
               'Developer Reminder',pcom,1)
      
! exist is T if isotope exist, loop is T if z exists
          exist = NDS_isotope_exist(ND_file%set, z, a, loop)
      
! Make a directory for this value of z if necessary
          IF (.NOT. loop) THEN
             WRITE(tmps,'(2A,I0,A)') '/bin/mkdir -p ''',TRIM(dir_path),z, &
                  ''' &> /dev/null'
             status = safe_shell(tmps)
             IF (status /= 0) CALL report_error('Could not make directory ' // &
                  'for z in set ' // TRIM(ND_file%set), &
                  'External fileio',pcom,1)
          END IF
      
! Make a backup of iso file if isotope exists, and open it
          IF (exist) THEN
             status = DELFILESQQ(TRIM(file_path) // '.old')
             loop = RENAMEFILEQQ(file_path, TRIM(file_path) // '.old')
             IF (.NOT. loop) CALL report_error('Could not rename old .rate ' // &
                  'file for set ' // TRIM(ND_file%set), &
                  'External fileio',pcom,1)
             OPEN(ND_INFO_OLD_UNIT, FILE=TRIM(file_path) // '.old', &
                  IOSTAT=status, ACTION='READ')
             IF (status /= 0) THEN
                WRITE(tmps,'(3A,I0)') 'Could not open old .rate file for ' // &
                     'set '//TRIM(ND_file%set),' Error ',status
                CALL report_error(tmps,'File input/output',pcom,1)
             END IF
          ELSE
! Add isotope if needed
             CALL NDS_add_isotope(ND_file%set, z, a)
          END IF
      
! Open new iso file
          OPEN(ND_INFO_UNIT, FILE=file_path, IOSTAT=status, ACTION='WRITE')
          IF (status /= 0) THEN
             WRITE(tmps,'(A,I0)') 'Could not open new .rate file for ' // &
                  'set ' // TRIM(ND_file%set) // ' Error ',status
             print *,TRIM(file_path)
             CALL report_error(tmps,'File input/output',pcom,1)
          END IF
      
! Most of the work is performed in save_ND_info_internal to simplify
! the save process
          CALL save_ND_info_internal(pcom,exist,report,set_info,log_level)
      
          IF (exist) CLOSE(ND_INFO_OLD_UNIT)
          CLOSE(ND_INFO_UNIT)
      
! Save rid list
          IF (PRESENT(path)) THEN
             CALL save_NDid_list(ND_file%set, pcom, z, a, path)
          ELSE
             CALL save_NDid_list(ND_file%set, pcom, z, a)
          END IF
      
! Mark ND_file buffer as cleared
          ND_file%set = ''
        END SUBROUTINE save_ND_info
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Info/open_ND_info
! NAME
!  SUBROUTINE open_ND_info(set,pcom,z,a)
! PURPOSE
!  Initialize ND_file buffer for modifing rate properties
! STATUS
!  Complete but untested
! CAUTION
!  Only one isotope from one set may be opened at a time
! INPUTS
!  set: Name of set to open for modifing rate info
!  pcom: cina_common variable needed for reporting errors
!  z: Number of protons of isotope to open for modifing rate info
!  a: Atomic mass of isotope to open for modifing rate info
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/20/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE open_ND_info(set,pcom,z,a)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: set
          INTEGER(KIND=4),INTENT(IN)       :: z,a
          
! Check that ND_file buffer is not being used
          IF (ND_file%set /= '') CALL report_error('Attempt to open a ' // &
               'used ND_file buffer','Improbable',pcom,1)
      
          ND_file%set = set
          ND_file%z = z
          ND_file%a = a
          ND_file%NDid_num = 0
          ND_file%NDids = ''
          ND_file%prop = ''
        END SUBROUTINE open_ND_info
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Info/change_ND_info
! NAME
!  SUBROUTINE change_ND_info(NDid,pcom,properties)
! PURPOSE
!  Modify rate info properties in ND_file buffer for an rate ID
! STATUS
!  Complete but untested
! CAUTION
!  The list of properties in ND_file will be overwritten with the list
!  of properties given to this subroutine.  However, this does not apply
!  when saving rate info.  When saving, the full list of properties is not
!  replaced with the contents in ND_file.  Instead, the value of each
!  property if ND_file replaces the value of THOSE properties on disk.
!  Thus properties that are not modified will not be erased.
!  
!  If a rate ID is a link to another set, all properties will be 
!  obtained from the other set.  For this reason, any properties
!  for rate IDs that are links WILL NOT be saved.
! INPUTS
!  NDid: rate ID to modify properties for
!  pcom: cina_common variable needed for reporting errors
!  properties: list of properties and values to modify.  List should be
!              separated using RATE_INFO_PROP_SEP and RATE_INFO_VAL_SEP
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/30/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE change_ND_info(NDid,pcom,properties)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: NDid,properties
      
          LOGICAL(KIND=4)                  :: loop
          INTEGER(KIND=4)                  :: i,NDid_index,z,a
      
! Check z and a from rate id
          CALL decode_NDid(NDid,z=z,a=a)
          IF ((z /= ND_file%z) .AND. (a /= ND_file%a)) CALL report_error( &
               'z and a in rate id are invalid.','Improper usage',pcom,1)
      
! See if NDid is in ND_file buffer
          NDid_index = -1
          DO i = 1, ND_file%NDid_num
             IF (ND_file%NDids(i) == NDid) NDid_index = i
          END DO
          IF (NDid_index == -1) THEN
             NDid_index = ND_file%NDid_num + 1
             IF (NDid_index > MAX_ND_INFO) CALL report_error('ND_file ' // &
                  'buffer is too small to add '//TRIM(NDid),'Developer Reminder', &
                  pcom,1)
      
             ND_file%NDids(NDid_index) = NDid
             ND_file%NDid_num = NDid_index
          END IF
      
! Now that NDid_index is set, save properties to buffer
          ND_file%prop(NDid_index) = properties
        END SUBROUTINE change_ND_info
!***
      
! read_ND_info_no_buf
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Info/update_ND_prop
! NAME
!  SUBROUTINE update_ND_prop(prop_list,prop_mod_list)
! PURPOSE
!  Update prop_list with values from prop_mod_list
! STATUS
!  Complete but untested
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
      
        SUBROUTINE update_ND_prop(prop_list,prop_mod_list)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)      :: prop_mod_list
          CHARACTER(LEN=*),INTENT(INOUT)   :: prop_list
          CHARACTER(LEN=MAX_ND_LEN)        :: prop,mod_list
          INTEGER(KIND=4)                  :: name_end,value_start
          LOGICAL(KIND=4)                  :: loop = .TRUE.
      
! Copy prop_mod_list to mod_list because prop_mod_list can't be modified
          mod_list = prop_mod_list
      
          DO WHILE (loop)
! Get next prop name/value pair
             loop = next_in_list(prop,mod_list,ND_INFO_PROP_SEP)
! Break prop into a name and value
             name_end = INDEX(prop,ND_INFO_VAL_SEP) - 1
      
             IF (name_end > 0) THEN
                value_start = name_end + LEN(ND_INFO_VAL_SEP) + 1
                CALL set_prop_value(prop(:name_end),prop(value_start:), &
                     prop_list,ND_INFO_PROP_SEP,ND_INFO_VAL_SEP)
             END IF
          END DO
        END SUBROUTINE update_ND_prop
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Info/save_ND_info_internal
! NAME
!  SUBROUTINE save_ND_info_internal(pcom,exist,report,NDS_info,
!                                     log_level)
! PURPOSE
!  Save changes in ND_file to isotope file on disk
! STATUS
!  Complete but untested
! CAUTION
!  This should only be called from save_ND_info because it performs all
!  error checking and preparation tasks.
! INPUTS
!  pcom: cina_common variable needed for reporting errors
!  exist: TRUE if iso file existed before calling save_ND_info
!  report: string containing existing report
!  NDS_info: string containing existing NDS_info
!  log_level: controls the level of detail in the report and NDS_info.  
!             Should be 2 for set management, otherwise rate
!             management detail is assumed.
! OUTPUTS
!  report: string with new report appended to existing report
!  NDS_info: string with new NDS_info appended to existing NDS_info
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/01/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE save_ND_info_internal(pcom,exist,report,NDS_info,log_level)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(INOUT)   :: report,NDS_info
          LOGICAL(KIND=4),INTENT(IN)       :: exist
          INTEGER(KIND=4),INTENT(IN)       :: log_level
          CHARACTER(LEN=MAX_NDS_LEN)       :: NDS,old_NDS
          CHARACTER(LEN=MAX_ND_ID_LEN)     :: NDid,next_NDid,old_NDid
          CHARACTER(LEN=MAX_ND_LEN)        :: properties
          CHARACTER(LEN=80)                :: reac_str
          INTEGER(KIND=4)                  :: status,NDid_index,NDid_list_index
          LOGICAL(KIND=4)                  :: link
      
! NDid_index is an index into ND_file%NDids(NDid_index)
! NDid_list_index is an index into NDid_list(NDid_list_index)
      
! Initializations
          IF (exist) THEN
             status = 0
             NDid = ''
          ELSE
             status = -1  ! If file doesn't exist don't try to read it in
          END IF
      
! Setup NDid_list for this isotope and NDSrary
          NDid_list_index = find_NDS_NDid_list(ND_file%set)
          IF (NDid_list_index <= 0) THEN
! Make new entry in NDid_list
             NDid_list_index = get_NDS_list_number() + 1
             IF (NDid_list_index > MAX_NDS) CALL report_error('NDid_list ' // &
                  ' is too full to hold new entry when saving nuclear data ' // &
                  'info for ' // TRIM(ND_file%set),'Improbable',pcom,1)
          END IF
! Fill NDid_list entry values
          NDid_list(NDid_list_index)%set = ND_file%set
          NDid_list(NDid_list_index)%z = ND_file%z
          NDid_list(NDid_list_index)%a = ND_file%a
          NDid_list(NDid_list_index)%NDid_num = 0
          NDid_list(NDid_list_index)%NDid = ''
          NDid_list(NDid_list_index)%seek_pos = -1
      
! Initialize for read_rate_prop
          next_NDid = ''
      
! Read old .rate file, make proper changes, and copy it to the new one
          DO WHILE (status == 0)
             CALL read_ND_prop(pcom,NDid,next_NDid,properties,status)
!!$       print '(/A,I0,6A)','status=',status,' " NDid=',TRIM(NDid), &
!!$            ' next_NDid=',TRIM(next_NDid),' properties=',TRIM(properties)
      
! Search for reaction in ND_file
             NDid_index = find_reac_ND_file(NDid)
             IF (NDid_index > 0) THEN
!print '(A,I0)','NDid_index=',NDid_index
! Update NDid to new one (they will differ if one is a link)
                NDid = ND_file%NDids(NDid_index)
! Mark this NDid as saved
                ND_file%NDids(NDid_index) = ''
      
                CALL decode_NDid(NDid,old_NDS)
      
! Verify that set name in rate id is either a PUBLIC set
! of this set
! link is TRUE if a link to a public set
                old_NDid = NDid
                CALL correct_NDid(NDid,ND_file%set,link)
                CALL decode_NDid(NDid,NDS,unique_reac_str=reac_str)
      
!!$          ! Load properties from source NDS in NDid if necessary
!!$          IF (old_NDS == ' ') THEN
!!$             properties = ND_file%prop(NDid_index)
!!$          ELSE IF (link) THEN
!!$             properties = ''
!!$             NDS = ND_file%set
!!$          ELSE IF (NDS /= old_NDS) THEN
!!$             ! Load properties from source NDS
!!$             properties = read_ND_info_no_buf(old_NDid,pcom)
!!$          END IF
!!$
! Erase properties if this set is a link
                IF (link) THEN
                   properties = ''
                   NDS = ND_file%set
                ELSE
                   properties = ND_file%prop(NDid_index)
                END IF
!print '(3A)','old props="',TRIM(properties),'"'
      
!!$          ! Update properties with modifications in ND_file%prop
!!$          ! if this is not a new rate or a link
!!$          IF ((old_NDS /= '') .AND. (.NOT. link)) CALL update_ND_prop( &
!!$               properties,ND_file%prop(NDid_index))
!print '(3A)','new props="',TRIM(properties),'"'
      
! Add to report and NDS_info
                IF (old_NDS == '' .AND. log_level < 2) THEN
                   report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                        TRIM(NDS) // ' was replaced with new reaction' // ACHAR(8)
                   NDS_info = TRIM(NDS_info) // ', replaced ' // TRIM(reac_str) &
                        // ' with new reaction'
                ELSE IF (log_level < 2) THEN
                   report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                        TRIM(NDS) // ' was replaced with reaction from ' // &
                        TRIM(old_NDS) // ACHAR(8)
                   NDS_info = TRIM(NDS_info) // ', replaced ' // TRIM(reac_str) &
                        // ' with reaction from ' // TRIM(old_NDS)
                END IF
             END IF
      
! Write changes to disk and NDid_list
             CALL write_ND_info(pcom,NDid,properties,NDid_list_index,link)
          END DO
      
!print *,'Adding new rates'
! Add new NDids to .rate file
          DO NDid_index = 1, ND_file%NDid_num
             NDid = ND_file%NDids(NDid_index)
             IF (NDid /= '') THEN
                properties = ND_file%prop(NDid_index)
                CALL decode_NDid(NDid,old_NDS)
      
! Verify that set name in rate id is either a PUBLIC set
! or this set
! link is TRUE if a link to a public set
                old_NDid = NDid
                CALL correct_NDid(NDid,ND_file%set,link)
      
! Add to report and NDS_info
                CALL decode_NDid(NDid,NDS,unique_reac_str=reac_str)
      
! Load properties from source NDS in NDid if necessary
                IF (link) THEN
                   properties = ''
                   NDS = ND_file%set
                END IF
!!$          print '(5A)','NDid=',TRIM(NDid),' new props="', &
!!$               TRIM(properties),'"'
      
! Add to report and NDS_info
                IF (old_NDS == '' .AND. log_level < 2) THEN
                   report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                        TRIM(NDS) // ' was added as a new reaction' // ACHAR(8)
                   NDS_info = TRIM(NDS_info) // ', added ' // TRIM(reac_str)
                ELSE IF (log_level < 2) THEN
                   report = TRIM(report) // TRIM(reac_str) // ' in ' // &
                        TRIM(NDS) // ' was copied from ' // TRIM(old_NDS) // &
                        ACHAR(8)
                   NDS_info = TRIM(NDS_info) // ', copied ' // TRIM(reac_str) // &
                        ' from ' // TRIM(old_NDS)
                END IF
      
! Write changes to disk and NDid_list
                CALL write_ND_info(pcom,NDid,properties,NDid_list_index,link)
             END IF
          END DO
        END SUBROUTINE save_ND_info_internal
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Info/read_ND_prop
! NAME
!  SUBROUTINE read_ND_prop(pcom,NDid,next_NDid,properties,status)
! PURPOSE
!  Read rate properties from an old .rate file for one rate ID
! STATUS
!  Complete
! CAUTION
!  This should only be called from save_ND_info_internal because it
!  performs all error checking and preparations tasks.
! INPUTS
!  pcom: cina_common variable needed for reporting errors
!  next_NDid: storage of next rate ID in file.  This should not be modified
!            by calling procedure and is used for files with multiple
!            rates.  next_NDid should be set to '' before calling this
!            subroutine the first time.
! OUTPUTS
!  NDid: Rate ID that was read in
!  next_NDid: storage of next rate ID in file.  This should not be modified
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
      
        SUBROUTINE read_ND_prop(pcom,NDid,next_NDid,properties,status)
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(INOUT)   :: next_NDid
          CHARACTER(LEN=*),INTENT(OUT)     :: NDid,properties
          INTEGER(KIND=4),INTENT(OUT)      :: status
          CHARACTER(LEN=MAX_ND_LEN)        :: line
      
          NDid = next_NDid
          status = 0
          properties = ''
      
          DO WHILE (status == 0)
             READ(ND_INFO_OLD_UNIT,'(A)',IOSTAT=status) line
      
             IF (status > 0) THEN
                CALL report_error('Error reading line from old .rate &
                    file.', 'File input/output',pcom,1)
             ELSE IF (status /= 0) THEN
! EOF reached
                status = -1
                next_NDid = 'EOF'
! Remove initial property separator from properties
                properties = properties(LEN(ND_INFO_PROP_SEP)+1:)
                RETURN
             END IF
      
! Check if a rate ID or a property
             IF ((INDEX(line,ACHAR(11)) > 0) .AND. &
                  (INDEX(line,ND_INFO_VAL_SEP) == 0)) THEN
! line is a rate ID
! If this is the first rate in a file, continue reading to get the
! properties, and the next rate ID
                IF (next_NDid == '') THEN
                   NDid = line
! Set next_NDid to something that isn't empty so that next case
! will be TRUE and subroutine will return.
                   next_NDid = line
                ELSE
                   next_NDid = line
! Remove initial property separator from properties
                   properties = properties(LEN(ND_INFO_PROP_SEP)+1:)
                   RETURN
                END IF
             ELSE 
! line is a rate property
                properties = TRIM(properties) // ND_INFO_PROP_SEP // &
                     TRIM(line2str(TRIM(line)))
             END IF
          END DO
        END SUBROUTINE read_ND_prop
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Info/find_reac_ND_file
! NAME
!  FUNCTION find_reac_ND_file(NDid)
! PURPOSE
!  Search for a reaction in ND_file
! STATUS
!  Complete but untested
! INPUTS
!  NDid: rate ID to extract reaction and unique string from
! RETURN VALUE
!  Index to a rate ID in ND_file with the same reaction and unique 
!  string if found.  Otherwise -1 is returned
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/01/2004 jpscott       Start subroutine
! SOURCE
      
        FUNCTION find_reac_ND_file(NDid)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)      :: NDid
          CHARACTER(LEN=MAX_ND_ID_LEN)     :: NDid2
          INTEGER(KIND=4)                  :: find_reac_ND_file
          INTEGER(KIND=4)                  :: i,t,start1,start2
      
! Default return value if error
          find_reac_ND_file = -1
      
          start1 = INDEX(NDid,ACHAR(9))
          CALL decode_NDid(NDid,rtype=t)
      
! Return if invalid NDid
          IF (t < 1) RETURN
      
          DO i = 1, ND_file%NDid_num
             NDid2 = ND_file%NDids(i)
             CALL decode_NDid(NDid2,rtype=t)
             start2 = INDEX(NDid2,ACHAR(9))
             IF (t >= 1 .AND. start2 >= 9) THEN
! Check if reaction and unique strings are the same
                IF (NDid(start1:) == NDid2(start2:)) THEN
                   find_reac_ND_file = i
                   RETURN
                END IF
             END IF
          END DO
        END FUNCTION find_reac_ND_file
!***
      
!-------------------------------------------------------------------------
!****is* Nuc_Data_Info/write_ND_info
! NAME
!  SUBROUTINE write_ND_info(pcom,NDid,properties,NDid_list_index,link)
! PURPOSE
!  Write rate properties to disk and NDid_list
! STATUS
!  Complete but untested
! CAUTION
!  This should only be called from save_ND_info_internal because it
!  performs all error checking and preparation tasks.
! INPUTS
!  pcom: cina_common variable needed for reporting errors
!  NDid: rate ID to save to disk
!  properties: properties of rate ID to save to disk
!  NDid_list_index: index into set entry in NDid_list
!  link: TRUE is NDid is a link
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/02/2004 jpscott       Start subroutine
! SOURCE
      
        SUBROUTINE write_ND_info(pcom,NDid,properties,NDid_list_index, &
          link)
          USE io
          IMPLICIT NONE
          TYPE(cina_common),INTENT(IN)    :: pcom
          CHARACTER(LEN=*),INTENT(IN)      :: NDid,properties
          INTEGER(KIND=4),INTENT(IN)       :: NDid_list_index
          LOGICAL(KIND=4),INTENT(IN)       :: link
          CHARACTER(LEN=MAX_ND_LEN)        :: prop,prop_copy
          INTEGER(KIND=4)                  :: seek_pos,status,NDid_index
          LOGICAL(KIND=4)                  :: loop
      
! Get seek position needed for saving NDid_list
          seek_pos = FTELL(ND_INFO_UNIT)
! Write NDid to new .rate file
          WRITE(ND_INFO_UNIT,'(A)',IOSTAT=status) TRIM(NDid)
          IF (status /= 0) CALL report_error('Can not write id to new &
             .rate ' // 'file','File input/output',pcom,1)
      
! Add NDid to NDid_list
          NDid_index = NDid_list(NDid_list_index)%NDid_num + 1
          IF (NDid_index > MAX_ND) CALL report_error('MAX_ND is too '&
               // 'small to hold all rates.','Developer Reminder',pcom,1)
          NDid_list(NDid_list_index)%NDid(NDid_index) = NDid
          NDid_list(NDid_list_index)%seek_pos(NDid_index) = seek_pos
          NDid_list(NDid_list_index)%NDid_num = NDid_index
      
! Make copy of properties because properties can't be modified
          prop_copy = properties
      
! Write properties to new .rate file
          loop = .NOT. link
          DO WHILE (loop)
             loop = next_in_list(prop,prop_copy,ND_INFO_PROP_SEP)
             IF (prop /= '') THEN
                prop = str2line(TRIM(prop))
                CALL print_long_string(TRIM(prop),unit=ND_INFO_UNIT,IOSTAT=status)
                IF (status /= 0) THEN
                   WRITE(prop,'(A,I0,2A)') 'Error ',status, &
                    ' while writing ', 'property to new .rate file'
                   CALL report_error(prop,'File input/output',pcom,1)
                END IF
             END IF
          END DO
        END SUBROUTINE write_ND_info
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Core_Misc/get_NDS_path
! NAME
!  FUNCTION get_NDS_path(set,user,group_name)
! PURPOSE
!  Return a UNIX path and set group name of a rate set
! STATUS
!  Complete
! CAUTION
!  Make sure rate set lists are loaded (read_rset_list) before calling
!  this subroutine.
! INPUTS
!  set: Set name
!  user: Username (needed for accessing correct users NDSraries)
! OUTPUTS
!  group_name (OPTIONAL): Name of set group that set belongs to if
!                         set was found, otherwise an empty string
! RETURN VALUE
!  UNIX path to set root if found, otherwise empty.  path will always
!  end with '/'
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/20/2004 jpscott       Start function
! SOURCE
      
        FUNCTION get_NDS_path(set,user,group_name)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)   :: set,user
          CHARACTER(LEN=*),INTENT(OUT)  :: group_name
          CHARACTER(LEN=MAX_PATH_LEN)   :: get_NDS_path
          CHARACTER(LEN=MAX_NDS_GRP_LEN):: group
          OPTIONAL                      :: group_name
      
! Get set group
          CALL find_NDS_list_index(set,group)
      
! Check if set was found
          IF (group == '') THEN
             IF (PRESENT(group_name)) group_name = ''
             get_NDS_path = ''
             RETURN
          END IF
      
          SELECT CASE (group)
          CASE ('USER ')
             get_NDS_path = TRIM(CINA_PATH) // 'USER/' // &
                  TRIM(user) // '/nuc_data/' // TRIM(set) // '/'
          CASE DEFAULT
             get_NDS_path = TRIM(CINA_PATH) // TRIM(group) // &
                  '/nuc_data/' // TRIM(set) // '/'
          END SELECT
          IF (PRESENT(group_name)) group_name = group
        END FUNCTION get_NDS_path
!***
      
!-------------------------------------------------------------------------
!****if* Nuc_Data_Core_Misc/get_NDS_iso_path
! NAME
!  FUNCTION get_NDS_iso_path(set,z,a,type,user,set_path)
! PURPOSE
!  Return a UNIX path to an isotope file (NDid list or rate info) for a 
!  rate set
! STATUS
!  Complete
! CAUTION
!  Make sure rate set lists are loaded (read_rset_list) before calling
!  this subroutine (unless set_path is used).  If set_path is present, it
!  should end with '/'
! INPUTS
!  set: Set name
!  z: Number of protons of isotope
!  a: Atomic mass of isotope
!  type: Type of file to use and should be 'NDid' or 'iso'
!  user: Username (needed for accessing correct users NDSraries)
!  set_path (OPTIONAL): Path to set instead of using get_rset_path
! RETURN VALUE
!  UNIX path to set isotope file if set is found in rset_list, 
!  otherwise empty
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/30/2004 jpscott       Start function
! SOURCE
      
        FUNCTION get_NDS_iso_path(set,z,a,type,user,set_path)
          IMPLICIT NONE
          CHARACTER(LEN=*),INTENT(IN)   :: set,type,set_path,user
          INTEGER(KIND=4),INTENT(IN)    :: z,a
          CHARACTER(LEN=MAX_PATH_LEN)   :: get_NDS_iso_path
          OPTIONAL                      :: set_path
      
          IF (PRESENT(set_path)) THEN
             WRITE(get_NDS_iso_path,'(A,I0,2A,I0,A,I0,A)') &
             TRIM(set_path),z,'/', TRIM(type),z,'_',a,'.rate'
          ELSE
! Get set path
             get_NDS_iso_path = get_NDS_path(set,user)
! Return blank string if set was not found
             IF (get_NDS_iso_path == '') RETURN
! Add suffix
             WRITE(get_NDS_iso_path,'(A,I0,2A,I0,A,I0,A)') &
                  TRIM(get_NDS_iso_path),z,'/',TRIM(type),z,'_',a, &
                  '.rate'
          END IF
        END FUNCTION get_NDS_iso_path
!***
      
      END MODULE nuc_data_core
      
