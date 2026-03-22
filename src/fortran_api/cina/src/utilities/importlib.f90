!Moved from the original directory and added the CVS info
!
!   $Author: bucknerk $
!   $Id: importlib.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
!   $Log: importlib.f90,v $
!   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
!   all in one place
!
!   Revision 1.2  2007/07/17 19:12:40  bucknerk
!   Don't need this any more.
!
!   
PROGRAM importlib
  USE IFLPORT
  USE convert
  USE constants
  USE reactionstrings
  USE importlib_core
  USE cina_core
  USE rate_man_core
  IMPLICIT NONE

  ! Program options
  LOGICAL(KIND=4),PARAMETER :: IGNORE_1_LINE = .TRUE.
!!$  LOGICAL(KIND=4),PARAMETER :: IGNORE_1_LINE = .FALSE.
  LOGICAL(KIND=4),PARAMETER :: SAVE_LIBRARY = .TRUE.
!!$  LOGICAL(KIND=4),PARAMETER :: SAVE_LIBRARY = .FALSE.
  LOGICAL(KIND=4),PARAMETER :: IGNORE_CAUTIONS = .TRUE.
!!$  LOGICAL(KIND=4),PARAMETER :: IGNORE_CAUTIONS = .FALSE.


  TYPE(cina_common):: pcom
  TYPE(netsu_entry) :: netsu,prev_netsu
  TYPE(lib_entry)   :: lib
  TYPE(reactionparticles) :: r,prev_r
  INTEGER(KIND=4)   :: reac_type,status,i,j,line_number,z_heavy,a_heavy
  INTEGER(KIND=4)   :: reac_type_counter(8) = 0
  INTEGER(KIND=4)   :: netsu_type_counter(8) = 0
  CHARACTER(LEN=MAX_RATE_LEN) :: lib_info
  CHARACTER(LEN=100):: tmps
  LOGICAL(KIND=4)   :: ok_to_save

  CALL GETARG(1,LIB_NAME)
  IF (LIB_NAME == '') THEN
     WRITE(*,'(A)') 'The first argument must be the name of the new library'
     STOP
  END IF

  pcom%ACTION = 'Importlib'
  pcom%USER = 'Importlib'
  reac_type = 1
  status = 0
  line_number = 0
  ok_to_save = .FALSE.

  OPEN(2,FILE='netsu',ACTION='READ')

  IF (IGNORE_1_LINE) THEN
     READ(2,'(A)') tmps
     line_number = line_number + 1
  END IF
  
  IF (SAVE_LIBRARY) THEN
     ! Erase any existing library
     i = SYSTEM('rm -fR '''//TRIM(LIB_NAME)//'''')
     IF (i /= 0) THEN
        WRITE(0,'(A,I0,A)') 'Error ',i,' while removing "library" directory'
        STOP
     END IF

     ! Create library directory
     i = SYSTEM('mkdir '''//TRIM(LIB_NAME)//'''')
     IF (i /= 0) THEN
        WRITE(0,'(A,I0,A)') 'Error ',i, &
             ' while making new "library" directory'
        STOP
     END IF

     ! Create a buffer entry for a user library
     ! The library can be moved to a PUBLIC or SHARED folder later
     CALL add_rlib(LIB_NAME,pcom,'USER',mkdir=.FALSE.)

  END IF

  DO WHILE (status == 0)
     ! Read in entry from netsu file
     READ(2,'(A1,4X,6A5,8X,A4,A1,A1,3X,1PE12.5)',IOSTAT=status) &
          netsu%marker, netsu%name, netsu%desc, netsu%nr, &
          netsu%inv, netsu%q_value
     line_number = line_number + 1
     IF (status == 0) THEN
        READ(2,'(4e13.6)') netsu%parm(1:4)
        READ(2,'(3e13.6)',IOSTAT=status) netsu%parm(5:7)
        line_number = line_number + 2

        ! Check if the marker is for a reaction type or a reaction
        IF (VERIFY(netsu%marker,'12345678') == 0) THEN
           ! A marker for a new reaction type

           ! Save last entry
           IF (ok_to_save) THEN
              tmps = getreac_str(lib%r,1)
              IF (lib%parm_num > 21) WRITE(0,'(3A,I0,A)') &
                   'Notice! Reaction "',TRIM(tmps),'" has ',lib%parm_num, &
                   ' parameters'
              IF (SAVE_LIBRARY) CALL save_entry(lib,pcom)
           END IF

           READ(netsu%marker,'(I1)') reac_type
           WRITE(*,'(A,I1)') 'Parsing reaction type ',reac_type
           ok_to_save = .FALSE.
        ELSE
           ! A Reaction

           ! Convert the name array into a reactionparticles structure
           CALL name2r(netsu%name,r,reac_type,z_heavy,a_heavy,netsu%desc, &
                netsu%type_str)
           CALL validreaction(r,i,tmps)
           SELECT CASE (i)
           CASE (0,-2)
              ! Do nothing
           CASE DEFAULT
              WRITE(0,'(A,I0,3A,I0)') 'Caution! Error ',i,' (',TRIM(tmps), &
                   ') while validating reaction for line ',line_number-2
              tmps = getreac_str(r,1)
              WRITE(0,'(4A)') TRIM(tmps),' (',TRIM(ADJUSTL(netsu%desc)),')'
              
              ! Exit if saving library and an error occurs
              IF (SAVE_LIBRARY .AND. (.NOT. IGNORE_CAUTIONS)) THEN
                 WRITE(0,'(A)') 'Program aborting because of error ' // &
                      'interpreting reaction.'
                 STOP
              END IF
           END SELECT

!!$           ! Print reaction string to screen
!!$           tmps = getreac_str(r,1)
!!$           WRITE(*,'(A)') TRIM(tmps)

           ! If netsu entry is same as previous lib entry, 
           ! just add the parameters to the lib entry
           IF (same_reaction(r,prev_r)) THEN
              ! An additional set of parameters for a previous netsu entry
              ! Check if lib%parm can hold new parameters
              IF (UBOUND(lib%parm,1) < lib%parm_num + 7) THEN
                 WRITE(*,'(A,I0)') 'MAX_A should be at least ', &
                      lib%parm_num + 7
                 STOP
              END IF

              IF (netsu%desc /= prev_netsu%desc) THEN
                 tmps = getreac_str(r,1)
                 WRITE(0,'(7A,I0)') 'Notice! Same reaction "',TRIM(tmps), &
                      '" has different biblio codes (',TRIM(netsu%desc), &
                      ',', &
                      TRIM(prev_netsu%desc),') near line ',line_number - 2
!!$                 ! Exit if saving library and an error occurs
!!$                 IF (SAVE_LIBRARY .AND. (.NOT. IGNORE_CAUTIONS)) THEN
!!$                    WRITE(0,'(A)') 'Program aborting because of error with reaction.'
!!$                    STOP
!!$                 END IF
              END IF

              IF (netsu%inv /= prev_netsu%inv) THEN
                 tmps = getreac_str(r,1)
                 WRITE(0,'(7A,I0)') 'Caution! Same reaction "',TRIM(tmps), &
                      '" has different inverse flags (',TRIM(netsu%inv),','&
                      ,TRIM(prev_netsu%inv),') near line ',line_number - 2
                 ! Exit if saving library and an error occurs
                 IF (SAVE_LIBRARY .AND. (.NOT. IGNORE_CAUTIONS)) THEN
                    WRITE(0,'(A)') 'Program aborting because of error ' // &
                         'with reaction.'
                    STOP
                 END IF
              END IF

              IF (netsu%q_value /= prev_netsu%q_value) THEN
                 tmps = getreac_str(r,1)
                 WRITE(0,'(1P,3A,E12.5,A,E12.5,A,I0)') 'Caution! ' // &
                      'Same reaction "',TRIM(tmps), &
                      '" has different q_values (',netsu%q_value,',', &
                      prev_netsu%q_value,') near line ',line_number - 2
                 ! Exit if saving library and an error occurs
                 IF (SAVE_LIBRARY .AND. (.NOT. IGNORE_CAUTIONS)) THEN
                    WRITE(0,'(A)') 'Program aborting because of error ' // &
                         'with reaction.'
                    STOP
                 END IF
              END IF

              lib%parm(lib%parm_num+1:lib%parm_num+7) = netsu%parm
              IF (netsu%nr == 'n') THEN
                 lib%resonant = TRIM(lib%resonant) // ',nr'
              ELSE IF (netsu%nr == 'r') THEN
                 lib%resonant = TRIM(lib%resonant) // ',r'
              ELSE IF (netsu%nr == '') THEN
                 tmps = getreac_str(lib%r,1)
                 WRITE(0,'(2A,I0,A)') TRIM(tmps),' is missing resonant/nonresonant flag near line ',line_number-2,'. Assuming nr'
                 IF (.NOT. IGNORE_CAUTIONS) STOP
                 lib%resonant = TRIM(lib%resonant) // ',nr'
              ELSE
                 tmps = getreac_str(lib%r,1)
                 WRITE(0,'(4A,I0,A)') TRIM(tmps),' has resonant/nonresonant flag set to ',TRIM(netsu%nr),' near line ',line_number-2,'. Assuming nr'
                 IF (.NOT. IGNORE_CAUTIONS) STOP
                 lib%resonant = TRIM(lib%resonant) // ',nr'
              END IF
              !print *,'2b: ',TRIM(lib%resonant)
              lib%parm_num = lib%parm_num + 7

           ELSE
              ! A new reaction instead of an additional set of parameters
              
              ! Save last entry
              IF (ok_to_save) THEN
                 tmps = getreac_str(lib%r,1)
                 IF (lib%parm_num > 21) WRITE(0,'(3A,I0,A)') &
                      'Notice! Reaction "',TRIM(tmps),'" has ', &
                      lib%parm_num,' parameters'
                 IF (SAVE_LIBRARY) CALL save_entry(lib,pcom)
              END IF

              ! convert netsu entry into lib entry
              lib%r = r
              lib%desc = netsu%desc
              IF (netsu%nr == 'n') THEN
                 lib%resonant = 'nr'
              ELSE IF (netsu%nr == 'r') THEN
                 lib%resonant = 'r'
              ELSE IF (netsu%nr == '') THEN
                 tmps = getreac_str(lib%r,1)
                 WRITE(0,'(2A,I0,A)') TRIM(tmps),' is missing resonant/nonresonant flag near line ',line_number-2,'. Assuming nr'
                 IF (.NOT. IGNORE_CAUTIONS) STOP
                 lib%resonant = 'nr'
              ELSE
                 tmps = getreac_str(lib%r,1)
                 WRITE(0,'(4A,I0,A)') TRIM(tmps),' has resonant/nonresonant flag set to ',TRIM(netsu%nr),' near line ',line_number-2,'. Assuming nr'
                 IF (.NOT. IGNORE_CAUTIONS) STOP
                 lib%resonant = 'nr'
              END IF
              !print *,'2a: ',TRIM(lib%resonant)
              IF (netsu%inv == 'v') THEN
                 lib%inverse = .TRUE.
              ELSE
                 lib%inverse = .FALSE.
              END IF
              lib%q_value = netsu%q_value
              lib%parm(1:7) = netsu%parm
              lib%parm_num = 7
              lib%z_heavy = z_heavy
              lib%a_heavy = a_heavy
              lib%type_str = netsu%type_str

              reac_type_counter(reac_type) = reac_type_counter(reac_type) +1
           END IF
           netsu_type_counter(reac_type) = netsu_type_counter(reac_type) + 1
           ok_to_save = .TRUE.
        END IF
        
        !IF (reac_type_counter(1) > 10) STOP
        prev_netsu = netsu
        prev_r = r
     END IF
  END DO
  line_number = line_number - 1  ! Don't count the EOF as a line
  IF (ok_to_save .OR. netsu_type_counter(8) == 0) THEN
     tmps = getreac_str(lib%r,1)
     IF (lib%parm_num > 21) WRITE(0,'(3A,I0,A)') &
          'Notice! Reaction "',TRIM(tmps),'" has ',lib%parm_num, &
          ' parameters'
     IF (SAVE_LIBRARY) THEN
        CALL save_entry(lib,pcom)           ! Save last entry

        CALL save_rlib_iso_list(LIB_NAME,pcom,TRIM(LIB_NAME)//'/')

        lib_info = 'Library Notes=' // ACHAR(9) // &
             'Creation Date=' // get_date() // ' ' // get_time() // &
             ACHAR(9) // 'Library Recipe=This library was imported by ' // &
             'nucastrodata.org coordinator.' // ACHAR(9) // &
             'All Inverses Present=NO'

        CALL save_rlib_info(LIB_NAME,pcom,TRIM(lib_info), &
             TRIM(LIB_NAME)//'/')
     END IF
  END IF

  ! Print summary

  WRITE(*,'(/I0,A/)') line_number,' lines in netsu have been parsed'
  WRITE(*,'(A,T17,A,T40,A)') 'Reaction type','Number of reactions', &
       'Number of netsu entries'
  DO i = 1,8
     WRITE(*,'(I0,T17,I0,T40,I0)') i,reac_type_counter(i), &
          netsu_type_counter(i)
  END DO
  WRITE(*,'(/A,I0)') 'Total number of reactions is ',SUM(reac_type_counter)
  WRITE(*,'(A,I0)') 'Total number of entries in netsu files is ', &
       SUM(netsu_type_counter)
END PROGRAM importlib

