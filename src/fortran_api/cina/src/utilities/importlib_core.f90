!Moved from the original directory and added the CVS info
!
!   $Author: bucknerk $
!   $Id: importlib_core.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
!   $Log: importlib_core.f90,v $
!   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
!   all in one place
!
!   Revision 1.2  2007/07/17 19:12:40  bucknerk
!   Don't need this any more.
!
!   
MODULE importlib_core
  USE IFLPORT
  USE reactionstrings
  USE constants
  USE cina_core
  USE rate_man_core

  TYPE,PUBLIC   :: netsu_entry
     CHARACTER(LEN=1) :: marker = ''
     CHARACTER(LEN=5) :: name(6) = ''
     CHARACTER(LEN=4) :: desc = ''
     CHARACTER(LEN=1) :: nr = ''
     CHARACTER(LEN=1) :: inv = ''
     REAL(KIND=8)     :: q_value = 0D0
     REAL(KIND=8)     :: parm(7) = 0D0
     CHARACTER(LEN=10):: type_str = ''
  END TYPE netsu_entry

  TYPE,PUBLIC   :: lib_entry
     TYPE(reactionparticles) :: r
     CHARACTER(LEN=4) :: desc = ''
     CHARACTER(LEN=50):: resonant = ''
     LOGICAL(KIND=4)  :: inverse = .FALSE.
     REAL(KIND=8)     :: q_value = 0D0
     REAL(KIND=8)     :: parm(MAX_A) = 0D0
     INTEGER(KIND=4)  :: parm_num = 0
     INTEGER(KIND=4)  :: z_heavy = -1
     INTEGER(KIND=4)  :: a_heavy = -1
     CHARACTER(LEN=10):: type_str = ''
  END TYPE lib_entry

  CHARACTER(LEN=200),PUBLIC :: LIB_NAME='library'

CONTAINS
  SUBROUTINE save_entry(lib_in,pcom)
    IMPLICIT NONE
    TYPE(cina_common),INTENT(IN):: pcom
    TYPE(lib_entry),INTENT(IN)  :: lib_in
    TYPE(lib_entry)             :: lib
    CHARACTER(LEN=2000) :: parm
    CHARACTER(LEN=100) :: reac_str
    CHARACTER(LEN=20)  :: rtype
    CHARACTER(LEN=MAX_RID_LEN)  :: rid
    CHARACTER(LEN=MAX_RATE_LEN) :: prop
    INTEGER(KIND=4)    :: i

    !CALL add_isotope(LIB_NAME,lib%z_heavy,lib%a_heavy,NEW_LIBRARY=.TRUE.)
    lib = lib_in

    parm = ''
    DO i = 1, lib%parm_num
       IF (i == 1) THEN
          WRITE(parm,'(1P,E12.5)') lib%parm(1)
       ELSE
          WRITE(parm,'(1P,A,E12.5)') TRIM(parm) // ',',lib%parm(i)
       END IF
    END DO
    reac_str = getreac_str(lib%r,1)
    i = getreactype(lib%r)
    lib%desc = ADJUSTL(lib%desc)

    WRITE(rid,'(I2.2,2I3.3,5A)') i,lib%z_heavy,lib%a_heavy,TRIM(LIB_NAME), &
         ACHAR(9),TRIM(reac_str),ACHAR(11),TRIM(lib%type_str)
    IF (lib%type_str == '') THEN
       WRITE(rtype,'(I0)') i
    ELSE
       WRITE(rtype,'(I0,A)') i,','//TRIM(lib%type_str)
    END IF
    IF (lib%inverse) rtype = TRIM(rtype) // ',v'

    !print *,'3: ',TRIM(lib%resonant)
    IF (lib%resonant == '') THEN
       lib%resonant = 'nr'
       IF (lib%type_str == '') THEN
          CALL report_error(TRIM(reac_str)//' is not a decay and has ' // &
               'no resonance flag','Improper usage',pcom,0)
       END IF
    END IF

!!$    WRITE(*,'(3A,I0,A,I0)') 'Saving ',TRIM(reac_str),' under z=', &
!!$         lib%z_heavy,' a=',lib%a_heavy

    WRITE(prop,'(1P,4A,E12.5,2(A,I0),3A,I0,2A)') &
         'Reaction String = ' // TRIM(reac_str), ACHAR(9) // &
         'Reaction Type = ' // TRIM(rtype), ACHAR(9) // &
         'Biblio Code = ' // TRIM(lib%desc), ACHAR(9) // &
         'Q-value = ', lib%q_value, ACHAR(9) // &
         'Number of Reactants = ', getreac_num(lib%r), ACHAR(9) // &
         'Number of Products = ', getprod_num(lib%r), ACHAR(9) // &
         'Resonant Components = ', TRIM(lib%resonant), ACHAR(9) // &
         'Number of Parameters = ', lib%parm_num, ACHAR(9) // &
         'Parameters = ',TRIM(parm)

    !WRITE(*,'(A)') TRIM(prop)

    CALL open_rate_info(LIB_NAME,pcom,lib%z_heavy,lib%a_heavy)
    CALL change_rate_info(rid,pcom,prop)
    CALL save_rate_info(pcom,parm,reac_str,path=TRIM(LIB_NAME)//'/')

  END SUBROUTINE save_entry

  SUBROUTINE name2r(name,r,type,z_heavy,a_heavy,biblio,type_str)
    IMPLICIT NONE
    CHARACTER(LEN=5),INTENT(IN) :: name(6)
    CHARACTER(LEN=*),INTENT(IN) :: biblio
    CHARACTER(LEN=10),INTENT(OUT):: type_str
    TYPE(reactionparticles),INTENT(OUT) :: r
    INTEGER(KIND=4),INTENT(IN)  :: type
    INTEGER(KIND=4),INTENT(OUT) :: z_heavy,a_heavy
    CHARACTER(LEN=10):: special_num
    CHARACTER(LEN=5) :: symbol,n,tmps
    INTEGER(KIND=4) :: i,j,z,a,reac_num
    
    tmps = ADJUSTL(biblio)
    SELECT CASE (tmps)
    CASE ('bet+', 'btyk')
       type_str = 'bet+'
    CASE ('bet-', 'bkmo', 'mo92')
       type_str = 'bet-'
    CASE ('bec')
       type_str = 'bec'
    CASE ('ec')
       type_str = 'ec'
    ! Flags for Jacob Fiskers XRB library
    CASE ('ffn', 'bex+', 'bqa+', 'bhi+', 'aexp')
       type_str = 'ec'
    CASE DEFAULT
       type_str = ''
    END SELECT
    CALL setreactype(r,type,type_str)
    
    reac_num = getreac_num(r)
    z_heavy = -1
    a_heavy = -1
    
    ! Put name(i) into r
    DO i = 1,6
       n = ADJUSTL(name(i))
       
       IF (n /= '') THEN
          ! Index to last character in symbol
          j = VERIFY(n,'0123456789 ',BACK=.TRUE.) 
          symbol = n(:j)
          READ(n(j+1:),'(I)') a

          IF (a > 0) THEN
             ! Capitalize first letter of symbol
             symbol(1:1) = ACHAR(IACHAR(symbol(1:1))-32)
             ! Add mass to symbol
             WRITE(symbol,'(A,I0)') TRIM(symbol),a
          END IF
          ! Get z and a for isotope
          j = 1
          CALL str2za(symbol,z,a,j,special_num)
          !IF (special_num /= '') print *,n,'"',TRIM(special_num),'"'

          ! Save z and a into r
          IF (i <= reac_num) THEN
             CALL setreac(r,i,z,a,special_num)

             ! Find heaviest reactant
             IF (i == 1) THEN
                z_heavy = z
                a_heavy = a
             ELSE
                IF (z == z_heavy) THEN
                   IF (a > a_heavy) THEN
                      z_heavy = z
                      a_heavy = a
                   END IF
                ELSE IF (z > z_heavy) THEN
                   z_heavy = z
                   a_heavy = a
                END IF
             END IF
          ELSE
             CALL setprod(r,i-reac_num,z,a,special_num)
          END IF
       END IF
    END DO
  END SUBROUTINE name2r
END MODULE importlib_core
