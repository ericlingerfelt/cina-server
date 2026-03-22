MODULE options
!DESC = This module contains routines for parsing command line options
  USE constants
  USE reactionstrings
  USE IFLPORT

! By default all procedures and global variables are private
  PRIVATE

  PUBLIC                           :: options_ver,get_fmtoutput_opt
  PUBLIC                           :: get_inputread_opt,get_inputcheck_opt

  TYPE,PUBLIC                      :: generaloptions
     LOGICAL                       :: help = .FALSE.
     LOGICAL                       :: version = .FALSE.
  END TYPE generaloptions

  TYPE,PUBLIC                      :: incheckoptions
     LOGICAL                       :: help = .FALSE.
     LOGICAL                       :: version = .FALSE.
     LOGICAL                       :: positivechk = .TRUE.
     LOGICAL                       :: singlevaluedchk = .TRUE.
     LOGICAL                       :: rangechk = .TRUE.
     LOGICAL                       :: continuouschk = .TRUE.
     LOGICAL                       :: errorvaluechk = .TRUE.
     LOGICAL                       :: reactionchk = .TRUE.
  END TYPE incheckoptions

  TYPE,PUBLIC                      :: fmtoutoptions
     CHARACTER(LEN=100)            :: formats = ''
     CHARACTER(LEN=100)            :: out_file = ''
     CHARACTER(LEN=100)            :: parm_file = ''
     CHARACTER(LEN=100)            :: iread_file = ''
     CHARACTER(LEN=100)            :: inv_file = ''
     CHARACTER(LEN=30)             :: biblio = ''
     CHARACTER(LEN=100)            :: desc = ''
     REAL(KIND=8)                  :: qvalue = 0E0
     LOGICAL(KIND=1)               :: ec = .FALSE.    ! electron capture decay reaction?
     LOGICAL(KIND=1)               :: bplus = .FALSE. ! beta plus decay reaction?
     REAL(KIND=8),DIMENSION(MAX_A) :: a
     INTEGER(KIND=4)               :: a_num = 0
     REAL(KIND=8),DIMENSION(MAX_A) :: a_inv
     INTEGER(KIND=4)               :: a_inv_num = 0
     TYPE(reactionparticles)       :: r
  END TYPE fmtoutoptions

CONTAINS
!---------------------------------------------------------------------
  FUNCTION options_ver
!PURPOSE = Return the cvs revision number for this file
!STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: options_ver
    CHARACTER(LEN=20),PARAMETER   :: OPTIONS_VERSION = '$Revision: 1.1.1.1 $'

    options_ver = OPTIONS_VERSION(12:LEN_TRIM(OPTIONS_VERSION)-2)

  END FUNCTION options_ver

!---------------------------------------------------------------------
  SUBROUTINE get_inputread_opt(fileio_opt,gen_opt)
!PURPOSE = Get, parse, and return the inputread command line options
!STATUS = Complete but briefly tested
!DESC = This function returns if no errors occurred
    USE fileio
    USE io
    USE convert
    USE reactionstrings
    IMPLICIT NONE
    TYPE(fileiooptions),INTENT(OUT):: fileio_opt
    TYPE(generaloptions),INTENT(OUT):: gen_opt

    CHARACTER(LEN=80)              :: tmps,tmps2,mes           ! Temporary strings
    CHARACTER(LEN=80)              :: reacstr,reactype
    INTEGER(KIND=4)                :: n,i
!EXTERNAL                       :: IARGC

    reacstr = ''
    reactype = ''

! Read in the number of command-line arguments
    n = IARGC()

    IF (n < 1) THEN
       gen_opt%help = .TRUE.
       RETURN
    END IF

    CALL GETARG(1,fileio_opt%file_in)
    CALL GETARG(2,fileio_opt%data_type)

! Read in command-line arguments
    i = 1
    fileio_opt%file_out = ''
    DO WHILE(i <= n)
       CALL GETARG(i,tmps)
       CALL GETARG(i+1,tmps2)
       SELECT CASE (TRIM(lowercase(tmps)))
       CASE ('-h ','--help ')
          gen_opt%help = .TRUE.
          RETURN
       CASE ('-v ','--version ')
          gen_opt%version = .TRUE.
          RETURN
       CASE ('-eu ')
          eunit: SELECT CASE (TRIM(lowercase(tmps2)))
          CASE ('ev ')
             fileio_opt%e_units_in = 1D0
          CASE ('kev ')
             fileio_opt%e_units_in = 1D3
          CASE ('mev ')
             fileio_opt%e_units_in = 1D6
          CASE ('gev ')
             fileio_opt%e_units_in = 1D9
          CASE DEFAULT
             WRITE(*,'(A)') 'Known energy units: ev, kev, mev, gev'
             CALL printerror('Unknown input energy unit "'//TRIM(tmps2)//'"',1)
          END SELECT eunit
          i = i + 1
       CASE ('-su ')
          sunit: SELECT CASE (TRIM(lowercase(tmps2)))
          CASE ('ev-b ')
             fileio_opt%s_units_in = 1D0
          CASE ('kev-b ')
             fileio_opt%s_units_in = 1D3
          CASE ('mev-b ')
             fileio_opt%s_units_in = 1D6
          CASE ('gev-b ')
             fileio_opt%s_units_in = 1D9
          CASE DEFAULT
             WRITE(*,'(A)') 'Known S-factor units: ev-b, kev-b, mev-b, gev-b'
             CALL printerror('Unknown input S-factor unit "'//TRIM(tmps2)//'"',1)
          END SELECT sunit
          i = i + 1
       CASE ('-csu ')
          csunit: SELECT CASE (TRIM(tmps2))
          CASE ('b ')
             fileio_opt%cs_units_in = 1D0
          CASE ('mb ')
             fileio_opt%cs_units_in = 1D-3
          CASE ('ub ')
             fileio_opt%cs_units_in = 1D-6
          CASE ('nb ')
             fileio_opt%cs_units_in = 1D-9
          CASE ('pb ')
             fileio_opt%cs_units_in = 1D-12
          CASE DEFAULT
             WRITE(*,'(A)') 'Known cross section units: b, mb, ub, nb, pb'
             CALL printerror('Unknown input cross section unit "'//TRIM(tmps2)//'"',1)
          END SELECT csunit
          i = i + 1
       CASE ('-tu ')
          tunit: SELECT CASE (TRIM(lowercase(tmps2)))
          CASE ('t0 ')
             fileio_opt%t_units_in = 1D0
          CASE ('t3 ')
             fileio_opt%t_units_in = 1D3
          CASE ('t6 ')
             fileio_opt%t_units_in = 1D6
          CASE ('t9 ')
             fileio_opt%t_units_in = 1D9
          CASE DEFAULT
             WRITE(*,'(A)') 'Known temperature units (K): T0,T3,T6,T9'
             CALL printerror('Unknown temperature unit "'//TRIM(tmps2)//'"',1)
          END SELECT tunit
          i = i + 1
       CASE ('-p ')
          READ(tmps2,'(I)') fileio_opt%plevel
          IF ((fileio_opt%plevel > 4) .OR. (fileio_opt%plevel < 0)) THEN
             WRITE(*,'(A)') 'Valid print levels: 0, 1, 2, 3, 4'
             CALL printerror('Invalid print level "'//TRIM(tmps2)//'"',1)
          END IF
          i = i + 1
       CASE ('-f ','-format ')
          fileio_opt%data_format = tmps2
          i = i + 1
       CASE ('-fo ','-formatoptions ')
          fileio_opt%fmt_options = tmps2
          i = i + 1
       CASE ('-o ','-output ')
          fileio_opt%file_out = tmps2
          i = i + 1
       CASE ('-r ','-reaction ')
          reacstr = tmps2
          i = i + 1
       CASE ('-rt ')
          reactype = tmps2
          i = i + 1
       CASE ('-n ','-notes ')
          CALL GETARG(i+1,fileio_opt%notes)
          i = i + 1
       CASE DEFAULT
          IF (i > 2) CALL printerror('Unknown option "'//TRIM(tmps)//'"',1)
       END SELECT
       i = i + 1
    END DO

    IF (n < 2) gen_opt%help = .TRUE.

    IF (reacstr /= '') THEN
       CALL read_reac_str(fileio_opt%r,reacstr,mes,reactype)
       IF (LEN_TRIM(mes) > 0) THEN
          WRITE(*,'(A)') TRIM(mes)
          CALL printerror('Could not read in reaction string',1)
       END IF
    END IF

  END SUBROUTINE get_inputread_opt

!---------------------------------------------------------------------
  SUBROUTINE get_inputcheck_opt(fileio_opt,ichk_opt)
!PURPOSE = Get, parse, and return the inputcheck command line options
!STATUS = Not finished
!DESC = This function returns if no errors occurred
    USE fileio
    USE io
    USE convert
    IMPLICIT NONE
    TYPE(fileiooptions),INTENT(OUT):: fileio_opt
    TYPE(incheckoptions),INTENT(OUT):: ichk_opt

    CHARACTER(LEN=80)              :: tmps,tmps2               ! Temporary strings
    INTEGER(KIND=4)                :: n,i
!EXTERNAL                       :: IARGC

! Read in the number of command-line arguments
    n = IARGC()

    IF (n < 1) THEN
       ichk_opt%help = .TRUE.
       RETURN
    END IF

! Read in command-line arguments
    i = 1
    DO WHILE(i <= n)
       CALL GETARG(i,tmps)
       CALL GETARG(i+1,tmps2)
       SELECT CASE (TRIM(lowercase(tmps)))
       CASE ('-h ','--help ')
          ichk_opt%help = .TRUE.
          RETURN
       CASE ('-v ','--version ')
          ichk_opt%version = .TRUE.
          RETURN
       CASE ('-p ')
          READ(tmps2,'(I)') fileio_opt%plevel
          IF ((fileio_opt%plevel > 4) .OR. (fileio_opt%plevel < 0)) THEN
             WRITE(*,'(A)') 'Valid print levels: 0, 1, 2, 3, 4'
             CALL printerror('Invalid print level "'//TRIM(tmps2)//'"',1)
          END IF
          i = i + 1
       CASE ('-positive ','-pos ')
          ichk_opt%positivechk = .FALSE.
       CASE ('-singlevalued ','-sv ')
          ichk_opt%singlevaluedchk = .FALSE.
       CASE ('-range ','-r ')
          ichk_opt%rangechk = .FALSE.
       CASE ('-continuous ','-c ')
          ichk_opt%continuouschk = .FALSE.
       CASE ('-errorvalue ','-ev ')
          ichk_opt%errorvaluechk = .FALSE.
       CASE ('-reaction ','-rc ')
          ichk_opt%reactionchk = .FALSE.
       CASE DEFAULT
          IF (i > 1) CALL printerror('Unknown option "'//TRIM(tmps)//'"',1)
       END SELECT
       i = i + 1
    END DO

    CALL GETARG(1,fileio_opt%file_in)

! Make sure that the input file doesn't begin with a '-'
    IF (fileio_opt%file_in(1:1) == '-') CALL printerror('Input file can not begin with a "-"',1)

! Make sure input filename extension is .inrd
    i = LEN_TRIM(fileio_opt%file_in)
    IF (i > 5) THEN
       IF (fileio_opt%file_in(i-4:) == '.inrd') THEN
          tmps = fileio_opt%file_in(1:i-5)
       ELSE
          WRITE(*,'(A)') 'inputread must be executed before inputcheck'
          CALL printerror('Input filename must end in ".inrd"',1)
       END IF
    ELSE
       WRITE(*,'(A)') 'inputread must be executed before inputcheck'
       CALL printerror('Input filename must end in ".inrd"',1)
    END IF

! Set output file to the same as input file with a different extension
    fileio_opt%file_out = TRIM(tmps) // '.inck'

  END SUBROUTINE get_inputcheck_opt

!---------------------------------------------------------------------
  SUBROUTINE get_fmtoutput_opt(gen_opt,opt)
!PURPOSE = Get, parse, and return the fmtoutput command line options
!STATUS = 
    USE convert
    USE io
    IMPLICIT NONE
    TYPE(generaloptions),INTENT(OUT) :: gen_opt
    TYPE(fmtoutoptions),INTENT(OUT)  :: opt
    CHARACTER(LEN=80)                :: tmps,tmps2               ! Temporary strings
    INTEGER(KIND=4)                  :: n,i,j,s
!EXTERNAL                         :: IARGC

! Read in the number of command-line arguments
    n = IARGC()

    IF (n < 1) THEN
       gen_opt%help = .TRUE.
       RETURN
    END IF

    CALL GETARG(1,opt%formats)
    CALL GETARG(2,opt%out_file)
    CALL GETARG(3,opt%parm_file)

! Read in command-line arguments
    i = 1

    DO WHILE(i <= n)
       CALL GETARG(i,tmps)
       CALL GETARG(i+1,tmps2)
       SELECT CASE (TRIM(lowercase(tmps)))
       CASE ('-h ','--help ')
          gen_opt%help = .TRUE.
          RETURN
       CASE ('-v ','--version ')
          gen_opt%version = .TRUE.
          RETURN
       CASE ('-inv ','--inverse ')
          opt%inv_file = tmps2
          i = i + 1
       CASE ('-ir ','--inputread ')
          opt%iread_file = tmps2
          i = i + 1
       CASE ('-b ')
          opt%biblio = tmps2
          i = i + 1
       CASE ('-q ','--qvalue ')
          j = 1   ! Start looking in character position 1 in tmps2 for a number
          CALL nextnum(tmps2,opt%qvalue,j,s)
          SELECT CASE (s)
          CASE (0)
! Do nothing, no errors
          CASE (1)
             CALL printerror('The Q value "'//TRIM(tmps2)//'" could not be represented.',0)
          CASE (2)
             WRITE(tmps2,'(G)') opt%qvalue
             CALL printerror('The Q value was truncated to '//TRIM(tmps2),0)
          CASE DEFAULT
             WRITE(tmps2,'(I0)') s
             CALL printerror('Developer reminder: Unknown status "'//TRIM(tmps2)// &
                  '" while reading Q value.',0)
          END SELECT
          i = i + 1
       CASE ('-ec ')
          IF (lowercase(tmps2) == 'yes') opt%ec = .TRUE.
          i = i + 1
       CASE ('-desc ')
          opt%desc = tmps2
          i = i + 1
       CASE DEFAULT
          IF (i > 3) CALL printerror('Unknown option "'//TRIM(tmps)//'"',1)
       END SELECT
       i = i + 1
    END DO

    IF (n < 3) gen_opt%help = .TRUE.

  END SUBROUTINE get_fmtoutput_opt

!---------------------------------------------------------------------
END MODULE options
