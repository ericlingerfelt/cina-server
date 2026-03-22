PROGRAM cina
  !PURPOSE = Execute CGI requests and return results to webserver
  !STATUS = Complete and tested
  !DESC =
  USE IFLPORT
  USE cina_core
  USE rategen
  USE element_sim
  USE rate_man
  USE nuc_data_man
  IMPLICIT NONE
  CHARACTER(LEN=9),PARAMETER       :: PROGRAM_VER = '0.1'
  TYPE(cina_common)               :: pcom
  INTEGER(KIND=4)                  :: i
  CHARACTER(LEN=20)                :: id

  ! Print HEADER to tell webserver what response cina is returning
  WRITE(*,'(A/)') 'Content-Type: text/plain'

  ! Check for command line options and return if none
  CALL chk_options(pcom)
  IF (pcom%help /= 0) CALL print_cina_help(PROGRAM_VER)
  IF (pcom%version /= 0) CALL print_cina_ver(PROGRAM_VER)

  ! Check for correct CGI environment variables and return if no problems
  CALL chk_cgi_env

  ! Get HEADER, ID, ACTION, USER, PW, etc.
  pcom = get_cina_common()

  ! Remove unneeded environment variables
  CALL rm_env_var

  ! Check for correct HEADER version
  SELECT CASE (pcom%HEADER)
  CASE ('2.7', '2.8')
     ! Do nothing
  CASE DEFAULT
     CALL report_error('The server software has been upgraded '//      &
          'to the point that this program must be updated.'//LF//      &
          'Update this program by closing it and launching it again.'//LF// &
          'The software version is '//TRIM(pcom%HEADER)//LF// &
          'Please launch this suite from http://nucastrodata.org/',    &
          'Improper usage',pcom,1)
  END SELECT


  ! Make sure that user is authencated by now
  IF (pcom%SESSIONP <= 0) CALL report_error('A software bug was found on the server and the coordinator has been notified with a bug report.','Improbable',pcom,1)

  SELECT CASE (pcom%ACTION)
  CASE ('CHECK REACTION ')
     CALL chk_reaction(pcom)
  CASE ('GET INVERSE REACTION ')
     CALL get_inv_reaction(pcom)
  CASE ('READ INPUT ')
     CALL read_input(pcom)
  CASE ('PREPROCESSING ')
     CALL preprocess(pcom)
  CASE ('GENERATE RATE ')
     CALL generate_rate(pcom)
  CASE ('ABORT RATE GENERATION ')
     CALL abort_rate_gen(pcom)
  CASE ('RATE GENERATION UPDATE ')
     CALL rategen_update(pcom)
  CASE ('RATE GENERATION STATUS FILE ')
     CALL report_error('Rate generation status files have not been implemented.', &
          'Missing feature',pcom,1)
  CASE ('RATE GENERATION OUTPUT ')
     CALL rategen_output(pcom)
  CASE ('PARAMETERIZE RATE ')
     CALL generate_parm(pcom)
  CASE ('ABORT RATE PARAMETERIZATION ')
     CALL abort_rate_parm(pcom)
  CASE ('RATE PARAMETERIZATION UPDATE ')
     CALL rateparm_update(pcom)
  CASE ('RATE PARAMETERIZATION STATUS FILE ')
     CALL report_error('Rate parameterization status files have not been implemented.', &
          'Missing feature',pcom,1)
  CASE ('RATE PARAMETERIZATION OUTPUT ')
     CALL rateparm_output(pcom)
  CASE ('INVERSE PARAMETERS ')
     CALL inverse_parm(pcom)
  CASE ('GENERATE PARAMETER FORMAT ')
     CALL gen_parm_fmt(pcom)
  CASE ('LOGOUT ')
     CALL CGI_logout(pcom)
  CASE ('GET TIMEOUT ')
     WRITE(*,'(A,I0)') 'TIMEOUT=',pcom%TIMEOUT
  CASE ('DEBUG ')
     CALL print_input(pcom)
!!$     i = SYSTEM('/bin/env')
!!$     WRITE(*,'(A)') '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!!$     i = SYSTEM('set')
!!$     WRITE(*,'(A)') '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
  CASE ('LOG ')
     CALL report_error(TRIM(pcom%BODY),'External log',pcom,3)
     WRITE(*,'(A)') 'LOG=SUCCESS'
  CASE ('GET RATE LIBRARY LIST ')
     CALL get_rlib_list(pcom)
  CASE ('GET RATE LIBRARY INFO ')
     CALL get_rlib_info(pcom)
  CASE ('GET RATE LIBRARY ISOTOPES ')
     CALL get_rlib_isotopes(pcom)
  CASE ('GET RATE LIST ')
     CALL get_rate_list(pcom)
  CASE ('GET RATE INFO ')
     CALL get_rate_info(pcom)
  CASE ('MODIFY RATES ')
     CALL modify_rates(pcom)
  CASE ('MODIFY RATE LIBRARY ')
     CALL modify_library(pcom)
  CASE ('RATES EXIST ')
     CALL rates_exist(pcom)
  CASE ('REGISTER ')
     CALL register(pcom)
  CASE ('ADD MISSING INV RATES ')
     CALL add_missing_inv(pcom)
  CASE ('SHARE RATE LIBRARY ')
     CALL share_rlib(pcom)
  CASE ('EXPORT RATE LIBRARY ')
     CALL export_rlib_cgi(pcom)
  CASE ('GET NUC DATA SET LIST')
     CALL get_nuc_data_set_list(pcom)
  CASE ('GET NUC DATA SET INFO')
     WRITE(*,'(A)') 'REPORT=Inverse rates will be added when this feature is complete.'
  CASE ('GET NUC DATA SET ISOTOPES')
     CALL get_nuc_data_set_isotopes(pcom)
  CASE ('GET NUC DATA LIST')
     CALL get_nuc_data_list(pcom)
  CASE ('GET NUC DATA INFO')
     CALL get_nuc_data_info(pcom)
  CASE ('NUC DATA EXIST')
     CALL nuc_data_exist(pcom)
  CASE ('MODIFY NUC DATA SET')
     CALL modify_nuc_data_set(pcom)
  CASE ('MODIFY NUC DATA')
     CALL modify_nuc_data(pcom)
  CASE ('SHARE NUC DATA SET')
     CALL share_NDS(pcom)
  CASE ('PARSE NUC DATA FILE')
     CALL parse_nuc_data_file(pcom)
  CASE ('ELEMENT SYNTHESIS SETUP')
     CALL em_syn_setup(pcom)
  CASE ('SYNTHESIZE ELEMENTS')
     CALL em_syn_start(pcom)
  CASE ('ELEMENT SYNTHESIS UPDATE')
     CALL em_syn_update(pcom)
  CASE ('ELEMENT SYNTHESIS OUTPUT')
     CALL em_syn_output(pcom)
  CASE ('ABORT ELEMENT SYNTHESIS')
     CALL abort_em_syn(pcom)
  CASE ('GET ELEMENT SYNTHESIS TIME MAPPING')
     CALL get_em_timemap(pcom)
  CASE ('GET ELEMENT SYNTHESIS ISOTOPE MAPPING')
     CALL get_em_isomap(pcom)
  CASE ('GET ELEMENT SYNTHESIS FLUX MAPPING')
     CALL get_em_reacmap(pcom)
  CASE ('GET ELEMENT SYNTHESIS THERMO PROFILE')
     CALL get_em_thermo(pcom)
  CASE ('GET ELEMENT SYNTHESIS ABUNDANCES')
     CALL get_em_abund(pcom)
  CASE ('GET ELEMENT SYNTHESIS FLUXES')
     CALL get_em_flux(pcom)
  CASE ('MAKE ELEMENT SYNTHESIS MOVIE')
     CALL make_es_movie(pcom)
  CASE ('GET ELEMENT SYNTHESIS WEIGHTED ABUNDANCES')
     CALL get_em_weighted_abund(pcom)
  CASE DEFAULT
     CALL report_error('Invalid action: '//TRIM(pcom%ACTION),'Improper usage',pcom,1)
  END SELECT

END PROGRAM cina

!---------------------------------------------------------------------
FUNCTION cina_ver
  IMPLICIT NONE
  CHARACTER(LEN=20),PARAMETER     :: CINA_VERSION = '$Revision: 1.2 $'
  CHARACTER(LEN=10)               :: cina_ver

  cina_ver = CINA_VERSION(12:LEN_TRIM(CINA_VERSION)-2)

END FUNCTION cina_ver

!---------------------------------------------------------------------
SUBROUTINE print_cina_ver(PROGRAM_VER)
  USE convert
  USE cina_core
  USE reactionstrings
  USE io
  USE fileio
  USE options
  USE math
  USE reaction
  USE rategen
  USE rate_man
  USE rate_man_core
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)      :: PROGRAM_VER
  CHARACTER(LEN=10)                :: cina_ver
  EXTERNAL                         :: cina_ver

  WRITE(*,'(3A)') TRIM(CINA_NAME),' version ',PROGRAM_VER
  WRITE(*,'( A)') 'cina dependencies:'
  WRITE(*,'(T5,2A)') 'cina.f90 version ',cina_ver()
  WRITE(*,'(T5,2A)') 'convert.f90 version ',convert_ver()
  WRITE(*,'(T5,2A)') 'rate_man.f90 version ',rate_man_ver()
  WRITE(*,'(T5,2A)') 'rate_man_core.f90 version ',rate_man_core_ver()
  WRITE(*,'(T5,2A)') 'fileio.f90 version ',fileio_ver()
  WRITE(*,'(T5,2A)') 'io.f90 version ',io_ver()
  WRITE(*,'(T5,2A)') 'math.f90 version ',math_ver()
  WRITE(*,'(T5,2A)') 'options.f90 version ',options_ver()
  WRITE(*,'(T5,2A)') 'cina_core.f90 version ',cina_core_ver()
  WRITE(*,'(T5,2A)') 'rategen.f90 version ',rategen_ver()
  WRITE(*,'(T5,2A)') 'reaction.f90 version ',reaction_ver()
  WRITE(*,'(T5,2A)') 'reactionstrings.f90 version ',reactionstrings_ver()
  STOP

END SUBROUTINE print_cina_ver

!---------------------------------------------------------------------
SUBROUTINE print_cina_help(PROGRAM_VER)
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)    :: PROGRAM_VER

  WRITE(*,'(3A)') 'cina ',PROGRAM_VER,' Help'
  WRITE(*,'(A)') 'This program receives CGI requests from the internet,'
  WRITE(*,'(A)') 'executes the appropriate fortran program, and returns'
  WRITE(*,'(A)') 'the results.'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Usage:'
  WRITE(*,'(A)') '  cina [-h | -v]'
  WRITE(*,'(A)') ''
  STOP
END SUBROUTINE print_cina_help

!---------------------------------------------------------------------
