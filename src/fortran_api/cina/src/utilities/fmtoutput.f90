!Moved from the original directory and added the CVS info
!
!   $Author: bucknerk $
!   $Id: fmtoutput.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
!   $Log: fmtoutput.f90,v $
!   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
!   all in one place
!
!   Revision 1.2  2007/07/17 19:12:40  bucknerk
!   Don't need this any more.
!
!   
PROGRAM fmtoutput
  !PURPOSE = Execute CGI requests and return results to webserver
  !STATUS = Complete and tested
  !DESC = 
  ! This program was originally written by Chase Hard during the summer of 2003.
  ! Jason Scott converted it to Fortran 90 and integrated it with the suite.
  USE io
  USE options
  USE parm_output
  USE fileio
  USE IFLPORT
  IMPLICIT NONE
  CHARACTER(LEN=9),PARAMETER       :: PROGRAM_VER = '0.1'
  TYPE(generaloptions)             :: gen_opt
  TYPE(fmtoutoptions)              :: opt
  TYPE(fileiooptions)              :: fopt
  REAL(KIND=8)                     :: array(999,4)
  INTEGER(KIND=4)                  :: i
  CHARACTER(LEN=80)                :: tmps

  ! Get all the command line arguments, parse them, and set the options variables
  CALL get_fmtoutput_opt(gen_opt,opt)
  IF (gen_opt%help) CALL print_fmtout_help(PROGRAM_VER)
  IF (gen_opt%version) CALL print_fmtout_ver(PROGRAM_VER)

  ! Make sure a format was specified
  IF (opt%formats == '') THEN
     WRITE(*,'(A)') 'Please select a format to generate.'
     STOP
  END IF

  ! Read in parameters from file
  CALL read_parm(opt%a_num,opt%a,opt%parm_file)

  ! Read in inverse parameters if a filename was specified on the command line
  IF (LEN_TRIM(opt%inv_file) > 0) CALL read_parm(opt%a_inv_num,opt%a_inv,opt%inv_file)

  ! Read in inputread file if specified on the command line
  IF (LEN_TRIM(opt%iread_file) > 0) THEN
     fopt%data_format = 'INPUTREAD'
     fopt%file_in = opt%iread_file
     CALL file2array(array,i,fopt)
     opt%r = fopt%r   ! Copy reactionparticles data to opt
  END IF

  ! Open output file
  OPEN(20,FILE=opt%out_file,IOSTAT=i,ACTION='WRITE')
  IF (i /= 0) THEN
     WRITE(tmps,'(3A,I0)') 'Can not open output file ',TRIM(opt%out_file),'  ERROR ',i
     CALL printerror(tmps,1)
  END IF

  ! CALL gen_parm_fmt as long as a format string is in opt%formats
  DO WHILE (LEN_TRIM(opt%formats) > 0)
     CALL gen_parm_fmt(opt)
  END DO

  CLOSE(20)
END PROGRAM fmtoutput

!---------------------------------------------------------------------
FUNCTION fmtoutput_ver
  !PURPOSE = Return the CVS revision number of this file
  !STATUS = Complete and tested
  IMPLICIT NONE
  CHARACTER(LEN=20),PARAMETER     :: FMTOUT_VERSION = '$Revision: 1.1.1.1 $'
  CHARACTER(LEN=10)               :: fmtoutput_ver

  fmtoutput_ver = FMTOUT_VERSION(12:LEN_TRIM(FMTOUT_VERSION)-2)

END FUNCTION fmtoutput_ver

!---------------------------------------------------------------------
SUBROUTINE print_fmtout_ver(PROGRAM_VER)
  !PURPOSE = Print the fmtoutput version information
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  USE convert
  USE reactionstrings
  USE io
  USE fileio
  USE options
  USE math
  USE reaction
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)      :: PROGRAM_VER
  CHARACTER(LEN=10)                :: fmtoutput_ver
  EXTERNAL                         :: fmtoutput_ver

  WRITE(*,'(2A)') 'fmtoutput version ',PROGRAM_VER
  WRITE(*,'( A)') 'fmtoutput dependencies:'
  WRITE(*,'(T5,2A)') 'fmtoutput.f90 version ',fmtoutput_ver()
  WRITE(*,'(T5,2A)') 'convert.f90 version ',convert_ver()
  WRITE(*,'(T5,2A)') 'fileio.f90 version ',fileio_ver()
  WRITE(*,'(T5,2A)') 'io.f90 version ',io_ver()
  WRITE(*,'(T5,2A)') 'math.f90 version ',math_ver()
  WRITE(*,'(T5,2A)') 'options.f90 version ',options_ver()
  WRITE(*,'(T5,2A)') 'reaction.f90 version ',reaction_ver()
  WRITE(*,'(T5,2A)') 'reactionstrings.f90 version ',reactionstrings_ver()
  STOP

END SUBROUTINE print_fmtout_ver

!---------------------------------------------------------------------
SUBROUTINE print_fmtout_help(PROGRAM_VER)
  !PURPOSE = Print help screen for fmtoutput
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)    :: PROGRAM_VER

  WRITE(*,'(3A)') 'fmtoutput ',PROGRAM_VER,' Help'
  WRITE(*,'(A)') 'This programs formats parameters into various formats.'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Usage:'
  WRITE(*,'(A)') '  fmtoutput [-h | -v]'
  WRITE(*,'(A)') '  fmtoutput formats output_file parm_file [option optionvalue] ...'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Description:'
  WRITE(*,'(A)') '  formats is one or more of the following:'
  WRITE(*,'(A)') '    ASCII,HTML,FORTRAN,NETSU,FULL_NETSU,REACLIB'
  WRITE(*,'(A)') '    formats may be a comma separated list such as "LIST,REACLIB,HTML"'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') '  parm_file is the parameterizer output file with the parameters to format'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Options   Option Values   Description'
  WRITE(*,'(A)') ' -inv     filename        Filename with inverse parameters'
  WRITE(*,'(A)') ' -ir      filename        Inputread file, ends in .inrd'
  WRITE(*,'(A)') ' -b       string          Bibliographic identifier'
  WRITE(*,'(A)') ' -q       number          Q Value for the reaction in inputread file'
  WRITE(*,'(A)') ' -ec      "yes" or "no"   Reaction type is electron capture decay'
  WRITE(*,'(A)') ' -desc    string          Descriptive or longer bibliographic string'
  WRITE(*,'(A)') ''
  STOP
END SUBROUTINE print_fmtout_help

!---------------------------------------------------------------------
