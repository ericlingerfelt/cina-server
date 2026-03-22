!Moved from the original directory and added the CVS info
!
!   $Author: bucknerk $
!   $Id: inputread.f90,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
!   $Log: inputread.f90,v $
!   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
!   all in one place
!
!   Revision 1.1  2007/07/17 19:10:07  bucknerk
!   Moved here
!
!   
PROGRAM inputread
  !! User documentation
  !!****p Astrodata-Suite/inputread
  !! VERSION
  !!   inputread 0.1
  !! PURPOSE
  !!   inputread is a fortran program for reading in users' input files and obtaining 
  !!   all the information needed for preprocessing.  This information includes the 
  !!   units, reaction, and any personal notes associated with the input file.
  !! STATUS
  !!   Complete and stable
  !! REQUIREMENTS
  !!   NucastroFLib 0.1 or above (nucastrodata Fortran Library)
  !! USAGE
  !!   inputread [-h | -v]
  !!   inputread input_file data_type [-h | -v] [option optionvalue ...]
  !!   
  !!   Available data types: S(E),CS(E),R(T)
  !!   Available read data formats: LOOSE_COLUMN,INPUTREAD,NARROW_RESONANCE,INPUTCHECK,RATEGEN
  !! COMMAND LINE OPTIONS
  !!   Options are not case sensitive and may not be required.
  !!    -v or --version prints the program version
  !!   
  !!   Options  Option Values    Description
  !!    -o      output_file      Pathname of output file
  !!    -eu     ev,kev,mev,gev   Energy units used in the input file
  !!    -su     ev-b,kev-b       S-factor units used in the input file
  !!            mev-b,gev-b
  !!    -csu    b,kb,mb,gb       Cross-section units used in the input file
  !!    -tu     T0,T3,T6,T9      Temperature units (K) used in the input file
  !!    -f      (see above list) Format of the input file
  !!    -fo     depends on fmt   Options for a file format
  !!    -p      0,1,2,3,4        Level of screen output.  0 is none
  !!    -r      6Li(p,a)3He      Reaction in the data files
  !!            p + Li6 -> a + 3He    (spaces are optional)
  !!            1,1 + 3,6 -> 2,4 + 2,3
  !!    -rt     ec,bet+,bet-     Reaction type, required for decays
  !!    -n      "Personal notes" Notes about the file's contents
  !! DESCRIPTION
  !!   A help message is displayed if no command line options are included when the program is executed.
  !!   
  !! INPUTS
  !!   
  !! OUTPUTS
  !!   
  !! EXAMPLE
  !!   
  !! NOTES
  !!   
  !! BUGS
  !!   
  !! TODO
  !!   
  !! SEE ALSO
  !!   
  !! AUTHOR
  !!   Jason P. Scott (jpscott@mail.phy.ornl.gov)
  !! CREATION DATE
  !!   
  !! MODIFICATION HISTORY
  !!   
  !! COPYRIGHT
  !!   
  !!***
  !
  ! Programmers' documentation
  !****p main/cina
  ! VERSION
  !   $Revision: 1.1.1.1 $
  ! PURPOSE
  !   
  ! STATUS
  !   
  ! CAUTION
  !   
  ! USES
  !   
  ! USAGE
  !   
  ! INPUTS
  !   
  ! OUTPUTS
  !   
  ! RETURN VALUE
  !   
  ! EXAMPLE
  !   
  ! NOTES
  !   
  ! BUGS
  !   
  ! TODO
  !   
  ! SEE ALSO
  !   
  ! AUTHOR
  !   Jason P. Scott (jpscott@mail.phy.ornl.gov)
  ! CREATION DATE
  !   
  ! MODIFICATION HISTORY
  !   
  ! COPYRIGHT
  !   
  ! SOURCE

  !***

  !PURPOSE = Parse input files of various types and formats and create files needed for other programs
  !STATUS = Complete and tested
  !STATUS = Supports data types: S(E),CS(E),R(T)
  ! Make procedures and variables in the convert and fileio module available for use
  USE io
  USE convert
  USE fileio
  USE options
  USE reactionstrings

  IMPLICIT NONE
  INTEGER(KIND=2),PARAMETER        :: array_len = 10000
  REAL(KIND=8),PARAMETER           :: e_units_out = 1D3        ! KeV
  REAL(KIND=8),PARAMETER           :: t_units_out = 1D9        ! T9 
  REAL(KIND=8),PARAMETER           :: s_units_out = 1D3        ! KeV-b
  REAL(KIND=8),PARAMETER           :: cs_units_out = 1D0       ! b
  REAL(KIND=8),PARAMETER           :: r_units_out = 1D0        ! depends on reaction, will update later
  CHARACTER(LEN=9),PARAMETER       :: PROGRAM_VER = '0.1'
  CHARACTER(LEN=70)                :: s,mes
  INTEGER(KIND=4)                  :: points
  REAL(KIND=8)                     :: array(array_len,4),tmpr
  TYPE(fileiooptions)              :: opt
  TYPE(generaloptions)             :: gen_opt

  ! Get all the command line arguments, parse them, and set the options variable
  ! This function returns unless an error occurs
  CALL get_inputread_opt(opt,gen_opt)
  IF (gen_opt%help) CALL print_inputread_help(PROGRAM_VER)
  IF (gen_opt%version) CALL print_inputread_ver(PROGRAM_VER)

  opt%e_units_out = e_units_out
  opt%t_units_out = t_units_out
  opt%s_units_out = s_units_out
  opt%cs_units_out = cs_units_out
  opt%r_units_out = r_units_out

  ! If reaction was not specified on the command line, ask for it
  IF (getreactype(opt%r) < 1) THEN
     WRITE(*,'(A)') ' '
     WRITE(*,'(A)') 'Type in the reaction for this file:'
     WRITE(*,'(A)') 'Examples: "6Li(p,a)3He" "p + Li6 -> a + 3He" "1,1 + 3,6 -> 2,4 + 2,3"'
     READ(*,'(A)') s
     CALL read_reac_str(opt%r,s,mes)
     IF (LEN_TRIM(mes) > 0) THEN
        WRITE(*,'(A)') TRIM(mes)
        CALL printerror('Could not read in reaction string',1)
     END IF
  END IF

  ! If notes were not speficied on the command line, ask for them
  IF (LEN_TRIM(opt%notes) == 0) THEN
     WRITE(*,'(A)') ' '
     WRITE(*,'(A)') 'Enter any notes for this reaction:'
     READ(*,'(A)') opt%notes
  END IF

  ! Start fileio module.  Give it options and it returns the array and number of points
  ! Note that the units of array are the same as the file
  ! They are not converted to the output units until written to a file
  CALL file2array(array,points,opt)

  ! Exit if no points were found
  IF (points <= 0) THEN
     WRITE(*,'(A)') 'Make sure the format options are correct.'
     CALL printerror('No points were found in input file.',1)
  END IF

  ! Print array summary to the screen
  IF (opt%plevel >= 2) THEN
     s = getreac_str(opt%r,0)
     WRITE(*,'(2A)') 'Reaction: ',TRIM(s)
     WRITE(*,'(2A)') 'Notes: ',TRIM(opt%notes)
     WRITE(*,'(A,I0)') 'Number of points: ',points
     dtype: SELECT CASE (TRIM(lowercase(opt%data_type)))
     CASE ('s(e) ') dtype
        tmpr = MINVAL(array(1:points,1)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy min:',tmpr,' ',get_e_units(opt%e_units_out)
        tmpr = MAXVAL(array(1:points,1)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy max:',tmpr,' ',get_e_units(opt%e_units_out)

        tmpr = MINVAL(array(1:points,2)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy error min:',tmpr,' ',get_e_units(opt%e_units_out)
        tmpr = MAXVAL(array(1:points,2)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy error max:',tmpr,' ',get_e_units(opt%e_units_out)

        tmpr = MINVAL(array(1:points,3)) * opt%s_units_in / opt%s_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'S-factor min:',tmpr,' ',get_s_units(opt%s_units_out)
        tmpr = MAXVAL(array(1:points,3)) * opt%s_units_in / opt%s_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'S-factor max:',tmpr,' ',get_s_units(opt%s_units_out)

        tmpr = MINVAL(array(1:points,4)) * opt%s_units_in / opt%s_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'S-factor error min:',tmpr,' ',get_s_units(opt%s_units_out)
        tmpr = MAXVAL(array(1:points,4)) * opt%s_units_in / opt%s_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'S-factor error max:',tmpr,' ',get_s_units(opt%s_units_out)
     CASE ('cs(e) ') dtype
        tmpr = MINVAL(array(1:points,1)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy min:',tmpr,' ',get_e_units(opt%e_units_out)
        tmpr = MAXVAL(array(1:points,1)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy max:',tmpr,' ',get_e_units(opt%e_units_out)

        tmpr = MINVAL(array(1:points,2)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy error min:',tmpr,' ',get_e_units(opt%e_units_out)
        tmpr = MAXVAL(array(1:points,2)) * opt%e_units_in / opt%e_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Energy error max:',tmpr,' ',get_e_units(opt%e_units_out)

        tmpr = MINVAL(array(1:points,3)) * opt%cs_units_in / opt%cs_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Cross section min:',tmpr,' ',get_cs_units(opt%cs_units_out)
        tmpr = MAXVAL(array(1:points,3)) * opt%cs_units_in / opt%cs_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Cross section max:',tmpr,' ',get_cs_units(opt%cs_units_out)

        tmpr = MINVAL(array(1:points,4)) * opt%cs_units_in / opt%cs_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Cross section error min:',tmpr,' ',get_cs_units(opt%cs_units_out)
        tmpr = MAXVAL(array(1:points,4)) * opt%cs_units_in / opt%cs_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Cross section error max :',tmpr,' ',get_cs_units(opt%cs_units_out)
     CASE ('r(t) ') dtype
        tmpr = MINVAL(array(1:points,1)) * opt%t_units_in / opt%t_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Temperature min:  ',tmpr,' ',get_t_units(opt%t_units_out)
        tmpr = MAXVAL(array(1:points,1)) * opt%t_units_in / opt%t_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Temperature max:  ',tmpr,' ',get_t_units(opt%t_units_out)

        tmpr = MINVAL(array(1:points,2)) * opt%t_units_in / opt%t_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Temperature error min:  ',tmpr,' ',get_t_units(opt%t_units_out)
        tmpr = MAXVAL(array(1:points,2)) * opt%t_units_in / opt%t_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Temperature error max:  ',tmpr,' ',get_t_units(opt%t_units_out)

        tmpr = MINVAL(array(1:points,3)) * opt%r_units_in / opt%r_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Rate min: ',tmpr,' ',get_r_units(opt%r_units_out)
        tmpr = MAXVAL(array(1:points,3)) * opt%r_units_in / opt%r_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Rate max: ',tmpr,' ',get_r_units(opt%r_units_out)

        tmpr = MINVAL(array(1:points,4)) * opt%r_units_in / opt%r_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Rate error min: ',tmpr,' ',get_r_units(opt%r_units_out)
        tmpr = MAXVAL(array(1:points,4)) * opt%r_units_in / opt%r_units_out
        WRITE(*,'(A,T27,G12.6,A,A)') 'Rate error max: ',tmpr,' ',get_r_units(opt%r_units_out)
     END SELECT dtype
  END IF

  ! Use fileio module to save file in a different format
  ! But a new format must be specified
  ! Note that using this data format will automatically sort the array from lowest to highest
  opt%data_format = 'INPUTREAD'
  ! Set fmt_options to 'UNKNOWN' to overwrite output files
  ! Set fmt_options to 'NEW' to not overwrite output files
  opt%fmt_options = 'UNKNOWN'
  CALL array2file(array,points,opt)

END PROGRAM inputread

!---------------------------------------------------------------------
FUNCTION inputread_ver
  !PURPOSE = Return the CVS revision number of this file
  !STATUS = Complete and tested
  IMPLICIT NONE
  CHARACTER(LEN=20),PARAMETER     :: INREAD_VERSION = '$Revision: 1.1.1.1 $'
  CHARACTER(LEN=10)               :: inputread_ver

  inputread_ver = INREAD_VERSION(12:LEN_TRIM(INREAD_VERSION)-2)

END FUNCTION inputread_ver

!---------------------------------------------------------------------
SUBROUTINE print_inputread_ver(PROGRAM_VER)
  !PURPOSE = Print the inputread version information
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  USE fileio
  USE io
  USE convert
  USE options
  USE reactionstrings
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)      :: PROGRAM_VER
  CHARACTER(LEN=10)                :: inputread_ver
  EXTERNAL                         :: inputread_ver

  WRITE(*,'(2A)') 'inputread version ',PROGRAM_VER
  WRITE(*,'( A)') 'inputread dependencies:'
  WRITE(*,'(T5,2A)') 'inputread.f90 version ',inputread_ver()
  WRITE(*,'(T5,2A)') 'io.f90 version ',io_ver()
  WRITE(*,'(T5,2A)') 'fileio.f90 version ',fileio_ver()
  WRITE(*,'(T5,2A)') 'convert.f90 version ',convert_ver()
  WRITE(*,'(T5,2A)') 'options.f90 version ',options_ver()
  WRITE(*,'(T5,2A)') 'reactionstrings.f90 version ',reactionstrings_ver()
  STOP

END SUBROUTINE print_inputread_ver

!---------------------------------------------------------------------
SUBROUTINE print_inputread_help(PROGRAM_VER)
  !PURPOSE = Print help screen for inputread
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  USE fileio
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)    :: PROGRAM_VER

  WRITE(*,'(3A)') 'inputread ',PROGRAM_VER,' Help'
  WRITE(*,'(A)') 'This program extracts information from input files, scales it'
  WRITE(*,'(A)') 'appropriately, and creates a file needed for inputcheck'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Usage:'
  WRITE(*,'(A)') '  inputread [-h | -v]'
  WRITE(*,'(A)') '  inputread input_file data_type [-h | -v] [option optionvalue ...] '
  WRITE(*,'(A)') ''
  CALL fprintdatatypes
  CALL fprintRdataformats
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Options are not case sensitive and may not be required.'
  WRITE(*,'(A)') '  -v or --version prints the program version'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Options  Option Values    Description'
  WRITE(*,'(A)') ' -o      output_file      Pathname of output file'
  WRITE(*,'(A)') ' -eu     ev,kev,mev,gev   Energy units used in the input file'
  WRITE(*,'(A)') ' -su     ev-b,kev-b       S-factor units used in the input file'
  WRITE(*,'(A)') '         mev-b,gev-b'
  WRITE(*,'(A)') ' -csu    b,mb,ub,nb,pb    Cross-section units used in the input file'
  WRITE(*,'(A)') ' -tu     T0,T3,T6,T9      Temperature units (K) used in the input file'
  WRITE(*,'(A)') ' -f      COLUMN           Format of the input file'
  WRITE(*,'(A)') ' -fo     depends on fmt   Options for a file format'
  WRITE(*,'(A)') ' -p      0,1,2,3,4        Level of screen output.  0 is none'
  WRITE(*,'(A)') ' -r      6Li(p,a)3He      Reaction in the data files'
  WRITE(*,'(A)') '         p + Li6 -> a + 3He    (spaces are optional)'
  WRITE(*,'(A)') '         1,1 + 3,6 -> 2,4 + 2,3'
  WRITE(*,'(A)') ' -rt     ec,bet+,bet-     Reaction type, required for decays'
  WRITE(*,'(A)') ' -n      "Personal notes" Notes about the file''s contents'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'If the format is "LOOSE_COLUMN" then you may use the -fo option'
  WRITE(*,'(A)') 'to specify which columns are energy,energy error, etc.'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'See the inputread.txt file for more information.'
  STOP
END SUBROUTINE print_inputread_help
