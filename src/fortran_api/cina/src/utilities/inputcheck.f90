!Moved from the original directory and added the CVS info
!
!   $Author: bucknerk $
!   $Id: inputcheck.f90,v 1.1.1.1 2008/04/22 13:36:33 bucknerk Exp $
!   $Log: inputcheck.f90,v $
!   Revision 1.1.1.1  2008/04/22 13:36:33  bucknerk
!   all in one place
!
!   Revision 1.2  2007/09/17 16:28:13  bucknerk
!   Changed the size of the input array from 10000 to 5000 (and could change it
!   to an even smaller value) and changed the return value array 'tmpra' from
!   size 10000 to dynamically allocated based on the number of points+1.  This
!   keeps the seg fault from occurring when the "numderiv" function returns.
!
!   Revision 1.1  2007/07/17 19:10:07  bucknerk
!   Moved here
!
!   
PROGRAM inputcheck
  !PURPOSE = Perform sanity check on input and search for narrow resonances
  !STATUS = Complete and tested
  !STATUS = Supports data types: S(E),CS(E),R(T)
  USE fileio
  USE options
  USE convert
  USE io
  USE math
  USE reaction
  USE reactionstrings
  IMPLICIT NONE
  INTEGER(KIND=2),PARAMETER        :: array_len = 5000
  CHARACTER(LEN=9),PARAMETER       :: PROGRAM_VER = '0.1'
  REAL(KIND=8),PARAMETER           :: e_min = 1D3   ! 1 KeV
  REAL(KIND=8),PARAMETER           :: e_max = 20D6  ! 4 MeV
  REAL(KIND=8),PARAMETER           :: s_min = 1D-100
  REAL(KIND=8),PARAMETER           :: s_max = 1D10
  REAL(KIND=8),PARAMETER           :: cs_min = 1D-100
  REAL(KIND=8),PARAMETER           :: cs_max = 1D10
  REAL(KIND=8),PARAMETER           :: t_min = 1D6   ! 0.001 T9
  REAL(KIND=8),PARAMETER           :: t_max = 10D9  ! 10 T9
  REAL(KIND=8),PARAMETER           :: r_min = 1D-100
  REAL(KIND=8),PARAMETER           :: r_max = 1D10
  REAL(KIND=8),PARAMETER           :: deriv_threshold = 1D6
  REAL(KIND=8),PARAMETER           :: nlimit = 0.2 ! Max Ratio of FWHM/Centroid to be considered a narrow resonance
  INTEGER(KIND=4)                  :: points,col_num,i,j,n,res_cur ! cursor into result array
  INTEGER(KIND=4)                  :: x_col,xe_col,y_col,ye_col,chk_col
  REAL(KIND=8)                     :: array(array_len,4),tmpr,xf,yf,scale
  REAL(KIND=8)                     :: chk_min,chk_max
  REAL, ALLOCATABLE                :: tmpra(:)
  CHARACTER(LEN=20)                :: x_unit,y_unit,check_unit
  CHARACTER(LEN=70)                :: check,s
  LOGICAL                          :: chk
  LOGICAL                          :: chk_e,chk_s,chk_cs,chk_r,chk_t
  TYPE(fileiooptions)              :: opt
  TYPE(incheckoptions)             :: ichk_opt
  TYPE(inputcheckresult)           :: result(10)
  TYPE(narrowresonance)            :: nres(50)       ! Can hold up to 20 narrow resonances

  ! Get all the command line arguments, parse them, and set the options variable
  ! This function returns unless displaying help, version, or an error occurs
  CALL get_inputcheck_opt(opt,ichk_opt)
  IF (ichk_opt%help) CALL print_inputcheck_help(PROGRAM_VER)
  IF (ichk_opt%version) CALL print_inputcheck_ver(PROGRAM_VER)

  ! Start fileio module.  Give it options and it returns the array and number of points
  ! It also returns the units and other useful info in opt structure.
  opt%data_format = 'INPUTREAD'
  CALL file2array(array,points,opt)

  ! Set output units to input units
  opt%e_units_out = opt%e_units_in
  opt%s_units_out = opt%s_units_in
  opt%cs_units_out = opt%cs_units_in
  opt%t_units_out = opt%t_units_in
  opt%r_units_out = opt%r_units_in

  chk_e = .FALSE.
  chk_t = .FALSE.
  chk_s = .FALSE.
  chk_cs = .FALSE.
  chk_r = .FALSE.
  res_cur = 1

  ! Get general information about the data type
  SELECT CASE (lowercase(opt%data_type))
  CASE ('s(e) ')
     col_num = 4                                  ! Total number of columns in array
     chk_e = .TRUE.
     chk_s = .TRUE.

     x_col = 1                                    ! Array column with dependent value
     xe_col = 2                                   ! Array column with dependent value error
     x_unit = get_e_units(opt%e_units_in)         ! Units of dependent values
     xf = opt%e_units_in                          ! Factor to multiply to get standard units

     y_col = 3                                    ! Array column with independent value
     ye_col = 4                                   ! Array column with independent value error
     y_unit = get_s_units(opt%s_units_in)         ! Units of independent values
     yf = opt%s_units_in                          ! Factor to multiply to get standard units
  CASE ('cs(e) ')
     col_num = 4                                  ! Total number of columns in array
     chk_e = .TRUE.
     chk_cs = .TRUE.

     x_col = 1                                    ! Array column with dependent value
     xe_col = 2                                   ! Array column with dependent value error
     x_unit = get_e_units(opt%e_units_in)         ! Units of dependent values
     xf = opt%e_units_in                          ! Factor to multiply to get standard units

     y_col = 3                                    ! Array column with independent value
     ye_col = 4                                   ! Array column with independent value error
     y_unit = get_cs_units(opt%cs_units_in)       ! Units of independent values
     yf = opt%cs_units_in                         ! Factor to multiply to get standard units
  CASE ('r(t) ')
     col_num = 4                                  ! Total number of columns in array
     chk_t = .TRUE.
     chk_r = .TRUE.

     x_col = 1                                    ! Array column with dependent value
     xe_col = 2                                   ! Array column with dependent value error
     x_unit = get_t_units(opt%t_units_in)         ! Units of dependent values
     xf = opt%t_units_in                          ! Factor to multiply to get standard units

     y_col = 3                                    ! Array column with independent value
     ye_col = 4                                   ! Array column with independent value error
     y_unit = get_r_units(opt%r_units_in)         ! Units of independent values
     yf = opt%r_units_in                          ! Factor to multiply to get standard units
  CASE DEFAULT 
     CALL printerror('Unknown data type in inputcheck',1)
  END SELECT

  s = getreac_str(opt%r,0)
  WRITE(*,'(2A)') 'Reaction: ',TRIM(s)
  WRITE(*,'(2A)') 'Notes: ',TRIM(line2str(opt%notes))
  WRITE(*,'(I0,A)') points,' points were found'

  ! Perform POSITIVE check
  result(res_cur)%result = 'SKIPPED'
  result(res_cur)%reason = ''                    ! Reason for a failed rseult
  result(res_cur)%test = 'Positive'
  IF (ichk_opt%positivechk) THEN
     tmpr = MINVAL(array(1:points,1:col_num))
     IF (tmpr >= 0D0) THEN
        result(res_cur)%result = 'PASSED'
     ELSE
        result(res_cur)%result = 'FAILED'
        WRITE(result(res_cur)%reason,'(G13.5)',IOSTAT=n) tmpr
     END IF
  END IF
  WRITE(*,'(A,T25,A,T35,A)') 'Positive check:',                        &
       result(res_cur)%result,TRIM(result(res_cur)%reason)
  res_cur = res_cur + 1

  ! Perform SINGLE VALUED check on the FIRST COLUMN of the array
  result(res_cur)%result = 'SKIPPED'
  result(res_cur)%reason = ''
  result(res_cur)%test = 'Single valued'
  IF (ichk_opt%singlevaluedchk) THEN
     result(res_cur)%result = 'PASSED'
     DO i = 1,points
        DO j = 1,points
           IF ((i /= j) .AND. (array(i,x_col) == array(j,x_col))) THEN
              result(res_cur)%result = 'FAILED'
              WRITE(result(res_cur)%reason,'(G13.5,2A)',IOSTAT=n) array(i,x_col),' ',TRIM(x_unit)
           END IF
        END DO
     END DO
  END IF
  WRITE(*,'(A,T25,A,T35,A)') 'Single valued check:',                   &
       result(res_cur)%result,TRIM(result(res_cur)%reason)
  res_cur = res_cur + 1

  ! Perform REASONABLE RANGE checks
  result(res_cur)%result = 'SKIPPED'
  WRITE(*,'(A)') 'Range checks:'
  IF (ichk_opt%rangechk) THEN
     DO i = 1,5
        SELECT CASE (i)
        CASE (1) ! energy check
           chk = chk_e
           chk_col = x_col
           chk_min = e_min
           chk_max = e_max
           check_unit = x_unit
           check = 'Energy'
           scale = xf
        CASE (2) ! temperature check
           chk = chk_t
           chk_col = x_col
           chk_min = t_min
           chk_max = t_max
           check_unit = x_unit
           check = 'Temperature'
           scale = xf
        CASE (3) ! s-factor check
           chk = chk_s
           chk_col = y_col
           chk_min = s_min
           chk_max = s_max
           check_unit = y_unit
           check = 'S-factor'
           scale = yf
        CASE (4) ! cross section check
           chk = chk_cs
           chk_col = y_col
           chk_min = cs_min
           chk_max = cs_max
           check_unit = y_unit
           check = 'Cross section'
           scale = yf
        CASE (5) ! rate check
           chk = chk_r
           chk_col = y_col
           chk_min = r_min
           chk_max = r_max
           check_unit = y_unit
           check = 'Rate'
           scale = yf
        CASE DEFAULT
           CALL printerror('Unknown case in loop for range check',1)
        END SELECT
        IF (chk) THEN
           result(res_cur)%reason = ''
           result(res_cur)%result = 'PASSED'
           result(res_cur)%test = TRIM(check) // ' range'
           ! Find the minimum value of the whole column
           tmpr = MINVAL(array(1:points,chk_col)) * scale
           IF (tmpr < chk_min) THEN
              result(res_cur)%result = 'FAILED'
              WRITE(result(res_cur)%reason,'(G13.5,3A)',IOSTAT=n)      &
                   tmpr / scale,' ',TRIM(check_unit),' is too low'
           END IF

           ! Find the maximum value of the whole array
           tmpr = MAXVAL(array(1:points,chk_col)) * scale
           IF (tmpr > chk_max) THEN
              result(res_cur)%result = 'FAILED'
              WRITE(result(res_cur)%reason,'(G13.5,3A)',IOSTAT=n)      &
                   tmpr / scale,' ',TRIM(check_unit),' is too high'
           END IF
           WRITE(*,'(T2,2A,T25,A,T35,A)') TRIM(check),' check:',       &
                result(res_cur)%result,TRIM(result(res_cur)%reason)
           res_cur = res_cur + 1
        END IF
     END DO
  END IF

  ! Perform CONTINUITY check
  result(res_cur)%result = 'SKIPPED'
  result(res_cur)%reason = ''
  result(res_cur)%test = 'Continuity'
  IF (ichk_opt%continuouschk) THEN
     result(res_cur)%result = 'PASSED'
     ! tmpra holds derivative. 
     ! xf and yf are used to get to standard units
     ALLOCATE(tmpra(1:points+1),STAT=i)
     DO i = 1,points+1
       tmpra(i)= 0.0
     END DO 
     tmpra = numderiv(xf * array(1:points,1), yf * array(1:points,3))
     DO i = 1, points - 1
        IF (ABS(tmpra(i)) > deriv_threshold) THEN
           WRITE(result(res_cur)%reason,'(G13.5,A,G13.5,2A)',IOSTAT=n) &
                array(i,x_col),', ',array(i+1,x_col),' ',TRIM(x_unit)
           result(res_cur)%result = 'FAILED'
        END IF
     END DO
  END IF
  WRITE(*,'(A,T25,A,T35,A)') 'Continuity check:',                      &
       result(res_cur)%result,TRIM(result(res_cur)%reason)
  res_cur = res_cur + 1

  ! Perform ERROR / VALUE check (Check if error is greater than value)
  result(res_cur)%result = 'SKIPPED'
  result(res_cur)%reason = ''
  result(res_cur)%test = 'Error / Value'
  IF (ichk_opt%errorvaluechk) THEN
     result(res_cur)%result = 'PASSED'
     DO i = 1, points
        IF (ABS(array(i,xe_col)) > ABS(array(i,x_col))) THEN
           result(res_cur)%result = 'FAILED'
           WRITE(result(res_cur)%reason,'(A,G13.5,2A)',IOSTAT=n) 'error too high at ', &
                array(i,x_col),' ',TRIM(x_unit)
        END IF
        IF (array(i,ye_col) > array(i,y_col)) THEN
           result(res_cur)%result = 'FAILED'
           WRITE(result(res_cur)%reason,'(A,G13.5,2A)',IOSTAT=n) 'error too high at ', &
                array(i,y_col),' ',TRIM(y_unit)
        END IF
     END DO
  END IF
  WRITE(*,'(A,T25,A,T35,A)') 'Error / value check:',result(res_cur)%result,TRIM(result(res_cur)%reason)
  res_cur = res_cur + 1

  ! Perform reaction check
  result(res_cur)%result = 'SKIPPED'
  result(res_cur)%reason = ''                    ! Reason for a failed rseult
  result(res_cur)%test = 'Reaction'
  IF (ichk_opt%reactionchk) THEN
     CALL validreaction(opt%r,i,check)
     IF (i == 0) THEN
        result(res_cur)%result = 'PASSED'
     ELSE IF (i == -2) THEN
        ! Since can't check reaction, just skip test but give reason
        WRITE(result(res_cur)%reason,'(A)',IOSTAT=n) TRIM(check)
     ELSE
        result(res_cur)%result = 'FAILED'
        WRITE(result(res_cur)%reason,'(A)',IOSTAT=n) TRIM(check)
     END IF
  END IF
  WRITE(*,'(A,T25,A,T35,A)') 'Reaction check:',                        &
       result(res_cur)%result,TRIM(result(res_cur)%reason)
  res_cur = res_cur + 1

  ! Find all NARROW RESONANCES
!!$  result(res_cur)%result = 'SKIPPED'
!!$  result(res_cur)%reason = ''
!!$  result(res_cur)%test = 'Narrow resonance'
!!$
!!$  ! Make sure the positive and single valued check tests passed
!!$  IF ((result(1)%result == 'FAILED') .OR. (result(1)%result == 'SKIPPED')) THEN
!!$     result(res_cur)%reason = 'Positive check did not pass'
!!$     WRITE(*,'(A,T25,A,T35,A)') 'Narrow resonance check:','SKIPPED',TRIM(result(res_cur)%reason)
!!$  ELSE IF ((result(2)%result == 'FAILED') .OR. (result(2)%result == 'SKIPPED')) THEN
!!$     result(res_cur)%reason = 'Single valued check did not pass'
!!$     WRITE(*,'(A,T25,A,T35,A)') 'Narrow resonance check:','SKIPPED',TRIM(result(res_cur)%reason)
!!$  ELSE
!!$     fnr: SELECT CASE (lowercase(opt%data_type))
!!$     CASE ('s(e) ','cs(e) ') fnr
!!$        ! xf and yf are used to get to standard units (eV)
!!$        i = opt%plevel
!!$        CALL find_narrow_res(nres,xf*array(1:points,1),yf*array(1:points,3),nlimit,i)
!!$
!!$        ! Count the number of narrow resonances
!!$        j = 0
!!$        DO i = 1, UBOUND(nres,1)
!!$           IF (nres(j+1)%centroid > 0D0) j = j + 1 
!!$        END DO
!!$
!!$        WRITE(result(res_cur)%reason,'(I0,A,G12.4)',IOSTAT=n) j,       &
!!$             ' resonances with FWHM/Centroid < ',nlimit
!!$        result(res_cur)%result = 'PASSED'
!!$        WRITE(*,'(A,T25,A,T35,A)') 'Narrow resonance check:','PASSED',TRIM(result(res_cur)%reason)
!!$
!!$        IF (j > 0) THEN
!!$           ! Print resonance locations with 4 numbers per line
!!$           DO i = 1, j / 4
!!$              IF (i == 1) THEN
!!$                 WRITE(*,'(A,T33,5G12.4)') 'Resonance locations ('//TRIM(x_unit)//'): ', &
!!$                      (nres(n)%centroid/opt%e_units_in,n=1,4)
!!$              ELSE
!!$                 WRITE(*,'(T33,5G12.4)') (nres(n)%centroid/opt%e_units_in,n=(i-1)*4+1,i*4)
!!$              END IF
!!$           END DO
!!$           ! Print resonance locations with under 4 numbers per line
!!$           IF (j <= 4) THEN
!!$              check = 'Resonance locations ('//TRIM(x_unit)//'): '
!!$              i = 0
!!$           ELSE
!!$              check = ''
!!$              i = j / 4
!!$           END IF
!!$           IF (MOD(j,4) > 0)                                   &
!!$                WRITE(*,'(A,T33,'//ACHAR(MOD(j,4)+48)//'G12.4)') TRIM(check),    &
!!$                (nres(n)%centroid/opt%e_units_in,n=i*4+1,i*4+MOD(j,4))
!!$        END IF
!!$
!!$        ! Set options for saving narrow resonances into file
!!$        opt%data_format = 'NARROW_RESONANCE'
!!$        ! Set fmt_options to 'UNKNOWN' to overwrite output files
!!$        ! Set fmt_options to 'NEW' to not overwrite output files
!!$        opt%fmt_options = 'UNKNOWN'
!!$        ! Set output filename  and add .innr extension to file
!!$        n = INDEX(opt%file_in,'.inrd')
!!$        IF (n > 0) THEN
!!$           opt%file_out = opt%file_in(1:n-1) // '.innr'
!!$        ELSE
!!$           opt%file_out = opt%file_in // '.innr'
!!$        END IF
!!$
!!$        ! Save narrow resonances into file
!!$        ! Use fileio module to save file in narrow resonance format
!!$        CALL nres2file(nres,j,opt)
!!$
!!$     CASE default
!!$        result(res_cur)%reason = 'Wrong data type ' // opt%data_type
!!$        WRITE(*,'(A,T25,A,T35,A)') 'Narrow resonance check:','SKIPPED','Wrong data type '//opt%data_type
!!$     END SELECT fnr
!!$  END IF
  !res_cur = res_cur + 1

  ! Set options for saving results into file
  opt%data_format = 'INPUTCHECK'
  ! Set fmt_options to 'UNKNOWN' to overwrite output files
  ! Set fmt_options to 'NEW' to not overwrite output files
  opt%fmt_options = 'UNKNOWN'
  ! Set output filename  and add .inck extension to file
  n = INDEX(opt%file_in,'.inrd')
  IF (n > 0) THEN
     opt%file_out = opt%file_in(1:n-1) // '.inck'
  ELSE
     opt%file_out = opt%file_in // '.inck'
  END IF

  ! Save results into file
  ! Use fileio module to save file in inputcheck format
  CALL icresults2file(result,res_cur,opt)

END PROGRAM inputcheck

!---------------------------------------------------------------------
FUNCTION inputcheck_ver
  !PURPOSE = Return the CVS revision number of this file
  !STATUS = Complete and tested
  IMPLICIT NONE
  CHARACTER(LEN=20),PARAMETER     :: INCHECK_VERSION = '$Revision: 1.1.1.1 $'
  CHARACTER(LEN=10)               :: inputcheck_ver

  inputcheck_ver = INCHECK_VERSION(12:LEN_TRIM(INCHECK_VERSION)-2)

END FUNCTION inputcheck_ver

!---------------------------------------------------------------------
SUBROUTINE print_inputcheck_ver(PROGRAM_VER)
  !PURPOSE = Print the inputcheck version information
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  USE fileio
  USE io
  USE convert
  USE options
  USE math
  USE reaction
  USE reactionstrings
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)      :: PROGRAM_VER
  CHARACTER(LEN=10)                :: inputcheck_ver
  EXTERNAL                         :: inputcheck_ver

  WRITE(*,'(2A)') 'inputcheck version ',PROGRAM_VER
  WRITE(*,'( A)') 'inputcheck dependencies:'
  WRITE(*,'(T5,2A)') 'inputcheck.f90 version ',inputcheck_ver()
  WRITE(*,'(T5,2A)') 'io.f90 version ',io_ver()
  WRITE(*,'(T5,2A)') 'fileio.f90 version ',fileio_ver()
  WRITE(*,'(T5,2A)') 'convert.f90 version ',convert_ver()
  WRITE(*,'(T5,2A)') 'math.f90 version ',math_ver()
  WRITE(*,'(T5,2A)') 'options.f90 version ',options_ver()
  WRITE(*,'(T5,2A)') 'reaction.f90 version ',reaction_ver()
  WRITE(*,'(T5,2A)') 'reactionstrings.f90 version ',reactionstrings_ver()
  STOP

END SUBROUTINE print_inputcheck_ver
!---------------------------------------------------------------------

SUBROUTINE print_inputcheck_help(PROGRAM_VER)
  !PURPOSE = Print help screen for inputcheck
  !STATUS = Complete and tested
  !CAUTION = This subroutine does not return
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)    :: PROGRAM_VER

  WRITE(*,'(3A)') 'inputcheck ',PROGRAM_VER,' Help'
  WRITE(*,'(A)') 'This program performs sanity checks on input files'
  WRITE(*,'(A)') 'and provides information needed for other programs.'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Usage:'
  WRITE(*,'(A)') '  inputcheck [-h | -v]'
  WRITE(*,'(A)') '  inputcheck input_file [-h | -v] [option optionvalue ...] '
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Options are not case sensitive and may not be required.'
  WRITE(*,'(A)') '  -v or --version prints the program version'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'Options  Option Values    Description'
  WRITE(*,'(A)') ' -p      0,1,2,3,4        Level of screen output.  0 is none'
  WRITE(*,'(A)') ' -pos,-positive           Skip positive check'
  WRITE(*,'(A)') ' -sv,-singlevalued        Skip single valued check'
  WRITE(*,'(A)') ' -r,-range                Skip range checks'
  WRITE(*,'(A)') ' -c,-continuous           Skip continuity check'
  WRITE(*,'(A)') ' -ev,-errorvalue          Skip error / value check'
  WRITE(*,'(A)') ' -rc,-reaction            Skip reaction string check'
  WRITE(*,'(A)') ''
  WRITE(*,'(A)') 'See the inputcheck.txt file for more information.'
  STOP
END SUBROUTINE print_inputcheck_help
