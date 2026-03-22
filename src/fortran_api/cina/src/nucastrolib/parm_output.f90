MODULE parm_output
!DESC = This module contains routines for formatting parameters
  USE constants
  USE io

! By default all procedures and global variables are private
  PRIVATE

  PUBLIC  :: read_parm,gen_parm_fmt,parm_fmt_ascii,parm_fmt_html
  PUBLIC  :: parm_fmt_fortran,parm_fmt_netsu,parm_fmt_full_netsu

!INTEGER(KIND=4),PARAMETER,PUBLIC :: MAX_A_NUM = 49

CONTAINS
!---------------------------------------------------------------------
  FUNCTION parm_output_ver
!PURPOSE = Return the cvs revision number for this file
!STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: parm_output_ver
    CHARACTER(LEN=20),PARAMETER   :: POUT_VERSION = '$Revision: 1.1.1.1 $'

    parm_output_ver = POUT_VERSION(12:LEN_TRIM(POUT_VERSION)-2)

  END FUNCTION parm_output_ver

!---------------------------------------------------------------------
  SUBROUTINE read_parm(a_num,a,file)
!PURPOSE = Read a file, and return parameter array
!STATUS = 
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: file
    REAL(KIND=8),INTENT(OUT)      :: a(MAX_A)
    INTEGER(KIND=4),INTENT(OUT)   :: a_num
    INTEGER(KIND=4)               :: i,j
    CHARACTER(LEN=80)             :: tmps

    OPEN(20,FILE=file,IOSTAT=i,ACTION='READ')
    IF (i /= 0) THEN
       WRITE(tmps,'(3A,I0)') 'Can not open ',TRIM(file),'  ERROR ',i
       CALL printerror(tmps,1)
    END IF

! Read in the number of parameters
    READ(20,'(I)',IOSTAT=i) a_num
    IF (i /= 0) THEN
       WRITE(tmps,'(3A,I0)') 'Can not read number of parameters in ',TRIM(file),'  ERROR ',i
       CALL printerror(tmps,1)
    END IF

! Make sure parameters will fit in the array
    IF (a_num > UBOUND(a,1)) THEN
       WRITE(tmps,'(A,I0,3A)') 'The number of parameters (',a_num,') in ',TRIM(file),' is too large'
       CALL printerror(tmps,1)
    END IF

    DO j = 1,a_num
       READ(20,'(G)',IOSTAT=i) a(j)
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,3A,I0)') 'Can not read in parameter ',j,' in ',TRIM(file),'  ERROR ',i
          CALL printerror(tmps,1)
       END IF
    END DO
  END SUBROUTINE read_parm

!---------------------------------------------------------------------
  SUBROUTINE gen_parm_fmt(opt)
!PURPOSE = Call the appropriate routine to generate the first format in opt%formats
!STATUS = 
    USE options
    USE convert
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    INTEGER(KIND=4)                :: i,c
    CHARACTER(LEN=20)              :: fmt
    CHARACTER(LEN=80)              :: tmps

    c = INDEX(opt%formats,',')    ! Get the index of the first comma
    IF (c == 0) THEN
       fmt = opt%formats
       opt%formats = ''
    ELSE
       fmt = opt%formats(:c-1)
       tmps = opt%formats(c+1:)
! If tmps is only a comma, then don't bother saving it
       IF (tmps == ', ') tmps = ''

       opt%formats = tmps
    END IF
! fmt is now the first format in opt%formats

    SELECT CASE (lowercase(fmt))
    CASE ('ascii ')
       CALL parm_fmt_ascii(opt)
    CASE ('html ')
       CALL parm_fmt_html(opt)
    CASE ('fortran ')
       CALL parm_fmt_fortran(opt)
    CASE ('netsu ')
       CALL parm_fmt_netsu(opt)
    CASE ('full_netsu ')
       CALL parm_fmt_full_netsu(opt)
    CASE ('reaclib ')
       CALL parm_fmt_reaclib(opt)
    CASE DEFAULT
       CALL printerror('Unknown format "'//TRIM(fmt)//'" ignored.',0)
    END SELECT

  END SUBROUTINE gen_parm_fmt

!---------------------------------------------------------------------
  SUBROUTINE parm_fmt_ascii(opt)
!PURPOSE = Generate ASCII parameter format
!STATUS = Complete and tested
    USE reactionstrings
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    TYPE(reactionparticles)        :: r_inv
    INTEGER(KIND=4)                :: i,j,m
    CHARACTER(LEN=80)              :: tmps,tmps2,tmps3

! Write header
    WRITE(20,'(A/,A)',IOSTAT=i) 'ASCII format:','Rate=exp(a1+a2/t9+a3/t913+a4*t913+a5*t9+a6*t953+a7*ln(t9))'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing header to output file.'
       CALL printerror(tmps,1)
    END IF

! Create header in tmps
    tmps = getreac_str(opt%r,0)
    WRITE(20,'(A)',IOSTAT=i) TRIM(tmps)
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing ASCII format to output file.'
       CALL printerror(tmps,1)
    END IF

! Print parameters
    DO j = 1,opt%a_num
       WRITE(20,'(A,I0,A,E13.6)',IOSTAT=i) 'a(',j,') = ',opt%a(j)
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing ASCII format to output file.'
          CALL printerror(tmps,1)
       END IF
    END DO

! Print inverse parameters if specified on command line
    IF (LEN_TRIM(opt%inv_file) > 0) THEN
! Print inverse reaction string
       r_inv = getinverse(opt%r)
       tmps = getreac_str(r_inv,0)
       WRITE(20,'(/A)',IOSTAT=i) TRIM(tmps)
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing ASCII format to output file.'
          CALL printerror(tmps,1)
       END IF

! Print inverse parameters
       DO j = 1,opt%a_inv_num
          WRITE(20,'(A,I0,A,E13.6)',IOSTAT=i) 'a(',j,') = ',opt%a_inv(j)
          IF (i /= 0) THEN
             WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing ASCII format to output file.'
             CALL printerror(tmps,1)
          END IF
       END DO
    END IF

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing footer to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE parm_fmt_ascii

!---------------------------------------------------------------------
  SUBROUTINE parm_fmt_reaclib(opt)
!PURPOSE = Generate REACLIB parameter format
!STATUS = Complete and tested
    USE options
    USE io
    USE reactionstrings
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    TYPE(reactionparticles)        :: r
    INTEGER(KIND=4)                :: i,j
    CHARACTER(LEN=80)              :: tmps

! Write header
    WRITE(20,'(A)',IOSTAT=i) 'REACLIB format:'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing header to output file.'
       CALL printerror(tmps,1)
    END IF

! Print header to reaclib file
    CALL print_reaclib_header(opt,'a')

    CALL print_reaclib(opt,'a')

! Print inverse parameters if specified on command line
    IF (LEN_TRIM(opt%inv_file) > 0) THEN
! See if inverse should be in different table
       r = getinverse(opt%r)
       i = getreactype(r)
       IF (i /= getreactype(opt%r)) THEN
! Print header to reaclib file
          WRITE(20,'(A/)',IOSTAT=i) ''
          CALL print_reaclib_header(opt,'a_inv')
       END IF

       CALL print_reaclib(opt,'a_inv')
    END IF

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing footer to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE parm_fmt_reaclib

!---------------------------------------------------------------------
  SUBROUTINE print_reaclib_header(opt,type)
!PURPOSE = Generate REACLIB parameter format
!STATUS = Complete and tested
    USE reactionstrings
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    CHARACTER(LEN=*),INTENT(IN)    :: type
    TYPE(reactionparticles)        :: r
    INTEGER(KIND=4)                :: i,j
    CHARACTER(LEN=80)              :: tmps

    SELECT CASE (type)
    CASE ('a ')
       r = opt%r
    CASE ('a_inv ')
       r = getinverse(opt%r)
    CASE DEFAULT
       CALL printerror('Invalid type in print_reaclib',1)
    END SELECT

    j = getreactype(r)
    IF (j == 1) THEN
       WRITE(20,'(A//,T3,A1,A,A/,A/,2(/A))',IOSTAT=i) ' Table 1. Decay Rates',opt%biblio,'-',TRIM(opt%desc), &
            '  Rate=ln2/T1/2','    Decay        Q-value(MeV)      Rate',REPEAT('-',42)
    ELSE
       tmps = getreactype_s(r)
       WRITE(20,'(A,I0,2A//,A,A4,A,A,5(/A))',IOSTAT=i) '                                        Table ', &
            j,'. Reaction Rates ',TRIM(tmps),' References: ',opt%biblio,'-',TRIM(opt%desc), &
            '      r-resonant,  n-nonresonant,  v-from inverse reaction', &
            '      Rate=exp(a1+a2/t9+a3/t913+a4*t913+a5*t9+a6*t953+a7*ln(t9))','', &
            ' Ref.     Reaction                a1            a2           a3           a4            a5           a6           a7',&
            REPEAT('-',132)
    END IF
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing reaclib header to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE print_reaclib_header

!---------------------------------------------------------------------
  SUBROUTINE print_reaclib(opt,type)
!PURPOSE = Generate REACLIB parameter format
!STATUS = Complete and tested
    USE reactionstrings
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    CHARACTER(LEN=*),INTENT(IN)    :: type
    TYPE(reactionparticles)        :: r
    INTEGER(KIND=4)                :: i,j,a_num
    CHARACTER(LEN=1)               :: t5,t6
    CHARACTER(LEN=2)               :: rs_len
    CHARACTER(LEN=80)              :: r_string,tmps
    REAL(KIND=8)                   :: a(MAX_A)

    SELECT CASE (type)
    CASE ('a ')
       r = opt%r
       a = opt%a
       a_num = opt%a_num
       t6 = ' '                         ! inverse rate
    CASE ('a_inv ')
       r = getinverse(opt%r)
       a = opt%a_inv
       a_num = opt%a_inv_num
       t6 = 'v'                         ! inverse rate
    CASE DEFAULT
       CALL printerror('Invalid type in print_reaclib',1)
    END SELECT

    r_string = getreac_str(r,2)
    i = getreactype(r)
    j = LEN_TRIM(r_string)

    SELECT CASE (i)
    CASE (1)
! Print from here, not in loop below
! Get string for first reactant
       r_string = getreac_s(r,1,1)
       j = 5 - LEN_TRIM(r_string)     ! j is the number of spaces to add
       IF (j < 0) THEN
          j = 0
          CALL printerror('Extra space was included in output to make room for reaction string',0)
       END IF
! Add j spaces
       r_string = REPEAT(' ',j) // r_string
       IF (opt%ec) THEN
          rs_len = 'EC'
       ELSE IF (opt%bplus) THEN
          rs_len = 'B+'
       ELSE
          rs_len = 'B-'
       END IF
! Get string for first product
       tmps = getprod_s(r,1,1)
       IF (LEN_TRIM(tmps) > 7) THEN
          j = 0
          CALL printerror('The string representating the product was truncated.',0)
          tmps = tmps(1:7)
       END IF

       WRITE(20,'(A1,A5,4A,T17,1Pe12.5,0Pe14.6)',IOSTAT=i) opt%biblio,r_string, &
            '(',rs_len,')',TRIM(tmps),opt%qvalue,a(1)
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing REACLIB format to output file.'
          CALL printerror(tmps,1)
       END IF
       RETURN
    CASE (2)
       rs_len = '31'
    CASE (3)
       rs_len = '34'
    CASE (4)
       rs_len = '31'
    CASE (5)
       rs_len = '34'
    CASE (6)
       rs_len = '37'
    CASE (7)
       rs_len = '40'
    CASE (8)
       rs_len = '40'
    CASE (9)
       WRITE(rs_len,'(I2)') j + 2
    CASE DEFAULT
       CALL printerror('Unknown reaction type in print_reaclib',1)
    END SELECT

! Check if rs_len is large enough
    READ(rs_len,'(I)') i
    IF (j + 1 > i) THEN
       CALL printerror('Extra spaces have been included to make room for full reaction string.',0)
       WRITE(rs_len,'(I2)') j + 1
    END IF

    DO i = 1,a_num/7
       IF (i == 1) THEN
          t5 = 'n'
       ELSE
          t5 = 'r'
       END IF

       WRITE(20,'(A4,2A,T9,A,T'//rs_len//',4E13.6,E14.6,2E13.6)',IOSTAT=j) opt%biblio,t5,t6,TRIM(r_string),(a(j+(i-1)*7),j=1,7)
       IF (j /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',j,' while writing REACLIB format to output file.'
          CALL printerror(tmps,1)
       END IF
    END DO
  END SUBROUTINE print_reaclib

!---------------------------------------------------------------------
  SUBROUTINE parm_fmt_fortran(opt)
!PURPOSE = Generate FORTRAN parameter format
!STATUS = Complete and tested
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    INTEGER(KIND=4)                :: i
    CHARACTER(LEN=80)              :: tmps

! Write header
    WRITE(20,'(A)',IOSTAT=i) 'FORTRAN format:'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing header to output file.'
       CALL printerror(tmps,1)
    END IF

    CALL print_fortran(opt,'a')

! Print inverse parameters if specified on command line
    IF (LEN_TRIM(opt%inv_file) > 0) THEN
       CALL print_fortran(opt,'a_inv')
    END IF

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing footer to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE parm_fmt_fortran

!---------------------------------------------------------------------
  SUBROUTINE print_fortran(opt,type)
!PURPOSE = Generate REACLIB parameter format
!STATUS = Complete and tested
    USE reactionstrings
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    CHARACTER(LEN=*),INTENT(IN)    :: type
    TYPE(reactionparticles)        :: r
    INTEGER(KIND=4)                :: i,j,a_num
    CHARACTER(LEN=80)              :: r_string,tmps
    REAL(KIND=8)                   :: a(MAX_A)

    SELECT CASE (type)
    CASE ('a ')
       r = opt%r
       a = opt%a
       a_num = opt%a_num
    CASE ('a_inv ')
       r = getinverse(opt%r)
       a = opt%a_inv
       a_num = opt%a_inv_num
    CASE DEFAULT
       CALL printerror('Invalid type in print_fortran',1)
    END SELECT

    r_string = getreac_str(r,2)

! Used fewer WRITE statements to reduce the number of times I have to check for an error
    WRITE(20,'(3(A,T7,A/),2(T7,A/),/T7,A)',IOSTAT=i) 'c',TRIM(r_string), &
         'c',TRIM(opt%desc),'c','t=temperature T9 in 1E09 K',  &
         'onethrd=1.0/3.0','fivthrd=5.0/3.0','rate='
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing FORTRAN format to output file.'
       CALL printerror(tmps,1)
    END IF

    DO j = 1,a_num/7
       IF (j == 1) THEN
          tmps = '  exp('
       ELSE
          tmps = '+ exp('
       END IF
       WRITE(20,'(T6,A1,T13,A,7(/T6,A1,T17,A,E13.6,A))',IOSTAT=i) '|',TRIM(tmps), &
            '|','  ',a(1+7*(j-1)),'','|','+ ',a(2+7*(j-1)),' / t', &
            '|','+ ',a(3+7*(j-1)),' / (t ** onethrd)','|','+ ',a(4+7*(j-1)),' * (t ** onethrd)', &
            '|','+ ',a(5+7*(j-1)),' * t','|','+ ',a(6+7*(j-1)),' * (t ** fivthrd)', &
            '|','+ ',a(7+7*(j-1)),' * log(t))'
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing FORTRAN format to output file.'
          CALL printerror(tmps,1)
       END IF
    END DO

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing FORTRAN format to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE print_fortran

!---------------------------------------------------------------------
  SUBROUTINE parm_fmt_netsu(opt)
!PURPOSE = Generate NETSU parameter format
!STATUS = Complete and tested
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    INTEGER(KIND=4)                :: i
    CHARACTER(LEN=80)              :: tmps

! Write header
    WRITE(20,'(A)',IOSTAT=i) 'NETSU format:'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing header to output file.'
       CALL printerror(tmps,1)
    END IF

    CALL print_netsu(opt,'a')

! Print inverse parameters if specified on command line
    IF (LEN_TRIM(opt%inv_file) > 0) THEN
       CALL print_netsu(opt,'a_inv')
    END IF

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing footer to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE parm_fmt_netsu

!---------------------------------------------------------------------
  SUBROUTINE print_netsu(opt,type)
!PURPOSE = Generate NETSU parameter format
!STATUS = Complete and tested
    USE reactionstrings
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    CHARACTER(LEN=*),INTENT(IN)    :: type
    TYPE(reactionparticles)        :: r
    INTEGER(KIND=4)                :: i,j,a_num
    CHARACTER(LEN=80)              :: r_string,tmps
    REAL(KIND=8)                   :: a(MAX_A)

    SELECT CASE (type)
    CASE ('a ')
       r = opt%r
       a = opt%a
       a_num = opt%a_num
    CASE ('a_inv ')
       r = getinverse(opt%r)
       a = opt%a_inv
       a_num = opt%a_inv_num
    CASE DEFAULT
       CALL printerror('Invalid type in print_netsu',1)
    END SELECT

    r_string = getreac_str(r,2)

    WRITE(20,'(A)',IOSTAT=i) TRIM(r_string)
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing NETSU format to output file.'
       CALL printerror(tmps,1)
    END IF

    DO j = 1,a_num/7
       WRITE(20,'(4E13.6 / 3E13.6)',IOSTAT=i) (a(i),i=1+7*(j-1),7+7*(j-1))
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing NETSU format to output file.'
          CALL printerror(tmps,1)
       END IF
    END DO

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing NETSU format to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE print_netsu

!---------------------------------------------------------------------
  SUBROUTINE parm_fmt_full_netsu(opt)
!PURPOSE = Generate FULL_NETSU parameter format
!STATUS = Complete and tested
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    INTEGER(KIND=4)                :: i
    CHARACTER(LEN=80)              :: tmps

! Write header
    WRITE(20,'(3(A/))',IOSTAT=i) 'FULL NETSU format:', &
         'The first line contains the particles, bibliographic string,', &
         'flags for resonance and inverse, and the Q-value'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing header to output file.'
       CALL printerror(tmps,1)
    END IF

    CALL print_full_netsu(opt,'a')

! Print inverse parameters if specified on command line
    IF (LEN_TRIM(opt%inv_file) > 0) THEN
       CALL print_full_netsu(opt,'a_inv')
    END IF

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing footer to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE parm_fmt_full_netsu

!---------------------------------------------------------------------
  SUBROUTINE print_full_netsu(opt,type)
!PURPOSE = Generate FULL_NETSU parameter format
!STATUS = Complete and tested
    USE reactionstrings
    USE options
    USE io
    USE convert
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    CHARACTER(LEN=*),INTENT(IN)    :: type
    TYPE(reactionparticles)        :: r
    INTEGER(KIND=4)                :: i,j,a_num
    CHARACTER(LEN=80)              :: tmps
    CHARACTER(LEN=1)               :: n,v
    CHARACTER(LEN=5)               :: p(6)=''
    REAL(KIND=8)                   :: a(MAX_A),qvalue

    SELECT CASE (type)
    CASE ('a ')
       r = opt%r
       a = opt%a
       a_num = opt%a_num
       v = ' '
       qvalue = opt%qvalue
    CASE ('a_inv ')
       r = getinverse(opt%r)
       a = opt%a_inv
       a_num = opt%a_inv_num
       v = 'v'
       qvalue = - opt%qvalue
    CASE DEFAULT
       CALL printerror('Invalid type in print_full_netsu',1)
    END SELECT

! Load p array with particle strings
    i = 1        ! number of particles printed so far
    j = getreac_num(r)
    DO WHILE (i <= j)
       p(i) = lowercase(getreac_s(r,i,1))
       p(i) = ADJUSTR(p(i))
       i = i + 1
    END DO
    DO WHILE (i <= j + getprod_num(r))
       p(i) = lowercase(getprod_s(r,i-j,1))
       p(i) = ADJUSTR(p(i))
       i = i + 1
    END DO

    DO j = 1,a_num/7
       IF (j == 1) THEN
          n = 'n'
       ELSE
          n = 'r'
       END IF
       WRITE(20,'(A,T6,6A,T44,A4,2A1,T53,E13.6/,4E13.6/3E13.6)',IOSTAT=i) &
            ' ',p,opt%biblio,n,v,qvalue,(a(i),i=1+7*(j-1),7+7*(j-1))
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing FULL_NETSU format to output file.'
          CALL printerror(tmps,1)
       END IF
    END DO
  END SUBROUTINE print_full_netsu

!---------------------------------------------------------------------
  SUBROUTINE parm_fmt_html(opt)
!PURPOSE = Generate HTML parameter format
!STATUS = Complete and tested
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    INTEGER(KIND=4)                :: i
    CHARACTER(LEN=80)              :: tmps

! Write header
    WRITE(20,'(A//,A)',IOSTAT=i) 'HTML format:','<P>Rate=exp(a1+a2/t9+a3/t913+a4*t913+a5*t9+a6*t953+a7*ln(t9))</P><BR>'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing header to output file.'
       CALL printerror(tmps,1)
    END IF

    CALL print_html(opt,'a')

! Print inverse parameters if specified on command line
    IF (LEN_TRIM(opt%inv_file) > 0) THEN
       CALL print_html(opt,'a_inv')
    END IF

! Print blank line
    WRITE(20,'(A)',IOSTAT=i) ''
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing footer to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE parm_fmt_html

!---------------------------------------------------------------------
  SUBROUTINE print_html(opt,type)
!PURPOSE = Generate HTML parameter format
!STATUS = Complete and tested
    USE reactionstrings
    USE options
    USE io
    IMPLICIT NONE
    TYPE(fmtoutoptions),INTENT(INOUT):: opt
    CHARACTER(LEN=*),INTENT(IN)    :: type
    TYPE(reactionparticles)        :: r
    INTEGER(KIND=4)                :: i,j,a_num
    CHARACTER(LEN=80)              :: r_string,tmps
    REAL(KIND=8)                   :: a(MAX_A)

    SELECT CASE (type)
    CASE ('a ')
       r = opt%r
       a = opt%a
       a_num = opt%a_num
    CASE ('a_inv ')
       r = getinverse(opt%r)
       a = opt%a_inv
       a_num = opt%a_inv_num
    CASE DEFAULT
       CALL printerror('Invalid type in print_html',1)
    END SELECT

    r_string = getreac_str(r,2)

    WRITE(20,'(3A/,3A/,A)',IOSTAT=i) '<FONT SIZE="+1">',TRIM(r_string),'</FONT>', &
         '<FONT SIZE="-1">',TRIM(opt%desc),'</FONT>','<PRE>'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing HTML format to output file.'
       CALL printerror(tmps,1)
    END IF

    DO j = 1,a_num
       WRITE(20,'(A,I0,A,E13.6)',IOSTAT=i) 'a(',j,') = ',a(j)
       IF (i /= 0) THEN
          WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing HTML format to output file.'
          CALL printerror(tmps,1)
       END IF
    END DO

    WRITE(20,'(A/)',IOSTAT=i) '</PRE>'
    IF (i /= 0) THEN
       WRITE(tmps,'(A,I0,A)') 'Error ',i,' while writing HTML format to output file.'
       CALL printerror(tmps,1)
    END IF
  END SUBROUTINE print_html

!---------------------------------------------------------------------
END MODULE parm_output
