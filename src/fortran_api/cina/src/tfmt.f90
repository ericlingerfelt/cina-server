PROGRAM tfmt
  USE fmt_parm
  USE io
  IMPLICIT NONE
  TYPE(fmt_parm_opt) :: opt,opt_inv
  INTEGER :: i
  CHARACTER(LEN=FMT_PARM_LEN) :: tmps

  opt%reac_str = 'a + K39 -> Sc43'
  opt%unique_str = 'ec'
  opt%biblio = 'jpss'
  opt%inverse = .FALSE.
  opt%resonant = .FALSE.
  opt%resonant(2) = .TRUE.
  opt%qvalue = 3.0
  opt%a_num = 14

  DO i = 1, opt%a_num
     opt%a(i) = i
  END DO

  opt_inv%reac_str = '43Sc->a+K39'
  opt_inv%unique_str = 'ec'
  opt_inv%biblio = 'jpss'
  opt_inv%inverse = .TRUE.
  opt_inv%resonant = .TRUE.
  opt_inv%resonant(2) = .FALSE.
  opt_inv%qvalue = 3.0
  opt_inv%a_num = 14

  DO i = 1, opt_inv%a_num
     opt_inv%a(i) = -i
  END DO

!!$  tmps = parm_fmt_ascii_header(opt,iostat=i)
!!$  IF (i /= 0) THEN
!!$     print *,'error ',i
!!$     STOP
!!$  END IF
!!$  CALL print_long_string(TRIM(tmps))

  tmps = parm_fmt_full_netsu(opt,iostat=i,rtype_in='0')
  IF (i /= 0) THEN
     print *,'error ',i
     print *,TRIM(tmps)
     STOP
  END IF
  CALL print_long_string(TRIM(tmps))

  tmps = parm_fmt_full_netsu(opt_inv,iostat=i,rtype_in='0')
  IF (i /= 0) THEN
     print *,'error ',i
     print *,TRIM(tmps)
     STOP
  END IF
  CALL print_long_string(TRIM(tmps))

END PROGRAM tfmt
