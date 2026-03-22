MODULE fmt_parm

  USE reactionstrings

  USE constants



  TYPE,PUBLIC                     :: fmt_parm_opt

     CHARACTER(LEN=40)            :: reac_str = ''

     CHARACTER(LEN=20)            :: unique_str = ''

     CHARACTER(LEN=20)            :: biblio = ''

     LOGICAL(KIND=1)              :: inverse = .FALSE.

     LOGICAL(KIND=1)              :: resonant(MAX_A/7) = .FALSE.

     REAL(KIND=8)                 :: qvalue = 0.0

     REAL(KIND=8)                 :: a(MAX_A) = 0.0

     INTEGER(KIND=4)              :: a_num = 0

  END TYPE fmt_parm_opt



  INTEGER(KIND=4),PARAMETER,PUBLIC:: FMT_PARM_LEN = 10240

  CHARACTER(LEN=1),PARAMETER,PUBLIC:: LF = ACHAR(10)

  CHARACTER(LEN=1),PARAMETER,PUBLIC:: TAB = ACHAR(9)



CONTAINS

!---------------------------------------------------------------------

  FUNCTION fmt_parm_ver

!PURPOSE = Return the cvs revision number for this file

!STATUS = Complete and tested

    IMPLICIT NONE

    CHARACTER(LEN=10)             :: fmt_parm_ver

    CHARACTER(LEN=20),PARAMETER   :: FMT_PARM_VERSION = '$Revision: 1.1.1.1 $'



    fmt_parm_ver = FMT_PARM_VERSION(12:LEN_TRIM(FMT_PARM_VERSION)-2)



  END FUNCTION fmt_parm_ver



!---------------------------------------------------------------------

  FUNCTION parm_fmt_header(opt,format,iostat)

    USE convert

    IMPLICIT NONE

    TYPE(fmt_parm_opt),INTENT(IN) :: opt

    CHARACTER(LEN=*),INTENT(IN)   :: format

    INTEGER(KIND=4),INTENT(OUT)   :: iostat

    CHARACTER(LEN=FMT_PARM_LEN)   :: parm_fmt_header

    OPTIONAL                      :: iostat



    IF (PRESENT(iostat)) iostat = 0



    SELECT CASE (lowercase(format))

    CASE ('ascii')

       IF (PRESENT(iostat)) THEN

          parm_fmt_header = parm_fmt_ascii_header(opt,iostat)

       ELSE

          parm_fmt_header = parm_fmt_ascii_header(opt)

       END IF

    CASE ('full netsu')

       parm_fmt_header = ''

    CASE DEFAULT

       parm_fmt_header = 'Invalid format: ' // TRIM(format)

       IF (PRESENT(iostat)) iostat = -33

    END SELECT

  END FUNCTION parm_fmt_header



!---------------------------------------------------------------------

  FUNCTION parm_fmt_main(opt,format,iostat,rtype_in)

    USE convert

    IMPLICIT NONE

    TYPE(fmt_parm_opt),INTENT(IN) :: opt

    CHARACTER(LEN=*),INTENT(IN)   :: format,rtype_in

    INTEGER(KIND=4),INTENT(OUT)   :: iostat

    CHARACTER(LEN=FMT_PARM_LEN)   :: parm_fmt_main

    OPTIONAL                      :: iostat,rtype_in



    SELECT CASE (lowercase(format))

    CASE ('ascii')

       IF (PRESENT(iostat)) THEN

          parm_fmt_main = parm_fmt_ascii(opt,iostat)

       ELSE

          parm_fmt_main = parm_fmt_ascii(opt)

       END IF

    CASE ('full netsu')

       IF (PRESENT(iostat) .AND. .NOT. PRESENT(rtype_in)) THEN

          parm_fmt_main = parm_fmt_full_netsu(opt,iostat)

       ELSE IF (PRESENT(iostat) .AND. PRESENT(rtype_in)) THEN

          parm_fmt_main = parm_fmt_full_netsu(opt,iostat,rtype_in)

       ELSE IF (.NOT. PRESENT(iostat) .AND. PRESENT(rtype_in)) THEN

          parm_fmt_main = parm_fmt_full_netsu(opt,rtype_in=rtype_in)

       ELSE

          parm_fmt_main = parm_fmt_full_netsu(opt)

       END IF

    CASE DEFAULT

       parm_fmt_main = 'Invalid format: ' // TRIM(format)

       IF (PRESENT(iostat)) iostat = -33

    END SELECT

  END FUNCTION parm_fmt_main



!---------------------------------------------------------------------

  FUNCTION parm_fmt_sort_rtype(format)

    USE convert

    IMPLICIT NONE

    CHARACTER(LEN=*),INTENT(IN)   :: format

    LOGICAL(KIND=4)               :: parm_fmt_sort_rtype



    SELECT CASE (lowercase(format))

    CASE ('ascii')

       parm_fmt_sort_rtype = .FALSE.

    CASE ('full netsu')

       parm_fmt_sort_rtype = .TRUE.

    CASE DEFAULT

       parm_fmt_sort_rtype = .FALSE.

    END SELECT

  END FUNCTION parm_fmt_sort_rtype



!---------------------------------------------------------------------

  FUNCTION parm_fmt_ascii_header(opt,iostat)

    IMPLICIT NONE

    TYPE(fmt_parm_opt),INTENT(IN) :: opt

    CHARACTER(LEN=FMT_PARM_LEN)   :: parm_fmt_ascii_header

    INTEGER(KIND=4),INTENT(OUT)   :: iostat

    OPTIONAL                      :: iostat



    parm_fmt_ascii_header = 'ASCII format:' // LF // 'Rate=exp(' // &

         'a1+a2/t9+a3/t913+a4*t913+a5*t9+a6*t953+a7*ln(t9))' // LF

    IF (PRESENT(iostat)) iostat = 0

  END FUNCTION parm_fmt_ascii_header



!---------------------------------------------------------------------

  FUNCTION parm_fmt_ascii(opt,iostat)

    IMPLICIT NONE

    TYPE(fmt_parm_opt),INTENT(IN) :: opt

    CHARACTER(LEN=FMT_PARM_LEN)   :: parm_fmt_ascii

    INTEGER(KIND=4),INTENT(OUT)   :: iostat

    OPTIONAL                      :: iostat



    INTEGER(KIND=4),PARAMETER     :: ENTRY_LEN = 23

    CHARACTER(LEN=2),PARAMETER    :: ELS = '23'

    INTEGER(KIND=4)               :: i,e,start



    IF (PRESENT(iostat)) iostat = 0

    parm_fmt_ascii = opt%reac_str

    IF (opt%unique_str /= '') parm_fmt_ascii = TRIM(parm_fmt_ascii) // &

         ' [' // TRIM(opt%unique_str) // ']'



    start = LEN_TRIM(parm_fmt_ascii) + 1

    DO i = 1, opt%a_num

       WRITE(parm_fmt_ascii((i-1)*ENTRY_LEN+start:i*ENTRY_LEN+start), &

            '(1P,A,I0,A,E14.6E3,T'//ELS//')',IOSTAT=e) &

            LF // 'a(',i,') = ',opt%a(i)

       IF (e /= 0 .AND. PRESENT(iostat)) THEN

          iostat = e

          RETURN

       END IF

    END DO



    parm_fmt_ascii(start + 1 + opt%a_num * ENTRY_LEN:) = LF

  END FUNCTION parm_fmt_ascii



!---------------------------------------------------------------------

  FUNCTION parm_fmt_full_netsu(opt,iostat,rtype_in,r_in)

    USE convert

    IMPLICIT NONE

    TYPE(fmt_parm_opt),INTENT(IN) :: opt

    CHARACTER(LEN=1),INTENT(IN)   :: rtype_in

    TYPE(reactionparticles),INTENT(IN) :: r_in

    CHARACTER(LEN=FMT_PARM_LEN)   :: parm_fmt_full_netsu

    INTEGER(KIND=4),INTENT(OUT)   :: iostat

    OPTIONAL                      :: iostat,rtype_in,r_in



    TYPE(reactionparticles)       :: r

    CHARACTER(LEN=5)              :: p(6)

    CHARACTER(LEN=4)              :: biblio

    CHARACTER(LEN=1)              :: rtype,n,v

    INTEGER(KIND=4)               :: i,j,e

    INTEGER(KIND=4),PARAMETER     :: ENTRY_LEN = 158

    LOGICAL(KIND=4)               :: no_nr



    parm_fmt_full_netsu = ''

    IF (PRESENT(iostat)) iostat = 0



    IF (PRESENT(rtype_in)) THEN

       rtype = rtype_in

    ELSE

       rtype = ' '

    END IF



! Get reactionparticles for reaction string

    IF (PRESENT(r_in)) THEN

       r = r_in

    ELSE IF (opt%reac_str == '') THEN

    ELSE

       CALL read_reac_str(r,opt%reac_str,parm_fmt_full_netsu)

       IF (parm_fmt_full_netsu /= '') THEN

          IF (PRESENT(iostat)) iostat = -33

          RETURN

       END IF

    END IF



! Load p array with particle strings

    p = ''

    IF (opt%reac_str /= '') THEN

       i = 1       ! number of particles printed so far

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

    END IF

    IF (opt%inverse) THEN

       v = 'v'

    ELSE

       v = ' '

    END IF



    no_nr = .FALSE.

    IF (opt%reac_str == '') no_nr = .TRUE.

! Don't print n or r if a decay

    SELECT CASE (lowercase(opt%unique_str))

    CASE ('ec','bet+','bet-','bec')

       no_nr = .TRUE.

    END SELECT



    biblio = opt%biblio

    biblio = ADJUSTR(biblio)



    DO i = 1, opt%a_num / 7

       IF (no_nr) THEN

          n = ' '

       ELSE IF (opt%resonant(i)) THEN

          n = 'r'

       ELSE

          n = 'n'

       END IF

       WRITE(parm_fmt_full_netsu(1+(i-1)*ENTRY_LEN : i*ENTRY_LEN), &

            '(A1,T6,6A5,T44,A4,2A1,T53,1PE12.5,A1,0P4E13.6,A1,3E13.6,A1)', &

            IOSTAT=e) rtype, p, biblio, n, v, opt%qvalue, LF, &

            (opt%a(j), j=1+7*(i-1), 4+7*(i-1)), LF, &

            (opt%a(j), j=5+7*(i-1), 7+7*(i-1)), LF



       IF (e /= 0 .AND. PRESENT(iostat)) THEN

          iostat = e

          RETURN

       END IF

    END DO

  END FUNCTION parm_fmt_full_netsu



!---------------------------------------------------------------------

!---------------------------------------------------------------------

END MODULE fmt_parm

