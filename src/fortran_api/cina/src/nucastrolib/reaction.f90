MODULE reaction
!DESC = This module contains procedures for processing nuclear reactions
!DESC = Use the reactionstrings module for processing reaction strings

! Procedures in this module are:
!

! By default all procedures and global variables are private
  PRIVATE
  PUBLIC   :: reaction_ver,find_narrow_res

  TYPE,PUBLIC                     :: narrowresonance
     REAL(KIND=8)                 :: centroid = -1D0
     REAL(KIND=8)                 :: ymax = -1D0
     REAL(KIND=8)                 :: fwhm = -1D0
  END TYPE narrowresonance

CONTAINS
!---------------------------------------------------------------------
  FUNCTION reaction_ver
!PURPOSE = Return the cvs revision number for this file
!STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: reaction_ver
    CHARACTER(LEN=20),PARAMETER   :: REACTION_VERSION = '$Revision: 1.1.1.1 $'

    reaction_ver = REACTION_VERSION(12:LEN_TRIM(REACTION_VERSION)-2)

  END FUNCTION reaction_ver

!---------------------------------------------------------------------
  SUBROUTINE find_narrow_res(nres,x,y,nlimit,plevel)
!PURPOSE = Find narrow resonances in a function
!STATUS = In development
!DESC = nres is a narrowresonance variable
!DESC = x is the dependent variable (energy)
!DESC = y is the independent variable
!DESC = nlimit is the maximum width of a narrow resonance / centroid
!DESC = plevel is the print level and is optional
!DESC = The only variable changed on return is nres
!DESC = x and y should be sorted so that x is increasing
    USE math
    USE io
    IMPLICIT NONE
    TYPE(narrowresonance),INTENT(OUT):: nres(:)
    REAL(KIND=8),INTENT(IN)       :: x(:),y(:),nlimit
    INTEGER(KIND=4),INTENT(IN)    :: plevel
    OPTIONAL                      :: plevel

    INTEGER(KIND=4)               :: plvl,i,j,m,points,p,nrescur
    INTEGER(KIND=4)               :: startp,stopp,peaks(UBOUND(x,1) / 2 + 1)
    REAL(KIND=8)                  :: deriv1(UBOUND(x,1)),deriv2(UBOUND(x,1))
    REAL(KIND=8)                  :: noise_lvl,y2(UBOUND(y,1)),tmpr
    REAL(KIND=8)                  :: ml,mr,bl,br
    LOGICAL(KIND=1)               :: loop

    IF (PRESENT(plevel)) THEN
       plvl = plevel
    ELSE
       plvl = 2
    END IF
    points = UBOUND(x,1)
    nrescur = 1

    IF (UBOUND(y,1) /= points) CALL printerror('find_narrow_res: Number of points in x and y are different',1)

! Check the noise level of y because the remaining procedures are not tolerant of noise
! This will be implemented later
    y2 = y

! Get first and second derivatives
    deriv1 = numderiv(x,y2,plvl)
    deriv2 = numderiv(x(1:points-1),deriv1(1:points-1),plvl)

    IF (plvl >= 4) THEN
       DO i = 1,points-2
          print '(I4,3G)',i,y(i),deriv1(i),deriv2(i)
       END DO
    END IF

! Find threshold for peaks in second derivative
    noise_lvl =  noise_levelz(deriv2(1:points-2))

    IF (plvl >= 3) WRITE(*,*) 'noise threshold = ',noise_lvl,' (max = ',MAXVAL(deriv2(1:points-2)),')'

! Find concave down peaks
    i = 1
    p = 0       ! p is the number of peaks
    DO WHILE (i <= points-2)
       IF (deriv2(i) < -noise_lvl) THEN          ! noise_lvl is always positive
          tmpr = 0  ! tmpr is the maximum negative number
          p = p + 1;
          DO WHILE (deriv2(i) < -noise_lvl)
             IF (deriv2(i) < tmpr) THEN
                tmpr = deriv2(i)
                peaks(p) = i + 1        ! Add 1 because 2nd deriv has 2 less points than y
             END IF
             i = i + 1
             IF (i > points-2) THEN              ! Make sure i <= points-2
                i = i - 1
                deriv2(i) = -noise_lvl
             END IF
          END DO
       END IF
       i = i + 1
    END DO

! Find the points in each peak
    DO i = 1,p
! Find starting point of peak
       startp = peaks(i) - 1
       m = startp                                ! Index to max positive slope
       loop = .TRUE.
! Find maximum positive slope to the left of peak
       IF (startp <= 1) loop = .FALSE. 
       DO WHILE (loop)
          IF (deriv1(startp) < 0D0) loop = .FALSE.
          IF (deriv1(startp) > deriv1(m)) m = startp
          startp = startp - 1
          IF (startp <= 1) loop = .FALSE.
       END DO

! Now that m is set, compare slopes to m to find startp
       startp = peaks(i) - 1
       loop = .TRUE.
       IF (startp <= 1) loop = .FALSE. 
       DO WHILE (loop)
          tmpr = deriv1(startp) / deriv1(m)      ! tmpr = slope / max slope
          IF ((tmpr < 0.3) .AND. (startp <= m)) loop = .FALSE.
          startp = startp - 1
          IF (startp <= 1) loop = .FALSE.
       END DO
       startp = startp + 2
       IF (startp == peaks(i)) startp = peaks(i) - 1
       IF (startp < 1) startp = 1

! Find stopping point of peak
       stopp = peaks(i)
       m = stopp                                 ! Index to max positive slope
       loop = .TRUE.
! Find maximum negative slope to right of peak
       IF (stopp >= points) loop = .FALSE. 
       DO WHILE (loop)
          IF (deriv1(stopp) > 0D0) loop = .FALSE.
          IF (deriv1(stopp) < deriv1(m)) m = stopp
          stopp = stopp + 1
          IF (stopp >= points) loop = .FALSE.
       END DO

! Now that m is set, compare slopes to m to find stopp
       stopp = peaks(i)
       loop = .TRUE.
       IF (stopp >= points) loop = .FALSE. 
       DO WHILE (loop)
          tmpr = deriv1(stopp) / deriv1(m)       ! tmpr = slope / max slope 
          IF ((tmpr < 0.3) .AND. (stopp >= m)) loop = .FALSE.
          stopp = stopp + 1
          IF (stopp >= points) loop = .FALSE.
       END DO
       stopp = stopp - 1
       IF (stopp == peaks(i)) stopp = stopp + 1
       IF (stopp > points) stopp = points

       IF (plvl >= 3) WRITE(*,*) 'peaks=',peaks(i),' startp=',startp,' stopp=',stopp

! Represent the left and right sides of the peak with lines
! Average the slopes to get m
       ml = average(deriv1(startp:peaks(i)-1))
       mr = average(deriv1(peaks(i):stopp-1))
! y intercept of left line is Bl = Yaverage - Maverage * Xaverage
       bl = average(y(startp:peaks(i))) - ml * average(x(startp:peaks(i)))
       br = average(y(peaks(i):stopp)) - mr * average(x(peaks(i):stopp))

       IF (plvl >= 5) WRITE(*,'(4(A,G15.5))') 'ml=',ml,' bl=',bl,' mr=',mr,' br=',br

       IF (nrescur <= UBOUND(nres,1)) THEN
! Find the center value of the peak
          nres(nrescur)%centroid = (br - bl) / (ml - mr)
! Find the maximum of the peak
          nres(nrescur)%ymax = ml * nres(nrescur)%centroid + bl
          IF (nres(nrescur)%ymax < y(peaks(i))) nres(nrescur)%ymax = y(peaks(i))

! Find point to left of peak that is less than half max
          loop = .TRUE.
          startp = peaks(i) + 1
          DO WHILE (loop)
             startp = startp - 1
             IF (startp <= 1) loop = .FALSE.
             IF (y(startp) < nres(nrescur)%ymax / 2) loop = .FALSE.
          END DO
! Form line segment between point less than and greater than half max
          ml = y(startp + 1) - y(startp) / (x(startp + 1) - x(startp))
          bl = y(startp) - ml * x(startp)

! Get x value at half max
          nres(nrescur)%fwhm = (nres(nrescur)%ymax / 2 - bl) / ml

! Find point on right of peak that is less than half max
          loop = .TRUE.
          stopp = peaks(i) - 1
          DO WHILE (loop)
             stopp = stopp + 1
             IF (stopp >= points) loop = .FALSE.
             IF (y(stopp) < nres(nrescur)%ymax / 2) loop = .FALSE.
          END DO
! Form line segment between point less than and greater than half max
          mr = y(stopp - 1) - y(stopp) / (x(stopp - 1) - x(stopp))
          br = y(stopp) - mr * x(stopp)

! Get x value at half max
          tmpr = (nres(nrescur)%ymax / 2 - br) / mr
!          print *,nres(nrescur)%fwhm,tmpr
!          print *,startp,stopp
! Get Full Width at Half Max
          nres(nrescur)%fwhm = tmpr - nres(nrescur)%fwhm

          IF (plvl >= 3) WRITE(*,*) 'peaks=',peaks(i),' startp=',startp,' stopp=',stopp
          IF (plvl >= 5) WRITE(*,'(4(A,G15.5))') 'ml=',ml,' bl=',bl,' mr=',mr,' br=',br
          IF (plvl >= 3) WRITE(*,'(3(A,G15.5))') 'centroid=',nres(nrescur)%centroid,   &
               ' peak max=',nres(nrescur)%ymax,' FWHM=',nres(nrescur)%fwhm

! Delete nres entry if fwhm/centroid is not small enough to be "narrow"
          IF ((nres(nrescur)%fwhm / nres(nrescur)%centroid) > nlimit) THEN
             nres(nrescur)%fwhm = -1D0
             nres(nrescur)%centroid = -1D0
             nres(nrescur)%ymax = -1D0
          ELSE
             nrescur = nrescur + 1
          END IF
       END IF
    END DO

    IF ((plvl >= 1) .AND. (p > UBOUND(nres,1)))    &
         CALL printerror('find_narrow_res: Too many narrow resonances to hold in variable',0)

  END SUBROUTINE find_narrow_res

!---------------------------------------------------------------------
END MODULE reaction
