MODULE math
!DESC = This module contains procedures for common math operations
!
! Procedures in this module are:
!

! By default all procedures and global variables are public
  PUBLIC

! Define an interface to the average function that automatically selects
! the function to use based on the type of input
  INTERFACE average
     MODULE PROCEDURE iaverage, daverage
  END INTERFACE


CONTAINS
!---------------------------------------------------------------------
  FUNCTION math_ver
!PURPOSE = Return the cvs revision number for this file
!STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: math_ver
    CHARACTER(LEN=20),PARAMETER   :: MATH_VERSION = '$Revision: 1.1.1.1 $'

    math_ver = MATH_VERSION(12:LEN_TRIM(MATH_VERSION)-2)

  END FUNCTION math_ver

!---------------------------------------------------------------------
  FUNCTION inumderiv(x,y,plevel)
!PURPOSE = Return the numerical first derivative of a 1D array or vector
!STATUS = Incomplete
!DESC = x and y are arrays of the same size
!DESC = The size of the result is UBOUND(x),1) - 1
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: x(:),y(:),plevel
    REAL(KIND=8)                  :: inumderiv(UBOUND(x,1)-1)
    OPTIONAL                      :: plevel

    inumderiv = 0.

  END FUNCTION inumderiv

!---------------------------------------------------------------------
  FUNCTION numderiv(x,y,plevel)
!PURPOSE = Return the numerical first derivative of a 1D array or vector
!STATUS = Complete and tested
!DESC = x and y are arrays of the same size
!DESC = The size of the result is UBOUND(x),1) - 1
    USE io
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)       :: x(:),y(:)
    INTEGER(KIND=4),INTENT(IN)    :: plevel
    REAL(KIND=8)                  :: numderiv(UBOUND(x,1)-1),dx,dy
    INTEGER(KIND=4)               :: i,points,plvl
    LOGICAL(KIND=1)               :: dbz,ovfl,tmpl
    OPTIONAL                      :: plevel

    dbz = .FALSE.
    ovfl = .FALSE.
    points = UBOUND(x,1)
    IF (PRESENT(plevel)) THEN
       plvl = plevel
    ELSE
       plvl = 2
    END IF
    IF (UBOUND(y,1) /= points) CALL printerror('numderiv: Number of points in x and y are different',1)

    DO i = 1, points - 1
       dx = (x(i+1)-x(i))
! Prevent division by zero
       IF (dx == 0D0) THEN
          dbz = .TRUE.
          numderiv(i) = HUGE(y)
       ELSE
          dy = (y(i+1)-y(i))
! Prevent overflow
          tmpl = .TRUE.
          IF (LOG10(ABS(dx)) /= 0) THEN
             IF (LOG10(ABS(dy)) / LOG10(ABS(dx)) >= RANGE(y)) THEN
                ovfl = .TRUE.
                tmpl = .FALSE.
                numderiv(i) = SIGN(1D0,dy) * SIGN(HUGE(y),dx)
             END IF
          END IF
          IF (tmpl) numderiv(i) = dy / dx
       END IF
    END DO

    IF (plvl .GE. 1) THEN
       IF (dbz) CALL printerror('numderiv: Division by zero while calculating derivative.',0)
       IF (ovfl) CALL printerror('numderiv: Overflow occurred while calculating derivative.',0)
    END IF

  END FUNCTION numderiv

!---------------------------------------------------------------------
  SUBROUTINE bins(y,bin_num,bin_max,bin_cnt,bin_type)
!PURPOSE = Put a vector into bins and return the counts in each bin
!STATUS = In development
!DESC = y is a 1D array of values to put into bins
!DESC = bin_num is the number of bins to create
!DESC = bin_max is returned with the maximum value allowed for a bin
!DESC = The minimum value allowed for a bin is the max of the previous bin or min(y)
!DESC = bin_cnt is a count of the number of y values in each bin
!DESC = bin_type controls how the bins are spaced and can be 'linear' or 'noise'
!DESC = noise bins have almost all of the bins near the minimum of y
!DESC = bin_type is optional with the default value 'linear'
    USE io
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)       :: y(:)
    INTEGER(KIND=4),INTENT(IN)    :: bin_num
    REAL(KIND=8),INTENT(OUT)      :: bin_max(:)
    INTEGER(KIND=4),INTENT(OUT)   :: bin_cnt(:)
    CHARACTER(LEN=*),INTENT(IN)   :: bin_type

    INTEGER(KIND=4)               :: i,j,points
    CHARACTER(LEN=15)             :: bintype
    REAL(KIND=8)                  :: tmpr1,tmpr2,tmpr3,tmpr4
    OPTIONAL                      :: bin_type

    IF (PRESENT(bin_type)) THEN
       bintype = bin_type
    ELSE 
       bintype = 'linear'
    END IF

    points = UBOUND(y,1)
    bin_max = 0D0
    bin_cnt = 0


! Argument checking
    IF (bin_num < 1) CALL printerror('bins: bin_num must be >= 1',1)
    IF (UBOUND(bin_max,1) < bin_num) CALL printerror('bins: bin_max is too small',1)
    IF (UBOUND(bin_cnt,1) < bin_num) CALL printerror('bins: bin_cnt is too small',1)

! Create values for bin_max.
! Get the max absolute value of y to know the max value needed for a bin
    tmpr1 = MAXVAL(y(1:points),1)
! Get the min absolute value of y to determine bin spacing
    tmpr2 = MINVAL(y(1:points),1)

    SELECT CASE (bintype)
    CASE ('linear ')
       DO i = 1,bin_num
          bin_max(i) = i * (tmpr1-tmpr2) / bin_num + tmpr2
       END DO
    CASE ('noise ')
       tmpr3 = 30
! Get the max value of x so that exp(x^2) is tmpr3
       tmpr4 = SQRT(LOG(tmpr3))
       DO i = 1,bin_num
! The EXP function varies from 1 to tmpr3 with most of the values < 12% of tmpr3
! bin_max(i) is scaled from 0 to 1
          bin_max(i) = (EXP((i * tmpr4 / bin_num) ** 2) -1 ) / (tmpr3 - 1)
! Multiply to put on same scale as y
          bin_max(i) = tmpr2 + (tmpr1 - tmpr2) * bin_max(i)
       END DO
    CASE DEFAULT
       CALL printerror('bins: unknown bin_type '//bintype,1)
    END SELECT

! Make sure last bin will hold the max of y
    IF (MAXVAL(y(1:points),1) > bin_max(bin_num)) bin_max(bin_num) = MAXVAL(y(1:points),1)

! Go through every point and put it in a bin
    DO i = 1, points
! Start searching the smallest bin until you find the right one
       j = 1
       DO WHILE (y(i) > bin_max(j))
          j = j + 1
       END DO
       bin_cnt(j) = bin_cnt(j) + 1
    END DO

  END SUBROUTINE bins

!---------------------------------------------------------------------
  FUNCTION daverage(in)
!PURPOSE = Calculate the average or mean of a REAL(KIND=8) vector
!STATUS = Complete and tested
!RETURNS = REAL(KIND=8) value
!DESC = in must be a 1D array or vector
!DESC = average calculates the average of the WHOLE array
!DESC = so pass a section of an array to calculate the average of part of an array
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)       :: in(:)
    REAL(KIND=8)                  :: daverage

    daverage = SUM(in) / (UBOUND(in,1) - LBOUND(in,1) + 1)

  END FUNCTION daverage

!---------------------------------------------------------------------
  FUNCTION iaverage(in)
!PURPOSE = Calculate the average or mean of an INTEGER(KIND=4) vector
!STATUS = Complete and tested
!RETURNS = REAL(KIND=8) value
!DESC = in must be a 1D array or vector
!DESC = average calculates the average of the WHOLE array
!DESC = so pass a section of an array to calculate the average of part of an array
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: in(:)
    REAL(KIND=8)                  :: iaverage

    iaverage = REAL(SUM(in)) / (UBOUND(in,1) - LBOUND(in,1))

  END FUNCTION iaverage

!---------------------------------------------------------------------
  FUNCTION noise_levelz(y)
!PURPOSE = Find the noise level of a vector
!STATUS = In development
!RETURNS = positive REAL(KIND=8) value slightly above the noise_level
!DESC = y should be a function such that it would always be zero without noise
!DESC = Consider setting y to the second derivative of functions that aren't normally zero
!DESC = y and yderiv2 should be sorted so that x values are increasing
!DESC = yderiv2 is the second derivative (numerical) of y
!DESC = The numderiv function can calculate derivatives
    IMPLICIT NONE
    REAL(KIND=8)                  :: noise_levelz
    REAL(KIND=8),INTENT(IN)       :: y(:)
    INTEGER(KIND=4),PARAMETER     :: bin_num = 30
    REAL(KIND=8)                  :: bin_max(bin_num)      ! The max value allowed in a bin
    INTEGER(KIND=4)               :: bin_cnt(bin_num)      ! The number of counts in a bin
    REAL(KIND=8)                  :: tmpr
    INTEGER(KIND=4)               :: i,j,m,mj,points

    points = UBOUND(y,1)

! Find a noise threshold by putting y into bins
! Then look for bins with a high number of counts near 0
! Select the number of bins 
    CALL bins(ABS(y),bin_num,bin_max,bin_cnt,'noise')

! Find where the count is great enough
    m = 0  ! holds maximum count
    j = 0  ! holds index to the end of noise
    mj = 0 ! holds the maximum at last change of j
    DO i = 1, bin_num
! See if this bin has enough points
       IF (REAL(bin_cnt(i)) >= (REAL(points)/bin_num)) THEN
! See how count compares to the max
          IF (bin_cnt(i) > m) THEN
             m = bin_cnt(i)
          END IF
       ELSE
!          print *,.TRUE.
          IF (m > mj) THEN
             j = i
             mj = m
          END IF
       END IF
! WRITE(*,'(I3,G,4I3)') i,bin_max(i),bin_cnt(i),j,m,mj
    END DO
    
    IF (j == 0) j = 1

    noise_levelz = bin_max(j)

  END FUNCTION noise_levelz
END MODULE math
