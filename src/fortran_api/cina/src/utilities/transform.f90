!****im* transform.robodoc.f90/transform
!* NOTES
!* This doc created with /home/sharp/gp/robodock.rc.
!* 
!* Any write, open, or close statements that deal with the three "_unit" constants,
!* R_unit, chi_unit, and chistep_unit, are solely for the purpose of printing development logs.
!* 
!* no_o_pts and no_o_files depend on the data in match.dat.
!* 
!* Most of the subprograms in this module use assumed shape arrays.
!* 
!* SOURCE

MODULE transform

integer, parameter :: MAX_POINTS=99999, MAX_A=49, no_o_pts=79, no_o_files=28, max_match_a=21
!integer, parameter :: R_unit=1000, chi_unit=1001, chistep_unit=2000

logical :: initial_run(3)=.true.

real, parameter :: one3=1.0/3.0,five3=5.0/3.0

real (KIND=8)   :: temp(MAX_POINTS),rate(MAX_POINTS)
real (KIND=8)   :: weight(MAX_POINTS)
integer :: N
integer :: plevel

common /printlevel/ plevel
common /pfitvar/ temp,rate,weight,N

PRIVATE
PUBLIC  rotate

CONTAINS
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****is* transform/rotate
!* FUNCTION
!* This subroutine is designed to take in a unfitted set of data (temp+rate)
!* and attempt to transform it to look like another set that has been fitted.
!* INPUTS
!* infile-the name of the file read in by getparm. Used in rotate to print logs.
!* NOTES
!* The criteria for looks like is now chi**2. It could be whatever else.
!* 
!* These logs are:
!* selection.log-tells which data is chosen, what the chi**2 was, and which alteration of R.
!* infile//.R-Records each alteration of R.
!* infile//.chi-Records the chi**2 for each alteration of R.
!* infile//.chistep-Records the contribution of each point to the chi**2.
!* 
!* All of these logs open in the 'append' position.
!* 
!* It appears that a can store between k*7 and (k+1)*7-1, k integer, without disrupting the program.
!* OUTPUT
!* besta-The best set of cof found by rotate.
!* best_no_o_a-Number of elements in besta.
!* SOURCE

 SUBROUTINE rotate(besta, best_no_o_a, infile)

   IMPLICIT NONE

   INTEGER, dimension(2), parameter :: loc_o_x=69
   INTEGER, parameter :: max_tries=50
   REAL (KIND=8), parameter :: p=0.9, x=5.0
   REAL (KIND=8), parameter :: underflow_val=0.0
   REAL (KIND=8), INTENT(OUT), dimension(MAX_A) :: besta
   INTEGER, INTENT(OUT):: best_no_o_a
   REAL (KIND=8), dimension(max_match_a+1) :: tempa
   CHARACTER (len=20) :: goodness_method='chisquared'
   INTEGER, dimension(2) :: in2, best_loc
   INTEGER  :: i, j, k, flow, flow_set=0, no_o_a
   REAL (KIND=8) :: best, goodness, alt_R, best_R, R
   REAL (KIND=8), dimension(2,no_o_pts) :: temporary
   CHARACTER (len=100) :: infile   
   !REAL (KIND=8), dimension(max_match_a+1,no_o_files), parameter :: a
   !REAL (KIND=8), dimension(2,no_o_pts,no_o_files), parameter :: data1

   INCLUDE 'match.inc'

   best=huge(x)
   goodness_method=goodness_method(1:len_trim(goodness_method))

   CALL find_index1(x, temp, in2)

!REWORK anything which deals with the choosing of initial parameters for parallel Marquardt.
!   if(in2(1) .Eq. -1 .And. in2(2) .Eq. -1)then
!      PRINT'(A,G,A/)', 'Failure in rotate. ',x,' is not in input data. Trying random parameters.'
!      initial_run=.false.
!      return
!   end if

!Of course this block is solely for printing logs.
!!$   open(R_unit,file='/home/sharp/gp/selection/'//infile(1:len_trim(infile))//'.R')
!!$   open(chi_unit,file='/home/sharp/gp/selection/'//infile(1:len_trim(infile))//'.chi')
!!$   open(chistep_unit,file='/home/sharp/gp/selection/'//infile(1:len_trim(infile))//'.chistep')
!!$   write(R_unit, *) infile
!!$   write(chi_unit, *) infile

!matchset_i-this loop performs the alterations and goodness of fit checks for each set in match.inc.
   matchset_i: do i=1,no_o_files

      no_o_a=a(max_match_a+1,i)

      CALL vert_scaleprime(data1(2,loc_o_x(1),i),(rate(in2(2))+rate(in2(1) ) )*0.5, data1, a, i)

      R=R0(loc_o_x, in2, x, data1, i)

!At this point in the loop, nothing has overflown yet.
      flow=0

!R_alteration-This loop alters set i and checks goodness. The magic happens here...
      R_alteration: do j=1,max_tries

         alt_R=R*((-p)**(j-1))

!More log printing. These print labels sos alls yous has to do is C-s for Run'whatever'Alt'whatever'. 
!!$         write(R_unit, '(/2(A,I2)/)')'Run',i,'Alt',j
!!$         write(chi_unit, '(/2(A,I2)/)')'Run',i,'Alt',j
!!$         write(R_unit,'(F)')alt_R

!matchset_alteration-this alters data1 and checks for overflows.
         matchset_alteration: do k=1,no_o_pts
            temporary(1,k)=data1(1,k,i)
            temporary(2,k)=exp(alt_R*(data1(1,k,i)-x) )*data1(2,k,i)
            if(alt_R*(data1(1,k,i)-x) .GE. log(huge(x)) )then
               print *, 'Overflow while applying R.(subroutine rotate)'
               flow=flow+1
               exit
            else if(temporary(2,k) .EQ. underflow_val)then
               print *, 'Underflow while applying R.(subroutine rotate)'
               flow=flow+1
               exit
            end if
         end do matchset_alteration

         CALL array_trim(a, i, no_o_a, tempa)
         goodness=chi_squared(temp, rate, temporary, no_o_a,  tempa, i, j)

!!$         write(chi_unit, '(/G/)') goodness !More log printing.

         if(goodness .LT. best)then
            do k=1,no_o_a
               besta(k)=a(k,i)
            end do
            best_loc(1)=i
            best_loc(2)=j
            best_no_o_a=no_o_a
            best_R=alt_R
            best=goodness
         else if(flow .Eq. j-1 .And. j .Ne. 1)then
!If this alteration is the only one that hasn't overflown, just say its the best.
            do k=1,no_o_a
               besta(k)=a(k,i)
            end do 
            best_no_o_a=no_o_a
            best_R=alt_R           
         else if(flow .Eq. max_tries)then
!If all alterations have overflown, mark that, and on to the next set.
            flow_set=flow_set+1
         end if
      end do R_alteration
   end do matchset_i

!Next two blocks are? You guessed it! LOG PRINTING STUFF!
!!$   close(chi_unit)
!!$   close(R_unit)
!!$   close(chistep_unit)

!!$   open(999, file='/home/sharp/gp/selection/selection.log', position='append')
!!$   write(999, *) infile
!!$   write(999,'(tr5,A,I2)')'besti=',best_loc(1)
!!$   write(999,'(tr5,A,I2)')'bestj=',best_loc(2)
!!$   write(999,'(tr5,A,F)')'bestR=',best_R
!!$   write(999,'(tr5,A,F)')'best chi**2=',best
!!$   close(999)

   if(flow_set .Eq. no_o_files)then
!If all sets over or underflow, find some way to get alternate parameters.
      if(plevel .Ge. 1)&
         print '(A/)','Flow errors in all transformed data. Using random parameters (subroutine rotate).'
      initial_run=.false.
   else if(flow_set .Eq. 11)then
      if(plevel .Ge. 1) print'(A/)', 'Coefficients of only non-over(under)flowing set returned.'
   else
   if(plevel .Ge. 1)print'(3A,I0,A,G)','Overall best ',goodness_method,' out of ',12-flow_set,' sets=',best
      if(plevel .Ge. 3)print'(A,G/)', 'Overall best R=',best_R
   end if

!After all that hard work, we return the best set here. Everything's boiled down to this.
   do i=1,best_no_o_a/7
      besta(1+7*(i-1))=besta(1+7*(i-1))-best_R*x
      besta(5+7*(i-1))=besta(5+7*(i-1))+best_R
   end do

 END SUBROUTINE rotate
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****is* transform/vert_scaleprime
!* FUNCTION
!* Takes data1. Scales it so that data1=data2 at a given point.
!* NOTES
!* The prime only has to do with the fact that it takes a rank 3 instead of the rank 2.
!* INPUTS
!* y1, y2-These are the ordinates for which data1 and data2 will match.
!*  data-This is the data to be scaled to better match the other set.
!*  dim-The dimension in data which contains the temp and rate data.
!* OUTPUT
!* data-After is has been scaled, it is passed back to the main program.
!*  a-This contains the coefficients that correspond to data. They will have been altered.
!* SOURCE

 SUBROUTINE vert_scaleprime(y1, y2, data, a, dim)

   IMPLICIT NONE

   INTEGER :: i
   INTEGER, INTENT(IN) :: dim
   REAL (KIND=8), INTENT(IN) :: y1, y2
   REAL (KIND=8) :: k
   REAL (KIND=8), INTENT(INOUT), DIMENSION(:,:,:) :: data
   REAL (KIND=8), INTENT(INOUT), DIMENSION(:,:) :: a

   k=y2/y1

   DO i=1,no_o_pts
      data(2,i, dim)=k*data(2,i,dim)
   END DO

   DO i=1,ubound(a,1)/7
      a(1+7*(i-1),dim)=a(1+7*(i-1),dim)+log(k)
   END DO

 END SUBROUTINE vert_scaleprime
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****is* transform/find_index1
!* NOTES
!* find_index1 is find_index for a rank 1 array. See find_index for more details.
!* SOURCE

 SUBROUTINE find_index1(x, data, in)

   IMPLICIT NONE

   INTEGER :: i
   INTEGER, INTENT(OUT), DIMENSION(2) :: in
   REAL (KIND=8), INTENT(IN) :: x
   REAL (KIND=8), INTENT(IN), DIMENSION(:) :: data

   in(2)=-1; in(1)=-1

   IF (x .LT. data(1) .OR. x .GT. data(N)) THEN
      return
   END IF

   DO i=1,N
      IF (x .Eq. data(i)) THEN
         in(1)=i; in(2)=i
         EXIT
      ELSE IF (x .GT. data(i) .AND. x .LT. data(i+1)) THEN
         in(1)=i; in(2)=i+1
         EXIT
      END IF     
   END DO

 END SUBROUTINE find_index1
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****if* transform/chi_squared
!* FUNCTION
!* Finds chi**2 based on Poisson distribution where expected holds the expected values.
!* INPUTS
!* expectedx, expectedy-These hold data2.
!*  actual-Holds data1.
!*  a-parameters of data1
!*  no_o_a-number of parameters in a
!*  k, l-these correspond to i and j respectively. Used in printing out chi_squared data.
!* OUTPUT
!* chi_squared
!* SOURCE

 REAL FUNCTION chi_squared(expectedx, expectedy, actual, no_o_a,  a, k, l)

   IMPLICIT NONE

   REAL (kind=8), dimension(:), INTENT(in):: expectedx, expectedy
   REAL (kind=8), dimension(:,:), INTENT(in) :: actual
   INTEGER, INTENT(in) :: no_o_a, k, l
   REAL (kind=8), dimension(:), INTENT(in) :: a
   REAL (kind=8) :: increase, RATEEEEE
   INTEGER, dimension(2) :: in
   INTEGER :: i, flow

   chi_squared=0.0;flow=0

!!$   write(chi_unit,*)'ExpectedX/ExpectedY/Actual/Increase'
!!$   write(chistep_unit,'(/2(A,I2)/)')'run',k,'alt',l

   do i=1,N
      if (abs(expectedx(i)-actual(1,i)) .GT. 0.0000001) then
         ! If the elements in each array don't line up, find out where the one corresponding to
         ! expectedx(i) is.
         CALL find_index(expectedx(i), actual, in)

         if(in(1) .Eq. -1 .Or. in(2) .Eq. -1)then
! The only reason increase is square rooted was to check for overflows.
            ! If the value searched for is not in array actual, calculate it.
            increase=sqrt((expectedy(i)-ratevalue(expectedx(i),a,no_o_a) )**2/expectedy(i) )
            RATEEEEE=ratevalue(expectedx(i),a, no_o_a)! Used only in printing chisquared data.
!!$            write(chi_unit,*)expectedx(i),expectedy(i),RATEEEEE,increase**2,'!!!!!'

         else
            ! Otherwise, use what you find.
            increase=sqrt((expectedy(i)-0.5*(actual(2,in(1) )+actual(2,in(2) ) ) )**2/expectedy(i) )
!!$            write(chi_unit,*)expectedx(i),expectedy(i),0.5*(actual(2,in(1) )+actual(2,in(2) ) ),increase**2
         end if

! Check for overflows.
         if (increase .Ge. sqrt(huge(increase)) )then
            flow=1
            exit
         end if

! Increase is squared to counteract the square rootng earlier.
         chi_squared=chi_squared+increase**2
!!$         write(chistep_unit,'(F,I2)')chi_squared,flow

      else
         increase=sqrt((expectedy(i)-actual(2,i) )**2/expectedy(i) )
!!$         write(chi_unit,*)expectedx(i),expectedy(i),actual(2,i),increase**2

         if (increase .Ge. sqrt(huge(increase)) )then
            flow=1
            exit
         end if

         chi_squared=chi_squared+increase**2
!!$         write(chistep_unit,*)chi_squared,flow
      end if
   end do

! If ANY overflows occur, chi**2 becomes massive.
   if (flow .EQ. 1) then
      if(plevel .Ge. 3)print *,'Overflow in chi squared. (function chi_squared)'
      chi_squared=huge(increase)
   end if

!!$   write(chistep_unit,'(A,F,I2)')'final=',chi_squared,flow

 END FUNCTION chi_squared
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****if* transform/R0
!* FUNCTION
!* Calculates initial R by solving d/dT9(rate1(T9))=d/dT9(exp(R*T9-x)*rate2(T9)) for R.
!* NOTES
!* The data for the unfitted graph (data2) is given through the module variables temp and rate.
!*  Make sure x is in data1!!!
!*  Probably not a good idea to use this if data1 and data2 have very different derivatives at x.
!*  Possibly not a good idea to use this if data1 .Ne. data2 at x.
!* INPUTS
!* in1, in2-The indices bracketing x in data1 and data2.
!*  x-The point where the curves should match.
!*  data1-The data to be altered to fit data2.
!*  dim-Where to look for the proper data in data1, since data1 holds everything from match.inc.
!* OUTPUT
!* R0-The value of R which make the slope of data1 and data2 equal at x. 
!* SOURCE
 REAL FUNCTION R0(in1, in2, x, data1, dim)

   IMPLICIT NONE

   integer, INTENT(in):: dim
   integer, DIMENSION(:), INTENT(in) :: in1, in2
   real (kind=8), INTENT(in) :: x
   real (kind=8) :: Dx1, Dx2, Dy2
   real (kind=8), DIMENSION(:,:,:), INTENT(in) :: data1

! If x is the last data point in a set, use the backward derivative, otherwise, forwards.
   if(in1(1) .Eq. no_o_pts .Or. in2(1) .Eq. N)then
      Dx1=x-data1(1,in1(1)-1,dim)
      Dx2=(temp(in2(1) )+temp(in2(2) ) )*0.5-temp(in2(1)-1)
      Dy2=(rate(in2(1) )+rate(in2(2) ) )*0.5-rate(in2(1)-1)
      R0=log(data1(2,in1(1)-1,dim)/(data1(2,in1(1),dim)-Dx1/Dx2*Dy2) )/Dx1
   else
      Dx1=data1(1,in1(1)+1,dim)-x
      Dx2=temp(in2(2)+1)-(temp(in2(1) )+temp(in2(2) ) )*0.5
      Dy2=rate(in2(2)+1)-(rate(in2(1) )+rate(in2(2) ) )*0.5
      R0=log((Dx1/Dx2*Dy2+data1(2,in1(1),dim ) )/data1(2,in1(1)+1,dim) )/Dx1
   end if

 END FUNCTION R0
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****if* transform/ratevalue
!* FUNCTION
!* Calculates the rate at a ceratin temperature.
!* INPUTS
!* T9-Temperature
!*  a-Parameters
!*  no_o_a-Number of parameters.
!* OUTPUT
!* ratevalue-rate(T9)
!* SOURCE
 REAL FUNCTION ratevalue(T9,a,no_o_a)

   IMPLICIT NONE

   REAL (kind=8), INTENT(in) :: T9
   REAL (kind=8), dimension(:), INTENT(in) :: a
   INTEGER, INTENT(in):: no_o_a
   INTEGER :: j

   ratevalue=0.0

   DO j=1,no_o_a/7
      ratevalue=ratevalue+exp(a(1+7*(j-1))+&
               &a(2+7*(j-1))/T9+&
               &T9**(-one3)*a(3+7*(j-1))+&
               &T9**(one3)*a(4+7*(j-1))+&
               &T9*a(5+7*(j-1))+&
               &T9**(five3)*a(6+7*(j-1))+&
               &a(7+7*(j-1))*log(T9) )

   END DO

 END FUNCTION ratevalue
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****is* transform/array_trim
!* FUNCTION
!* To trim an array down a rank.
!* INPUTS
!* array-The rank 2 array to be trimmed to rank 1.
!* dim-Which dimension of array is to be kept.
!* no_o_a
!* OUTPUT
!* array_out-Rank 1 array corresponding to the dim dimension of array.
!* SOURCE
 SUBROUTINE array_trim(array, dim, no_o_a, array_out)

   INTEGER :: i
   INTEGER, INTENT(in) :: dim
   REAL (kind=8), INTENT(in), dimension(:,:) :: array
   INTEGER, INTENT(in) :: no_o_a
   REAL (kind=8), INTENT(out), dimension(no_o_a) :: array_out

   do i=1,no_o_a
      array_out(i)=array(i,dim)
   end do

 END SUBROUTINE array_trim
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****is* transform/find_index
!* FUNCTION
!* To find the indices bracketing a point in an array.
!* INPUTS
!* x-The point to be found in data.
!* data-The array in which we wish to find the index of x.
!* NOTES
!* If x isn't in data, in will have the value -1.
!* The fail condition (point not in set) works only if the indices of data >= 0
!* OUTPUT
!* in-The array in contains either the index where x is held, or the indices on either
!*    side of where x should be, if x is not actually a element of the array, but bounded
!*    by elements of the array.
!* SOURCE
 SUBROUTINE find_index(x, data, in)

   integer :: i
   integer, INTENT(OUT), DIMENSION(:) :: in
   real (KIND=8), INTENT(IN) :: x
   real (KIND=8), INTENT(IN), DIMENSION(:,:) :: data

   in(2)=-1; in(1)=-1

   if(x .Lt. data(1,1) .Or. x .Gt. data(1,no_o_pts))then
      return
   end if

   do i=1,ubound(data,2)

      if (abs(x-data(1,i)) .Lt. 0.000001)then
         in(1)=i; in(2)=i
         exit
      else if (x .Gt. data(1,i) .And. x .Lt. data(1,i+1))then
         in(1)=i; in(2)=i+1
         exit
      end if     
   end do

 END SUBROUTINE find_index
!***

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****iv* transform/intial_run
!* FUNCTION
!* Was designed to act as a mask when calling getranset, but now is not really used. Each of the three
!* elements corresponded to parameters 1-7, 8-14, and 15-21. The reason for only using 21 is
!* 21 is the max number of parameters in match.inc. getranset was modified to use the parameters given by
!* rotate before any others, based on the truth of the element of initial_value which corresponded
!* to set of 7 just added by smartfit.
!* NOTES
!* The conditions in rotate which cause initial_run to become false are still valid concerns.
!***
!****iw* transform/data2
!* NOTES
!* temp+rate=data2. When I say data2, I mean collectively those two arrays.
!***
!****iw* transform/run
!* NOTES
!* The procedures corresponding to each rate from match.inc
!* There will be no_o_files runs each time rotate is called.
!***
!****iw* transform/runs
!* NOTES
!* The procedures corresponding to each rate from match.inc
!* There will be no_o_files runs each time rotate is called.
!***
!****iw* transform/alteration
!* NOTES
!* For each run, R0 is called. R0 is then multiplied by -p max_tries-1 times, one -p ata a time.
!* Each one of these multiplications is an alteration.
!* There will be max_tries alterations for each run.
!***
!****iw* transform/alterations
!* NOTES
!* For each run, R0 is called. R0 is then multiplied by -p max_tries-1 times, one -p ata a time.
!* Each one of these multiplications is an alteration.
!* There will be max_tries alterations for each run.
!***
!****iv* transform/temp
!* FUNCTION
!* It's the temp used in getparm (as the common block should imply).
!* NOTES
!***
!****iv* transform/rate
!* FUNCTION
!* It's the rate used in getparm (as the common block should imply).
!* NOTES
!***
!****ic* transform/MAX_POINTS
!* FUNCTION
!* This is the same used in getparm. Make sure they match!
!***
!****ic* transform/MAX_A
!* FUNCTION
!* This is the same used in getparm. Make sure they match!
!***
!****ic* transform/no_o_pts
!* FUNCTION
!* This constant holds the number of points in the files included in match.dat.
!* All of the data in match.dat came from the infrastructure and has the same temperature intervals
!* as given on the table of rate vs. temperature.
!***
!****ic* transform/no_o_files
!* FUNCTION
!* This constant holds the number of rates in match.inc. Do not forget to change it when adding rates.
!***
!****ic* transform/max_match_a
!* FUNCTION
!* No parameter set in match.inc has more parameters than this.
!***
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****iv* rotate/besta
!* FUNCTION
!* Best set of a found by rotate.
!***
!****iv* rotate/data1
!* FUNCTION
!* Holds all no_o_files rates from match.inc. Parameters corresponding to each rate are stored in a.
!***
!****iv* rotate/a
!* FUNCTION
!* Holds the parameters of the sample rates from match.inc.
!***
!****iv* rotate/infile
!* FUNCTION
!* Used in log printing. Simply names output log files (except selection.log)
!***
!****iv* rotate/temporary
!* FUNCTION
!* Stores the temp and rate of the current run that has been altered.
!***
!****iv* rotate/R
!* FUNCTION
!* Stores value from R0.
!***
!****iv* rotate/best_R
!* FUNCTION
!* best alteration of R of fit up to that point.
!***
!****iv* rotate/alt_R
!* FUNCTION
!* Stores R for each alteration. R is modified by multiplying it by integral powers of -p.
!***
!****iv* rotate/goodness
!* FUNCTION
!* current goodness of fit, in this case chi**2.
!***
!****iv* rotate/best
!* FUNCTION
!* best goodness of fit up to that point.
!***
!****iv* rotate/no_o_a
!* FUNCTION
!* no_o_a=number of parameters that correspond to a certain run.
!***
!****iv* rotate/flow_set
!* FUNCTION
!* flow=(number of runs where every alteration has overflows)
!***
!****iv* rotate/flow
!* FUNCTION
!* flow=(number of alterations with overflows)
!***
!****iv* rotate/best_no_o_a
!* FUNCTION
!* Number of parameters in besta.
!***
!****iv* rotate/tempa
!* FUNCTION
!* Holds the parameter set for the current set from match.inc (the set given by i).
!* NOTES
!* The extra element above max_match_a holds the number of parameters in the set. 
!***
!****iv* rotate/goodness_method
!* FUNCTION
!* Used when printing out best goodness. Made it a variable so it could be changed easily if needed.
!***
!****iv* rotate/best_loc
!* FUNCTION
!* Holds the indices that bracket x in data2.
!***
!****iv* rotate/in2
!* FUNCTION
!* Used in printing selection.log. Tells which run and alteration were the best. 
!***
!****iv* rotate/i
!* FUNCTION
!* Index variable. Used mostly to denote the run.
!***
!****iv* rotate/j
!* FUNCTION
!* Index variable. Used mostly to denote the alteration.
!***
!****iv* rotate/k
!* FUNCTION
!* Index variable.
!***
!****ic* rotate/loc_o_x
!* FUNCTION
!* This is j of data1(1,j,k) equals the other constant x.
!* NOTES
!* To ensure that x is always at 69, use data with the same temperature intervals as the data from the
!* infrastructure, you know, the kind you get when you use the reaction rate plotting interface.
!***
!****ic* rotate/max_tries
!* FUNCTION
!* Max number of different R's tried on 1 set from match.inc. 
!***
!****ic* rotate/p
!* FUNCTION
!* Factor by which R is multiplied. Each time R is multiplied by -p, I call that an alteration.
!* That might be good terminology to know.
!***
!****ic* rotate/x
!* FUNCTION
!* At x, the set to be fit and the set it is compared against will have equal rates.
!* NOTES
!* x=5.0 now so as to avoid over and underflow errors. It's right in the middle of a decade,
!* so that means it will be less likely to affect the exponent than a value at the ends of a decade.
!* See the rotation formula.
!***
!****ic* rotate/underflow_val
!* FUNCTION
!* The value of underflow_val is what the computer rounds to when underflows occur.
!***

 END MODULE transform
