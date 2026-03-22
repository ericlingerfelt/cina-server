* Version 16.9
* Changes since 16.8
*   - None
*
* Routines for finding parameters to make a function approximate a
* tabulated set of data.

* Routines were copied from 'Numerical Recipes in FORTRAN' second edition
* ISBN: 0-521-43064-X    Authored by William H. Press, et. all

* This library was compiled by Jason Scott (jpscott@ieee.org) on 6/2002

******************************************************************************
* SUBROUTINES:
*     parm_ver  Function that gets the revision number of this file
*     functn    Function that approximates data for fitting parameters
*     minfunc   Function that approximates data for scaling parameters
*     mrqmin    Perform one iteration of Marquardt's method for finding
*               a set of parameters to fit a nonlinear function
*     mrqcof    Rearranging covariance matrix for mrqmin subroutine
*     gaussj    Find the matrix solution for mrqmin subroutine
*
******************************************************************************

******************************************************************************

c     ****if* parmfit.f/parm_ver
c     SOURCE

      SUBROUTINE parm_ver

      implicit none
      character(LEN=20) PARMFIT_VER
      common       /PARMVER/ PARMFIT_VER
c      integer lnblnk
c      external lnblnk

      PARMFIT_VER = '$Revision: 1.1.1.1 $'
      PARMFIT_VER = PARMFIT_VER(12:LEN_TRIM(PARMFIT_VER)-2)

      end
c     ***


******************************************************************************
c     ****if* parmfit.f/functn
c     FUNCTION
c      Calculate the rate for a given set of parameters.
c      Also calculates the partial deriv with respect to each cof.
c     SOURCE

      SUBROUTINE functn(x,a,y,dyda,na,max_dyda)

* Routine written by Jason Scott

      IMPLICIT  none
      INTEGER   na,i,MAX_A,tmpi,plevel,MAX_POINTS
      REAL(KIND=8)    x,y,a(MAX_A),dyda(MAX_A),tmpr,total
      REAL(KIND=8)    max_dyda
      COMMON    /printlevel/ plevel
      COMMON    /maxsize/ MAX_A,MAX_POINTS

      total=0.0

      do i=1,na/7
*        tmpi is what should be added to index for more than 7 parameters
         tmpi = (i-1)*7
         tmpr =
     c             a(1+tmpi)
     c           + a(2+tmpi)/x
     c           + a(3+tmpi)/x**(1.0d0/3.0d0)
     c           + a(4+tmpi)*x**(1.0d0/3.0d0)
     c           + a(5+tmpi)*x
     c           + a(6+tmpi)*x**(5.0d0/3.0d0)
     c           + a(7+tmpi)*dlog(x)

* Prevent float overflow by limiting tmpr(1) to a max value
         if (tmpr .GT. dlog(max_dyda)) then
            if (plevel .GE. 4) then
               print *,'functn: Floating Overflow prevented when',
     c                 ' calculating rate, extreme parameters.'
               print *,'Warning occured at temp = ',x,' and a = '
               print *,(a(tmpi), tmpi = 1,na)
            end if
            max_dyda = 1.0d0
            return
         end if

         y = dexp(tmpr)
         total = total + y

* Calculate dyda(n)
         dyda(1+tmpi)=y
         dyda(2+tmpi)=y/x
         dyda(3+tmpi)=y/x**(1.d0/3.d0)
         dyda(4+tmpi)=y*x**(1.d0/3.d0)
         dyda(5+tmpi)=y*x
         dyda(6+tmpi)=y*x**(5.d0/3.d0)
         dyda(7+tmpi)=y*dlog(x)
      end do

* Return the sum not partial sum
      y = total

      return
      END
c     ***

******************************************************************************
c     ****if* parmfit.f/minfunc
c     FUNCTION
c     SOURCE

      SUBROUTINE minfunc(x,a,y,dyda,na,max_dyda)

*     Routine written by Jason Scott

      IMPLICIT  none
      INTEGER   na,MAX_A,plevel,MAX_POINTS,i,j
      REAL(KIND=8)    x,y,a(MAX_A),dyda(MAX_A),tmpr
      REAL(KIND=8)    max_dyda
      COMMON    /printlevel/ plevel
      COMMON    /maxsize/ MAX_A,MAX_POINTS

*      if (plevel .GE. 4) print *,(a(i),i=1,4)
*      if (plevel .GE. 4) print *,x,na,max_dyda

      if (plevel .GE. 4) print *,'minfunc: 1'

      i = 0
      if (dabs(a(1)) .GT. 1d10) i = -1
      if (dabs(a(2)) .GT. 1d5) i = -1
      if (dabs(a(3)) .GT. 9.3d1) i = -1
      if (dabs(a(4)) .GT. 1d10) i = -1

*      if (plevel .GE. 4) print *,'i = ',i

      if (i .NE. 0) then
         if (plevel .GE. 4) then
            print *,'minfunc: Floating Overflow prevented when',
     c           ' calculating rate, extreme initial parameters.'
            print *,'Warning occured at temp = ',x,' and a = '
            print *,(a(j), j = 1,na)
         end if
         max_dyda = 1.0d0
         return
      end if

      y = 0d0
      tmpr = a(1)+a(2)*(x ** a(3))+a(4)*dlog(x)

      if (plevel .GE. 4) print *,'minfunc: 2'

*     Prevent float overflow by limiting tmpr to a max value
      if (tmpr .GT. dlog(max_dyda)) then
         if (plevel .GE. 4) then
            print *,'minfunc: Floating Overflow prevented when',
     c           ' calculating rate, extreme parameters.'
            print *,'Warning occured at temp = ',x,' and a = '
            print *,(a(j), j = 1,na)
         end if
         max_dyda = 1.0d0
         return
      end if

      y = dexp(tmpr)

      if (plevel .GE. 4) print *,'minfunc: 3'

*     Calculate dyda(n)
      dyda(1) = y
      dyda(2) = y * (x ** a(3))
      dyda(3) = dyda(2) * dlog(x)
      dyda(4) = y * dlog(x)

      if (plevel .GE. 4) print *,'minfunc: 4'

      end
c     ***

******************************************************************************
c     ****if* parmfit.f/mrqmin
c     FUNCTION
* Routine is discussed on page 680.
* Levenberg-Marquardt method, attempting to reduce the value of X^2 of a
* fit between a set of data points x(a:ndata),y(1:ndata) with individual
* standard deviations sig(1:ndata) and a nonlinear function dependent on
* ma coefficients a(1:ma).  The input aray ia(1:ma) indicates by zero
* entries those components that should be held fixed at their input
* values.  The program returns current best-fit values for the parameters
* a(1:ma), and X^2 = chisq.  The arrays covar(1:nca,a:nca),
* alpha(1:nca,1:nca) with physical dimension nca ( >= the number of
* fitted parameters) are used as working space during most iterations.
* Supply a subroutine funcs(x,a,yfit,dyda,ma) that evaluates the fitted
* function yfit, and its derivatives dyda with respect to the fitting
* parameters a at x.  On the first call provide an initial guess for the
* parameters a, and set alamda<0 for initialization (which then sets
* alamda=.001).  If a step succeeds chisq becomes smaller and alamda
* decreased by a factor of 10.  If a step fails alamda grows by a factor
* of 10.  You must call this routine repeatedly until convergence is 
* achieved.  Then, make one final call with alamda=0, so that 
* covar(1:ma,a:ma) returns the covariance matrix, and alpha the curvature
* matrix.  (Parameters held fixed will return zero covariances.)
c     SOURCE

      SUBROUTINE mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,nca,
     c                  chisq,alamda,max_dyda,chkparm)

      IMPLICIT none
      INTEGER  MAX_A,MAX_POINTS
      PARAMETER (MAX_A = 49)
      INTEGER  ma,nca,ndata,ia(MAX_A),chkparm
      REAL(KIND=8)   alamda,chisq,a(MAX_A),alpha(MAX_A,MAX_A)
      REAL(KIND=8)   covar(MAX_A,MAX_A),sig(MAX_POINTS),x(MAX_POINTS)
      REAL(KIND=8)   y(MAX_POINTS)
      INTEGER  j,k,l,mfit,plevel,tmpi
      REAL(KIND=8)   ochisq,atry(MAX_A),beta(MAX_A),da(MAX_A),max_dyda
      SAVE     ochisq,atry,beta,da,mfit
      COMMON   /printlevel/ plevel   
      COMMON   /maxsize/ tmpi,MAX_POINTS

* Make sure that MAX_A is the same as the global MAX_A
*      if (MAX_A .NE. tmpi) then
*         print *,'Parameter MAX_A in mrqmin subroutine in file ',
*     c           'parmfit.f is not the same as the one defined in ',
*     c           'getparm.f'
*         print *,'local MAX_A = ',MAX_A,', global MAX_A = ',tmpi
*         stop
*      end if

* Initialization.
      if (alamda.lt.0.) then
         mfit=0
         ochisq=0.0
         do j=1,MAX_A
            beta(j)=0.0
            da(j)=0.0
            atry(j)=0.0
         end do
         do j=1,ma
            if (ia(j).ne.0) mfit=mfit+1
         end do
         alamda=0.001
         call mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nca,chisq,
     c               max_dyda)
* If max_dyda is exceeded, exit
         if (abs(max_dyda - 1.0d0).lt.0.000001) then
            if (plevel .GE. 3) then
               print *,'mrqmin: Starting parameters too extreme.'
            end if
            return
         end if
         ochisq=chisq
         do j=1,ma
            atry(j)=a(j)
         end do
      end if

* Alter linearized fitting matrix, by augmenting diagonal elements.
      do j=1,mfit
         do k=1,mfit
            covar(j,k)=alpha(j,k)
         end do
         covar(j,j)=alpha(j,j)*(1.+alamda)
         da(j)=beta(j)
      end do

      if (plevel .GE. 4) print *,'mrqmin: 1'

* Matrix solution.
      call gaussj(covar,mfit,nca,da,1,1,max_dyda)

      if (plevel .GE. 4) print *,'mrqmin: 2'

* Check for error (singular matrix) and force failure in cycle
      if (abs(max_dyda - 1.0d0).lt.0.000001) then
         alamda=10.*alamda
         chisq=ochisq
         return
      end if
* Once converged, evaluate covariance matrix
      if(alamda.eq.0.) then
         call covsrt(covar,nca,ma,ia,mfit)
         return
      end if
      j=0

* Did the trial succeed
      do l=1,ma
         if(ia(l).ne.0) then
            j=j+1
            atry(l)=a(l)+da(j)
         end if
      end do

*      print *,'#1 ',(a(j), j=1,7)
*      print *,'#1 ',(atry(j), j=1,7)

      if (plevel .GE. 4) print *,'mrqmin: 3'

      call mrqcof(x,y,sig,ndata,atry,ia,ma,covar,da,nca,chisq,
     c            max_dyda)
* If max_dyda is exceeded, then force failure for this cycle
      if (abs(max_dyda - 1.0d0).lt.0.000001) then
         chisq=ochisq * 1.01
         max_dyda = 1d100
      end if

      if (plevel .GE. 4) print *,'mrqmin: 4'

* If parameter checking is on and a(2) > 0 force failure for this cycle
*      if (chkparm .NE. 0) then
*         do j=1,ma/7
*            print *,a(2 + (j-1) * 7)
*            if (a(2 + (j-1) * 7) .GT. 0) then
*               chisq = ochisq * 1.01
*               a(2 + (j-1) * 7) = -a(2 + (j-1) * 7)
*               print *, a(2 + (j-1) * 7)
*            end if
*         end do
*      end if

* Success, accept the new solution
      if(chisq.lt.ochisq) then
         alamda=0.1*alamda
         ochisq=chisq
         do j=1,mfit
            do k=1,mfit
               alpha(j,k)=covar(j,k)
            end do
            beta(j)=da(j)
         end do
         do l=1,ma
            a(l)=atry(l)
         end do

* Failure, increase alamda and return
      else
         alamda=10.*alamda
         chisq=ochisq
      end if
      return
      END
c     ***

******************************************************************************
c     ****if* parmfit.f/mrqcof
c     FUNCTION
* Routine is discussed on page 681
* Used by mrqmin to evaluate the linearized fitting matrix alpha, and
* vector beta as in (15.5.8), and calculate X^2
c     SOURCE

      SUBROUTINE mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nalp,
     c                  chisq,max_dyda)

      IMPLICIT none
      INTEGER  MAX_A,MAX_NUM
      PARAMETER(MAX_A=49)
      PARAMETER(MAX_NUM=99999)
*     MAX_A should be set equal to parameter MAX_A in file n2r.for
*     MAX_NUM should be set equal to parameter MAX_NUM in file n2r.for
      
      INTEGER  ma,nalp,ndata,ia(MAX_A)
      REAL(KIND=8)  chisq,a(MAX_A),alpha(MAX_A,MAX_A),beta(MAX_A),
     c         sig(MAX_NUM),x(MAX_NUM),y(MAX_NUM)
      INTEGER  mfit,i,j,k,l,m,plevel,funcs
      REAL(KIND=8)  dy,sig2i,wt,ymod,dyda(MAX_A),max_dyda
*      EXTERNAL funcs
      COMMON    /printlevel/ plevel   
      COMMON    /funcsvar/ funcs

      mfit=0
      do j=1,ma
         if (ia(j).ne.0) mfit=mfit+1
      end do

* Initialize (symmetric) alpha, beta
      do j=1,mfit
         do k=1,j
            alpha(j,k)=0.
         end do
         beta(j)=0.
      end do
      chisq=0.

* Summation loop over all data
      do i=1,ndata

         if (funcs .EQ. 0) call functn(x(i),a,ymod,dyda,ma,max_dyda)
         if (funcs .EQ. 1) call minfunc(x(i),a,ymod,dyda,ma,max_dyda)

      if (plevel .GE. 4) print *,' mrqcof: 1'

* Check if max_dyda exceeded
*       print *,max_dyda,x(i),ymod,y(i)
*       print *,(a(j),j=1,7)
         if (abs(max_dyda - 1.0d0).lt.0.000001) return

         sig2i=1./(sig(i)*sig(i))
         dy=y(i)-ymod
         j=0
         do l=1,ma
            if (ia(l).ne.0) then
               j=j+1
               wt=dyda(l)*sig2i
               k=0
               do m=1,l
                  if (ia(m).ne.0) then
                     k=k+1
* Check if dyda > max_dyda
                     if (dabs(dyda(m)).gt.max_dyda) then
                        if (plevel .GE. 3) then
                           print *,'mrqcof: max_dyda exceeded.  ',
     c                          'Failure in current cycle forced.'
                        end if
                        max_dyda = 1.0d0
                        return
                     end if
                     alpha(j,k)=alpha(j,k)+wt*dyda(m)
                  end if
               end do
               beta(j)=beta(j)+dy*wt
            end if
         end do
         chisq=chisq+dy*dy*sig2i
      end do

      if (plevel .GE. 4) print *,' mrqcof: 2'

* Fill in the symmetric side
      do j=2,mfit
         do k=1,j-1
            alpha(k,j)=alpha(j,k)
         end do
      end do

      return
      END
c     ***

******************************************************************************
c     ****if* parmfit.f/covsrt
c     FUNCTION
*     covsrt subroutine copied out of Numerical Recipies p. 669
*     covsrt expands the covariance matrix covar, so as to take into account 
*     parameters that are being held fixed.
c     SOURCE

      SUBROUTINE covsrt(covar,npc,ma,mfit)
      INTEGER ma,mfit,npc,ia(ma)
      REAL(KIND=8) covar(npc,npc)
      INTEGER i,j,k
      REAL(KIND=8) swap

      do i = mfit+1, ma
         do j = 1, i
            covar(i,j) = 0.
            covar(j,i) = 0.
         end do
      end do
      k = mfit
      do j = ma,1,-1
         if (ia(j).ne.0) then
            do i = 1, ma
               swap = covar(i,k)
               covar(i,k) = covar(i,j)
               covar(i,j) = swap
            end do
            do i = 1, ma
               swap = covar(k,i)
               covar(k,i) = covar(j,i)
               covar(j,i) = swap
            end do
            k = k + 1
         end if
       end do
       return
       END
c      ***

******************************************************************************
c     ****if* parmfit.f/gaussj
c     FUNCTION
* Routine is discussed on page 30
* Linear equation solution by Gauss-Jordan elimination.  a(1:n,1:n)
* is an input matrix stored in an array of physical dimensions np by np.
* b(1:n,1:m) is an input matrix containing the m right-hand side vectors,
* stored in an array of physical dimensions np by mp.  On output,
* a(1:n,1:n) is replaced by its matrix inverse, and b(1:n,1:m) is replaced
* by the corresponding set of solution vectors.  
* Parameter NMAX is the largest anticipated value of n
c     SOURCE

      SUBROUTINE gaussj(a,n,np,b,m,mp,max_dyda)

      IMPLICIT none
      INTEGER  MAX_A,MAX_POINTS
      parameter (MAX_A = 49)      
      INTEGER  m,mp,n,np
      REAL(KIND=8)  a(MAX_A,MAX_A),b(MAX_A,mp)
      INTEGER  i,icol,irow,j,k,l,ll,indxc(MAX_A),indxr(MAX_A),
     c         ipiv(MAX_A),plevel
*     Arrays ipiv, indxr, and indxc are used for bookkeeping on pivoting

      REAL(KIND=8)  big,dum,pivinv,max_dyda
      COMMON    /printlevel/ plevel   
*      COMMON    /maxsize/ MAX_A,MAX_POINTS

      do j=1,MAX_A
         ipiv(j)=0
      end do

* This is the main loop over the columns to be reduced.
      do i=1,n
         big=0.

* This is the outer loop of the search for a pivot point
         do j=1,n
            if (ipiv(j).ne.1) then
               do k=1,n
                  if (ipiv(k).eq.0) then
                     if (dabs(a(j,k)).ge.big) then
                        big=dabs(a(j,k))
                        irow=j
                        icol=k
                     end if
                  else if (ipiv(k).gt.1) then
* If singular matrix, force failure in current cycle
                     if (plevel .GE. 3) then
                        print *, 'singular matrix in gaussj:1'
                     end if
                     max_dyda = 1.0d0
                     return
                  end if
               end do
            end if
         end do
         ipiv(icol)=ipiv(icol)+1

      if (plevel .GE. 4) print *,'gaussj: 1'

* We now have the pivot element, so we interchage rows, if needed, to
* put the pivot element on the diagonal.  The columns are not physically
* interchanged, only relabeled: indxc(i), the column of the ith pivot
* element, is the ith column that is reduced, while indxr(i) is the row
* in which that pivot element was originally located.  If indxr(i) is
* not equal to indxc(i) there is an implied column interchange.  With
* this form of bookkeeping, the solution b's will end up in the correct
* order, and the inverse matrix will be scrambled by columns.
         if (irow.ne.icol) then
            do l=1,n
               dum=a(irow,l)
               a(irow,l)=a(icol,l)
               a(icol,l)=dum
            end do
            do l=1,m
               dum=b(irow,l)
               b(irow,l)=b(icol,l)
               b(icol,l)=dum
            end do
         end if

* We are now ready to divide the pivot row by the pivot element,
* located at irow and icol.
         indxr(i)=irow
         indxc(i)=icol

* If singular matrix, force failure in current cycle
         if (a(icol,icol).eq.0.) then
            if (plevel .GE. 3) then
               print *, 'singular matrix in gaussj:2  Failure forced'
            end if
            max_dyda = 1.0d0
            return
         end if

      if (plevel .GE. 4) print *,'gaussj: 2'

         pivinv=1./a(icol,icol)
         a(icol,icol)=1.
         do l=1,n
            a(icol,l)=a(icol,l)*pivinv
         end do
         do l=1,m
            b(icol,l)=b(icol,l)*pivinv
         end do
         do ll=1,n
            if (ll.ne.icol) then
               dum=a(ll,icol)
               a(ll,icol)=0.
               do l=1,n
                  a(ll,l)=a(ll,l)-a(icol,l)*dum
               end do
               do l=1,m
                  b(ll,l)=b(ll,l)-b(icol,l)*dum
               end do
            end if
         end do
      end do

* This is the end of the main loop over columns of the reduction.  It
* only remains to unscramble the solution in view of the column
* interchanges.  We do this by interchanging pairs of columns in the
* reverse order that the permutation was built up.

      if (plevel .GE. 4) print *,'gaussj: 3'

      do l=n,1,-1
         if (indxr(l).ne.indxc(l)) then
            do k=1,n
               dum=a(k,indxr(l))
               a(k,indxr(l))=a(k,indxc(l))
               a(k,indxc(l))=dum
            end do
         end if
      end do
      return
      END
c     ***
