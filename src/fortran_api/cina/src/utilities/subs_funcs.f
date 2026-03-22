* Version 1.0
* This file contains general-purpose functions used by several programs
* The functions and subroutines in this file are:
*   integer FUNCTION   char2int (tmps, l)
*   real*8  FUNCTION   char2dbl (tmps, l)
*           SUBROUTINE printhelp
*           SUBROUTINE makeparm(meth, a_num, a)
*           SUBROUTINE scaleparm(temp,rate,N,a,parm_set,log_mval)
*           SUBROUTINE rounda(a,a_num,plevel)
*   real*8  FUNCTION   evalsub(a,temp,a_set,log_mval)
*   real*8  FUNCTION   evalrate(a,a_num,temp,log_mval)
*   real*8  FUNCTION   maxperdiff(N,temp,rate,a,a_num,log_mval)
*   real*8  FUNCTION   chisquared(N,temp,rate,sig,a,a_num,log_mval)
*   real*8  FUNCTION   dchi(temp,rate,sig,a,a_num,log_mval)
*   real    FUNCTION   pfit(n,x)
*           SUBROUTINE fitmarq(temp,rate,weight,N,a,ia,a_num,maxiter,
*                              maxfail,chistop,perstop,plevel,chkparm)
*           SUBROUTINE fitint(tnew,rnew,startpoint,endpoint,chisq)
*           SUBROUTINE parmcheck(a_num,a,maxvalue)
*           SUBROUTINE parmcheck2(a_num,a,maxvalue)
*           SUBROUTINE makebackup(fileunit,filename,a,a_num,chisq,iter)
*           SUBROUTINE getparm_exit(stat, desc, len, exitfile)

******************************************************************************

c     ****if* subs_funcs.f/sub_ver
c     FUNCTION
*Provides version number.
c     SOURCE

      SUBROUTINE sub_ver

      implicit none
      character(LEN=20) SUB_FUN_VER
      common       /SUBVER/ SUB_FUN_VER
c      integer lnblnk
c      external lnblnk

      SUB_FUN_VER = '$Revision: 1.1.1.1 $'
      SUB_FUN_VER = SUB_FUN_VER(12:LEN_TRIM(SUB_FUN_VER)-2)

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/char2int
c FUNCTION
* Function that converts a string with the number of characters equal to l
* into an integer 
* 
* Warning:  this function does not check for integer overflows 
* or one character strings and the first character must be 
* a non-zero digit
*     SOURCE

      integer FUNCTION char2int (tmps, l)

      implicit none
      integer c,d,l
      character(LEN=100) tmps

      c = 1
      char2int = 0

* Count how many characters are in the number
      do while ((ichar(tmps(c:c)) .GE. 48) .AND. 
     c          (ichar(tmps(c:c)) .LE. 57))
         c = c + 1
      end do

      do d = 1, c-1
         char2int = char2int + (ichar(tmps(d:d)) - 48) * 10**(c-d-1)
      end do

      return
      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/char2dbl
c     FUNCTION
* Function that converts a string with the number of characters equal to l
* into a real*8 value
*
* Warning:  this function does not check for floating overflows and the
*           first character must be a digit, decimal point, or negative
*           sign
c     SOURCE

      real(KIND=8) FUNCTION char2dbl (tmps, l)
         
      implicit none
      integer c,d,e,l
      character(LEN=100) tmps
         
      c = 1
      d = 0
      e = 1
      char2dbl = 0d0
         
* Is the first character a negative sign?
      if (tmps(c:c) .EQ. '-') then
            e = -1   
            c = c + 1
      end if

* Is the first character a positive sign?
      if (tmps(c:c) .EQ. '+') c = c + 1

* Get the numbers before the decimal point
      do while ((ichar(tmps(c:c)) .GE. 48) .AND.
     c          (ichar(tmps(c:c)) .LE. 57))
         char2dbl = char2dbl * 10d0 + (ichar(tmps(c:c)) - 48)
         c = c + 1
      end do
      
* Is the next character a decimal point?
      if (tmps(c:c) .EQ. '.') then
         d = 1
         do while ((ichar(tmps(c+d:c+d)) .GE. 48) .AND.
     c             (ichar(tmps(c+d:c+d)) .LE. 57))
            char2dbl = char2dbl + (ichar(tmps(c+d:c+d)) - 48) * 1d-1**d
            d = d + 1
         end do
      end if
      c = c + d

* Make the negative sign take effect
      char2dbl = e * char2dbl

* Is the next character an exponent indicator?
      if ((tmps(c:c) .EQ. 'e').OR.(tmps(c:c) .EQ. 'E').OR.
     c    (tmps(c:c) .EQ. 'd').OR.(tmps(c:c) .EQ. 'D')) then
* If so, get the value and put it into d
         c = c + 1
         d = 0
         e = 1
* Is there a negative sign in the exponent?
         if (tmps(c:c) .EQ. '-') then
            e = -1
            c = c + 1
         end if

* Is there a positive sign in the exponent?
         if (tmps(c:c) .EQ. '+') c = c + 1

         do while ((ichar(tmps(c:c)) .GE. 48) .AND.
     c             (ichar(tmps(c:c)) .LE. 57))
            d = d * 10d0 + (ichar(tmps(c:c)) - 48)
            c = c + 1
         end do
         char2dbl = char2dbl * 1.0d1 ** (d * e)
      end if
            
      return
      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/printhelp
c     FUNCTION
* Subroutine that prints a short help screen
c     SOURCE

      SUBROUTINE printhelp

      implicit none

               print *,'getparm - Get REACLIB parameters for reaction ',
     c                 'rate vs temperature data.'
               print *,'Usage:'
               print *,'   getparm [-v | -h | --help]'
               print *,'   getparm [options] input_file [options] ',
     c                    '[output_file] [options]'
               print *,'Options (separated by spaces):'
               print '(A,T40,A)',' -cf FILE    Config file',
     c               '-h   Help'
               print '(A,T40,A)',' -bf FILE    Backup file',
     c               '-v   Version'
               print '(A,T40,A)',' -ef FILE    Exit status file',
     c               '-pX  Print level'
               print '(A,T40,A)',' -sf FILE    Parameter file',
     c               '-t   Faster solution'
               print '(A,T40,A)',' -b  COUNT   Initial parmeter sets',
     c               '-n   NACRE input file'   
               print '(A,T40,A)',' -pi COUNT   Print interval',
     c               '-o   Simple output file'
               print '(A,T40,A)',' -si COUNT   Save interval',
     c               '-ov  Overwrite output file'
               print '(A,T40,A)',' -m  VALUE   Maximum value',
     c               '-sX  Start parameter method'
               print '(A,T40,A)',' -c  VALUE   Chisquared value',
     c               '-p   Disable parameter checking'
               print '(A,T40,A)',' -e  VALUE   Percent error value',
     c               '-g   Genetic algorithm'  
               print '(A,T40,A)',' -f  COUNT   Consecutive failures',
     c               '-l   Marquardt method'   
               print '(A,T40,A)',' -i  COUNT   Maximum iterations',
     c               ' '   
               print '(A,T40,A)',' -r  COUNT   Reset alamda interval',
     c               ' '   
               print '(A,T40,A)',' -id STRING  ID string',
     c               ' '   
               print *,' '
               print *,'Read manual.txt for more information.'
      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/makeparm
c     FUNCTION
* Subroutine that gets starting parameters
*     SOURCE

      SUBROUTINE makeparm(meth, a_num, a)

      implicit none
      integer  meth,a_num,i
      real(KIND=8)   a(49)

* Make sure parameters start with a value of 0
      do i=1,a_num
         a(i) = 0d0
      end do

      if (meth .EQ. 1) then
* parameters are from n14pg.out
         a(1) =  0.304327E+02
         a(2) = -0.110428E-01
         a(3) = -0.126163E+02
         a(4) = -0.191439E+02
         a(5) =  0.337040E+01
         a(6) = -0.394119E+00
         a(7) =  0.360513E+01

         a(1) =  0.221646e+02
         a(2) = -0.329980e-02
         a(3) = -0.783923e+01
         a(4) = -0.215310e+01
         a(5) =  0.133483e+00
         a(6) = -0.732414e-02
         a(7) =  0.201780e+00

      else if (meth .EQ. 2) then
* parameters are from li6pg
         a( 1) =  0.155301E+02
         a( 2) = -0.307304E-03
         a( 3) = -0.824739E+01
         a( 4) = -0.221811E+01
         a( 5) =  0.303129E+00
         a( 6) = -0.197142E-01
         a( 7) = -0.285797E+00
         continue

      end if
      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/scaleparm
c     FUNCTION
*     Subroutine that scales a(1) so that the fit equals the midpoint of the
*     interval

*     N is the points to scale to, parm_set is the parameter set to scale
*     SOURCE

      SUBROUTINE scaleparm(temp,rate,N,a,parm_set,log_mval)

      implicit none
      integer N,parm_set,i,j,k,plevel
      real(KIND=8)  temp(99999),rate(99999),a(49)
      real(KIND=8)  tscale,rscale,log_mval,tmpr

      real(KIND=8)  evalsub,evalrate
      external evalsub,evalrate
      common        /printlevel/ plevel
      
*     get temp and rate at midpoint of interval
      tscale = temp(N)
      rscale = rate(N)

*     k is the amount to add to i to account for different parm sets
      k = (parm_set - 1)*7

*     If there are more than 7 parameters, make rscale the residual rate
      if (parm_set .GT. 1) then
         tmpr = evalrate(a,k,temp,log_mval)
         if (tmpr .LT. 0d0) then
            if (plevel .GE. 1) then
               print *,'WARNING: Unable to scale parameters, ',
     c              'too extreme.'
            end if
            return
         end if
         rscale = rscale - tmpr
      end if

*     print *,'tscale=',tscale
*     print *,'rscale=',rscale
*     print *,'k=',k
*     print *,'Rstart=',evalsub(a,tscale,parm_set,log_mval)

*     a1scaled = ln( Rscaled * e^a1 / Rstart )

      tmpr = evalsub(a,tscale,parm_set,log_mval)
      if (tmpr .LT. 0) then
         if (plevel .GE. 1) then
            print *,'WARNING: unable to scale parameters, too extreme'
         end if
      else
         a(1+k) = dlog( rscale * dexp(a(1+k)) / tmpr)
      end if

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/rounda
c     FUNCTION
* Subroutine that rounds the parameters to 6 significant digits
c     SOURCE

      SUBROUTINE rounda(n,a,a_num,plevel)

      implicit none
      integer i,j,plevel,a_num,n
      real(KIND=8)  a(49)

      if (plevel .GE. 3) print '(A,I2,A)','Rounding parameters to ',n,
     c                         ' significant figures.'

      do i = 1, a_num
*         print '(A, E)', 'Before: ', a(i)

*        j is the order or exponent of a parameter
         if (a(i) .EQ. 0.0) then
            j = 1
         else
            j = dint( dlog10( dabs(a(i)) ))
         end if

         if (dabs(a(i)) .lt. 1.0)  j = j - 1

         a(i) = dnint(a(i) * 10d0 ** ((n-1)-j)) / 10d0 ** ((n-1)-j)
*         print '(A, E, I, I)', 'After:  ', a(i), j, n
      end do

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/roundr
c     FUNCTION
*     Subroutine that rounds a number to n digits
c     SOURCE

      SUBROUTINE roundr(n,tmpr)

      implicit none
      integer j,n
      real(KIND=8)  tmpr

*      print *,'Before tmpr=',tmpr

*     j is the order or exponent of the number
      if (tmpr .EQ. 0.0) then
         j = 1
      else
         j = dint( dlog10( dabs(tmpr) ))
      end if

      if (dabs(tmpr) .lt. 1.0)  j = j - 1

      tmpr = dnint(tmpr * 10d0 ** ((n-1)-j)) / 10d0 ** ((n-1)-j)

*      print *,'After tmpr=',tmpr

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/evalsub
c     FUNCTION
* To evaluate a rate for a single parameter set
* a is the parameters, temp is the value to evaluate the rate for
* a_set is 1 for 1st parm set, 2 for 2nd parm set, etc.
*
c     WARNING
* This is only part of the reaction rate, use evalrate to 
* get the full reaction rate
c     SOURCE

      real(KIND=8) FUNCTION evalsub(a,temp,a_set,log_mval)

      implicit none
      real(KIND=8)        log_mval

      integer a_set,k
      real(KIND=8)  temp,a(49)

      k = (a_set - 1) * 7

      evalsub =  
     c            a(1+k) 
     c          + a(2+k) / temp
     c          + a(3+k) / temp ** (1.0d0 / 3.0d0)
     c          + a(4+k) * temp ** (1.0d0 / 3.0d0)
     c          + a(5+k) * temp
     c          + a(6+k) * temp ** (5.0d0 / 3.0d0)
     c          + a(7+k) * dlog(temp)

* Prevent float overflow by limiting evalsub to maxvalue
      if (evalsub .GT. log_mval) then
         evalsub = -1d0
         return
      end if

      evalsub = dexp(evalsub)
      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/evalrate
c      FUNCTION
*To evaluate a rate for a temp
c     SOURCE

      real(KIND=8) FUNCTION evalrate(a,a_num,temp,log_mval)

      implicit none
      external evalsub
      integer  a_num,j
      real(KIND=8)   a(49),temp,tmpr,evalsub,log_mval

      evalrate = 0d0
      do j = 1, a_num/7
         tmpr = evalsub(a,temp,j,log_mval)
         if (tmpr .LT. 0d0) then
            evalrate = -1d0
            return
         end if
         evalrate = evalrate + tmpr
      end do

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/maxperdiff
c     FUNCTION
*To find the maximum percent difference
c     SOURCE

      real(KIND=8) FUNCTION maxperdiff(N,temp,rate,a,a_num,log_mval)

      implicit none
      external evalrate
      integer  a_num,i,N,plevel
      real(KIND=8)  a(49),temp(99999),rate(99999),diff,evalrate,log_mval
      real(KIND=8)   tmpr
      common        /printlevel/ plevel

      maxperdiff = -1.0

      do i = 1, N
         tmpr = evalrate(a,a_num,temp(i),log_mval)
         if (tmpr .LT. 0d0) then
            if (plevel .GE. 1) then
               print *,'WARNING: overflow prevented in maxperdiff'
            end if
            maxperdiff = -dexp(log_mval)
            return
         end if
         diff = (tmpr - rate(i)) / rate(i)
         maxperdiff = max(maxperdiff, dabs(diff))
      end do

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/chisquared
c     FUNCTION
*To find chi-squared between the fit and measured rate
c     SOURCE

      real(KIND=8) FUNCTION chisquared(N,temp,rate,sig,a,a_num,log_mval)

      implicit none
      external dchi
      integer  a_num,i,N,plevel
      real(KIND=8)   a(49),temp(99999),rate(99999),sig(99999),dchi
      real(KIND=8)   log_mval,tmpr
      common        /printlevel/ plevel

      chisquared = 0.0d0

      do i = 1, N
         tmpr = dchi(temp(i),rate(i),sig(i),a,a_num,log_mval)
         if (tmpr .LE. 0d0) then
            if (plevel .GE. 1) then
               print *,'WARNING: unable to calculate chisquared.'
            end if
            chisquared = -dexp(log_mval)
         else
            chisquared = chisquared + tmpr
         end if
      end do

      chisquared = chisquared/N

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/dchi
c     FUNCTION
*To find the contribution of a point to the chisquared
c     SOURCE

      real(KIND=8) FUNCTION dchi(temp,rate,sig,a,a_num,log_mval)

      implicit none
      external evalrate
      integer  a_num,i,plevel
      real(KIND=8)   a(49),temp,rate,sig,evalrate,log_mval,tmpr
      common        /printlevel/ plevel

      tmpr = evalrate(a,a_num,temp,log_mval)
      if (tmpr .LT. 0) then
         if (plevel .GE. 1) then
            print *,'WARNING: overflow prevented in dchi'
         end if
         dchi = -dexp(log_mval)
         return
      end if
      dchi = ((rate-tmpr)/sig) ** 2

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/pfit
c      FUNCTION
*To determine the level of fit for pikaia genetic algorithm
c     SOURCE

      real FUNCTION pfit(d,x)

      implicit none
      integer d,N,i,A_SCALE
      parameter (A_SCALE = 1000)
      real x(d)
      real(KIND=8) log_mval,temp(99999),rate(99999),sig(99999),a(49)
      real(KIND=8) chisquared,tmpr
      external chisquared
      common /pfitvar/ temp,rate,sig,N
      common /logmval/ log_mval

      pfit = 0

      do i = 1,d
         a(i) = (x(i) - 0.5) * A_SCALE
*         print *,a(i)
      end do

*      print *,N,d,log_mval,temp(1),rate(1),sig(1)
      tmpr = dabs(chisquared(N,temp,rate,sig,a,d,log_mval))
*      print *,'chisquared = ',tmpr
      if (tmpr .GT. 0) pfit = 1 / tmpr

      if (pfit .GT. 1) pfit = 1.0

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/fitmarq
c     FUNCTION
* Subroutine to fit a rate with Marquardt routine
c     SOURCE

      SUBROUTINE fitmarq(temp,rate,weight,N,a,ia,a_num,
     c             maxiter,maxfail,chistop,perstop,plevel,chkparm)

      implicit  none
      integer   MAX_A,MAX_POINTS
*      parameter (MAX_A = 49)

      integer   maxiter,N,ia(MAX_A),a_num,maxfail,plevel,chkparm
      real(KIND=8)    temp(MAX_POINTS),rate(MAX_POINTS)
      real(KIND=8)    weight(MAX_POINTS),a(MAX_A)
      real(KIND=8)    chistop,perstop

      integer   i,j
      real(KIND=8)    max_dyda,covar(MAX_A,MAX_A),alpha(MAX_A,MAX_A)
      real(KIND=8)    alamda,tmpr

      integer       pcount,scount
      real(KIND=8)        maxvalue,chisq,log_mval,resetval
      common        /mfitvar/ chisq,resetval,maxvalue,
     c                        pcount,scount 
      common        /logmval/ log_mval
      common        /maxsize/ MAX_A,MAX_POINTS

      real(KIND=8)    maxperdiff
      external  maxperdiff

      max_dyda = maxvalue
      alamda = -1.0

* Call mrqmin so it can initialize
      call mrqmin(temp,rate,weight,N,a,ia,a_num,covar,alpha,
     c            MAX_A,chisq,alamda,max_dyda,chkparm)
     
* Check if starting parameters are too extreme
      if (abs(max_dyda - 1.0d0).lt.0.000001) then
         if (plevel .GE. 1) print '(A,A)','getparm ERROR: ',
     c                   'starting parameters are too extreme.'
*         call getparm_exit(10,'Starting parameters are too extreme',
*     c                      35,exitfile)
         return
      end if

      if (plevel .GE. 2) print '(A,I6,A,G11.4,A,G10.3)',
     c  '  Iteration: ',0,'    Chisquared:',chisq,'    alamda: ',alamda
               
* j is the number of consecutive failures
      j = 0
* tmpr holds alamda for the previous iteration, used to test for failure
      tmpr = alamda

* Main loop
      do i = 1, maxiter
         max_dyda = maxvalue
         call mrqmin(temp,rate,weight,N,a,ia,a_num,covar,alpha,
     c               MAX_A,chisq,alamda,max_dyda,chkparm)
            
* Print the status to the screen
         if ((mod(i,pcount) .eq. 0) .AND. (plevel .GE. 2)) then
            print '(A,I6,A,G11.4,A,G10.3)','  Iteration: ',i,
     c            '    Chisquared:',chisq,'    alamda: ',alamda
         end if
     
* Make a backup file
         if (mod(i,scount) .eq. 0)
     c      call makebackup(15,a,a_num,chisq,i)

* If it failed to reduce chisquared, increment j and check if greater
* than max_fail.  If chisquared lessened, set j=0 and loop again
         if (alamda .gt. tmpr) then
            j = j+1
            if ((j .ge. maxfail)) then
               if (plevel .GE. 2) then
                  print '(A,I6,A,G11.4,A,G10.3)','  Iteration: ',
     c            i,'    Chisquared:',chisq,'    alamda: ',alamda
                  print *,'Reached max number of consecutive ',   
     c                 'failures, exiting early.'
               end if
               return
            end if
         else
            j = 0 
         end if
         tmpr = alamda

* Exit early if chisq is less than max_chisq
         if ((chisq .lt. chistop)) then
            if (plevel .GE. 2) then
               print '(A,I6,A,G11.4,A,G10.3)','  Iteration: ',
     c         i,'    Chisquared:',chisq,'    alamda: ',alamda
               print *,'Chi-squared is low enough to exit early.'
            end if
            return
         end if

* Check if the max percent error is under perstop
* since this takes a long time to compute, only calculate it every
* 100 iterations
         if (mod(i,100) .eq. 0) then
            if (dabs(maxperdiff(N,temp,rate,a,a_num,log_mval)) .LT.
     c          perstop * 0.01) then
                if (plevel .GE. 2) then
                   print '(A,I6,A,G11.4,A,G10.3)','  Iteration: ',
     c          i,'    Chisquared:',chisq,'    alamda: ',alamda
                   print *,'Maximum percent difference is low ', 
     c                     'enough to exit early.'
                end if
                return
            end if
         end if
               
* Check if lamda needs to be reset to 0.001 
         if (alamda .GE. resetval) then
            alamda = -1d0
            tmpr = 0.001
            j = 0
         end if
      end do

      if (plevel .GE. 2) then
         print *,'Maximum number of iterations reached.'
      end if

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/fitint
c     FUNCTION
*     Subroutine to fit an interval of the rate with a simpler function that
*     won't cause low and high temp problems.  This is used for adding a point to
*     the normal data set to prevent low and high temp problems.  Given a new 
*     temp (tnew which is usually 0.001) this routine finds the rate (rnew) 
*     to add.  If an error occurs, rnew = -1d0
c     SOURCE

      SUBROUTINE fitint(tnew,rnew,startpoint,endpoint,chisq,maxvalue)

      implicit none
      integer       MA
      real(KIND=8)        RMIN,RMAX
      parameter     (MA = 49)
      parameter     (RMIN = 1d-150)
      parameter     (RMAX = 1d200)

      integer       startpoint,endpoint,i,j,ndata,ia(MA),tmpi
      real(KIND=8)        tnew,rnew,chisq,maxvalue,max_dyda
      real(KIND=8)        x(99999),y(99999),sig(99999),a(MA)
      real(KIND=8)        covar(MA,MA),alpha(MA,MA),alamda,tmpr

      integer       funcs,plevel,N
      real(KIND=8)        temp(99999),rate(99999),weight(99999)
      common        /printlevel/ plevel
      common        /pfitvar/    temp,rate,weight,N
      common        /funcsvar/   funcs

*     Tell mrqcof to fit to function #2 (minfunc)
      funcs = 1

*     Get x, y, and sig
      ndata = endpoint - startpoint + 1
      do i = 1, ndata
         x(i) = temp(i - 1 + startpoint)
         y(i) = rate(i - 1 + startpoint)
         sig(i) = weight(i - 1 + startpoint)
      end do

*     Initialize variables
      do i = 1, MA
         do j = 1, MA
            covar(i,j) = 0d0
            alpha(i,j) = 0d0
         end do
      end do
      do i = 1, MA
         a(i) = 0d0
         ia(i) = -1
      end do
      a(1) = 1d-3
      a(2) = -1d-2
      a(3) = -1d0
      a(4) = 1d-3
      alamda = -1d0
      max_dyda = maxvalue
      chisq = 0d0

*     Call mrqmin so it can initialize
      call mrqmin(x,y,sig,ndata,a,ia,4,covar,alpha,MA,chisq,
     c     alamda,max_dyda,0)

*     Check if starting parameters are too extreme
      if (abs(max_dyda - 1.0d0).lt.0.000001) then
         if (plevel .GE. 1) print '(A,A)','fitint ERROR: ',
     c        'starting parameters are too extreme.'
*     call getparm_exit(10,'Starting parameters are too extreme',
*     c                      35,exitfile)
         rnew = -1d0
         return
      end if

      if (plevel .GE. 3) print '(A,I6,A,G11.4,A,G10.3)',
     c   '  Iteration: ',0,'    Chisquared:',chisq,'    alamda: ',alamda
      
*     j is the number of consecutive failures
      j = 0
*     tmpr holds alamda for the previous iteration, used to test for failure
      tmpr = alamda

*     Main loop
      do i = 1, 5000
         max_dyda = maxvalue
         if (plevel .GE. 4) print *,i
         call mrqmin(x,y,sig,ndata,a,ia,4,covar,alpha,MA,chisq,
     c        alamda,max_dyda,0)

         tmpi = 100
         if (plevel .GE. 4) tmpi = 1
* Print the status to the screen
         if ((mod(i,tmpi) .eq. 0) .AND. (plevel .GE. 3)) then
            print '(A,I6,A,G11.4,A,G10.3)','  Iteration: ',i,
     c            '    Chisquared:',chisq,'    alamda: ',alamda
         end if
     
         if (plevel .GE. 4) print *,(a(tmpi),tmpi=1,4)

* If it failed to reduce chisquared, increment j and check if greater
* than max_fail.  If chisquared lessened, set j=0 and loop again
         if (alamda .gt. tmpr) then
            j = j + 1
            if ((j .ge. 50)) then
               if (plevel .GE. 3) then
                  print '(A,I6,A,G11.4,A,G10.3)','  Iteration: ',
     c            i,'    Chisquared:',chisq,'    alamda: ',alamda
                  print *,'Reached max number of consecutive ',   
     c                 'failures, exiting early.'
               end if
               goto 1000
            end if
         else
            j = 0 
         end if
         tmpr = alamda

* Exit early if chisq is less than 10
         if ((chisq .lt. 10d0)) then
            if (plevel .GE. 3) then
               print '(A,I6,A,G11.4,A,G10.3)','  Iteration: ',
     c         i,'    Chisquared:',chisq,'    alamda: ',alamda
               print *,'Chi-squared is low enough to exit early.'
            end if
            goto 1000
         end if
      end do

      if (plevel .GE. 3) then
         print *,'Maximum number of iterations reached.'
      end if

* evaluate rate at tnew
 1000 rnew = dexp(a(1)+a(2)*(tnew ** a(3))+a(4)*dlog(tnew))
      if (rnew .LT. RMIN) rnew = RMIN
      if (rnew .GT. RMAX) rnew = RMAX

*     Tell mrqcof to fit to function #1 (functn)
      funcs = 0

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/parmcheck
c     FUNCTION
*     Subroutine to fit with parameter checking
c     SOURCE

      SUBROUTINE parmcheck(a_num,a,maxvalue)

      implicit none
      integer       MAX_A
      parameter     (MAX_A = 49)

      real(KIND=8)        evalsub,evalrate,maxperdiff
      external      evalsub,evalrate,maxperdiff

      integer       plevel,runfast,maxiter,maxfail
      integer       N,i,j,k,intsep(99999),ia(49),a_num,m,E
      real(KIND=8)        perstop,chistop,tadd(2),radd(2),chisq
      real(KIND=8)        rate(99999),temp(99999),weight(99999)
      real(KIND=8)        a(49),agood(49),pgood,log_mval,maxvalue
      real(KIND=8)        tmpra(99999),tmpr,tmpra2(99999),tmpra3(99999)
      
      common        /logmval/ log_mval
      common        /pfitvar/ temp,rate,weight,N
      common        /printlevel/ plevel
      common        /parmchko/ perstop,chistop,runfast,
     c     maxiter,maxfail

c$$$*     Calculate derivative of log10(rate)
c$$$      do i = 1, N-1
c$$$         tmpra(i) = (dlog10(rate(i+1))-dlog10(rate(i)))
c$$$     c        / (dlog10(temp(i+1))-dlog10(temp(i)))
c$$$      end do
c$$$
c$$$*     Calculate second derivative and record the point index if positive
c$$$      j = 1
c$$$*     j is the next empty storage element in intsep
c$$$      do i = 1, N-2
c$$$         tmpr = (tmpra(i+1)-tmpra(i)) / 
c$$$     c        (dlog10(temp(i+1))-dlog10(temp(i)))
c$$$         if ((tmpr .GT. 0) .AND. (i .GT. 1)) then
c$$$            intsep(j) = i
c$$$*     print *,'i=',i,' tmpr=',tmpr
c$$$            j = j + 1
c$$$         end if
c$$$      end do
c$$$*     add the end point to the interval
c$$$      intsep(j) = N
c$$$
c$$$*     print out the number of intervals
c$$$      if (plevel .GE. 2) then
c$$$         k = 0
c$$$         if (intsep(1) .GT. 1) k = k + 1
c$$$         do i = 2, j
c$$$            if (intsep(i) .GT. intsep(i-1) + 1) k = k + 1
c$$$         end do
c$$$         print '(A,I6,A)','Reaction rate is separated into ',
c$$$     c        k,' intervals.'
c$$$         print *,'Trying to fit with 7 parameters . . .'
c$$$      end if
c$$$
c$$$*     scale the first parameter set
c$$$      call scaleparm(temp,rate,1,a,1,log_mval)
c$$$
*     Find values to add if needed to prevent low and high temp problems
      tadd(1) = 1d-2
      tadd(2) = 1d1
      radd(1) = 0d0
      radd(2) = 0d0

      if (temp(1) .GT. tadd(1)) then
         i = N / 4
         if (i .LT. 6) i = 6
         call fitint(tadd(1),radd(1),1,i,chisq,maxvalue)
      else
         radd(1) = rate(1)
      end if

      if (temp(N) .LT. tadd(2)) then
         i = N / 4
         if (i .LT. 6) i = 6
         call fitint(tadd(2),radd(2),N-i,N,chisq,maxvalue)
      else
         radd(2) = rate(N)
      end if

*     initialize variables
      do i = 1, MAX_A
         ia(i) = 1
         agood(i) = a(i)
      end do
      pgood = dabs(maxperdiff(N,temp,rate,a,a_num,log_mval))
      a_num = 7
      i = 1
*     i is the interval number
      m = 0
      E = 0

*     loop to fit every interval
      do while (intsep(i) .GT. 0)

         if (plevel .GE. 4) print *,'parmcheck: 1'

*     copy variables into temp variables
         do k = 1, intsep(i)
            tmpra(k) = temp(k)
            tmpra2(k) = rate(k)
            tmpra3(k) = weight(k)
         end do
         E = 0

*     add a point at 0.001 if it doesn't exist
*     if ((tmpra(1) .GT. tadd(1)) .AND. (a_num .GT. 7)) then
*     E = 1
*     k = intsep(i) + E
*     tmpra(k) = tadd(1)
*     tmpra2(k) = radd(1)
*     tmpra3(k) = tmpra2(k) * 0.001

*     check to see if this is a bad point
*     if (a(2) .GT. 0d0) E = 0
*     if (tmpra2(k) .GT. rate(1)) E = 0

*     if ((plevel .GE. 1) .AND. (E .EQ. 0)) then
*     print *,'WARNING: Check for low temp problems'
*     end if

*     if (tmpra2(k) .LE. 1d-150) E = 0

*     if ((plevel .GE. 2) .AND. (E .EQ. 1)) then
*     print *,'Added ',tmpra(k),tmpra2(k)
*     end if
*     end if

*     add a point at 10 if it doesn't exist
*     if ((tmpra(intsep(i)) .LT. tadd(2)) .AND. (a_num .GT. 7)) then
*     j = E
*     E = E + 1
*     k = intsep(i) + E
*     tmpra(k) = tadd(2)
*     tmpra2(k) = radd(2)
*     tmpra3(k) = tmpra2(k) * 0.001

*     check to see if this is a bad point
*     if (tmpra2(k) .GE. maxvalue) E = j
*     if (a(6) .GT. 0d0) E = j
*     if (tmpra2(k) .LT. rate(N)) E = j

*     if ((plevel .GE. 1) .AND. (E .EQ. j)) then
*     print *,'WARNING: Check for high temp problems'
*     end if

*     if ((plevel .GE. 2) .AND. (E .GT. j)) then
*     print *,'Added ',tmpra(k),tmpra2(k)
*     end if
*     end if
*     E = 0

*     fit this interval
         call fitmarq(tmpra, tmpra2, tmpra3, intsep(i)+E, a,
     c        ia, a_num, 10000, 30*a_num, 0d0, 0d0, plevel-1, 0)

         if (plevel .GE. 4) print *,'parmcheck: 2'

         tmpr = dabs(maxperdiff(intsep(i),tmpra,
     c        tmpra2,a,a_num,log_mval))
*     if fit isn't good enough, try to fit again
         if ((perstop .LE. tmpr*100.0) .AND. 
     c        (runfast .EQ. 0)) then
            if (plevel .GE. 2) then
               print *,'Trying to fit again . . .'
            end if

            do k = 1, a_num
               ia(k) = 1
            end do

            call fitmarq(tmpra,tmpra2,tmpra3,intsep(i)+E,a,ia,
     c           a_num,int(maxiter*(a_num**2)*.02),
     c           maxfail,chistop,perstop,plevel,0)

            if (plevel .GE. 4) print *,'parmcheck: 3'

            tmpr = dabs(maxperdiff(intsep(i),tmpra,
     c           tmpra2,a,a_num,log_mval))
         end if

*     print status of fit
         if (plevel .GE. 2) then
            print '(A,I6,A,I6,A,G10.3,A)','  Points ',1,
     c           ' to',intsep(i),' fit to ',tmpr*100, '%'
            print *,'a= ',(a(k),k=1,a_num)
         end if

         if (perstop .GT. tmpr*100.0) then
            do k = 1, a_num
               agood(k) = a(k)
            end do
            pgood = tmpr
*     add more points and try to fit
            i = i + 1
            do while (intsep(i) .EQ. intsep(i-1) + 1)
               i = i + 1
            end do
         else 
*     if the first interval did not fit well enough
*     print *,'m=',m, ' i=',i
*     if (m .EQ. 1) then
*     print *,'Did not fit the first interval'
*     goto 900
*     end if
            if (m .EQ. i) then
               if (plevel .GE. 4) print *,'parmcheck: 4'

               if (plevel .GE. 2) then
                  print *,'Adding parameter set did not help ',
     c                 'enough.  Allow getparm to fit longer.'
               end if
               if (pgood .LT. tmpr) then
                  a_num = a_num - 7
                  do k = 1, a_num
                     a(k) = agood(k)
                  end do
               end if
               goto 900
            end if

*     add a parameter set
            if (a_num .GE. 21) then
               if (plevel .GE. 2) then
                  print '(A,A,I2)','The maximum number of ',
     c                 'parameters allowed is ',MAX_A
               end if
               goto 900
            end if

            do k = 1, a_num
               ia(k) = 0
            end do

*     do k = 1, 7
*     a(k+a_num) = a(k+a_num-7)
*     end do
*     a(a_num + 1) = a(a_num + 1) / 2

*     parameters are from n14pg
c$$$  a(1+a_num) =  0.529563E+02
c$$$  a(2+a_num) =  0.486554E+00
c$$$  a(3+a_num) = -0.913778E+02
c$$$  a(4+a_num) =  0.474007E+02
c$$$  a(5+a_num) =  0.671334E+00
c$$$  a(6+a_num) = -0.280711E+00
c$$$  a(7+a_num) = -0.430376E+02
c$$$  
c$$$  a(1+a_num) = 0.209412e+03
c$$$  a(2+a_num) = -0.705043e+01
c$$$  a(3+a_num) = 0.173982e+03
c$$$  a(4+a_num) = -0.398988e+03
c$$$  a(5+a_num) = 0.350364e+02
c$$$  a(6+a_num) = -0.266760e+01
c$$$  a(7+a_num) = 0.155921e+03

*     parameters are from li7pg
            a(1+a_num) =  0.529563E+02
            a(2+a_num) =  0.486554E+00
            a(3+a_num) = -0.913778E+02
            a(4+a_num) =  0.474007E+02
            a(5+a_num) =  0.671334E+00
            a(6+a_num) = -0.280711E+00
            a(7+a_num) = -0.430376E+02

            a_num = a_num + 7

*     scale the additional parameter set
            if (i .GT. 1) then
               k = intsep(i-1)
            else
               k = 1
            end if
            call scaleparm(temp,rate,k,a,a_num/7,log_mval)

            if (plevel .GE. 2) then
               print '(A,I2,A)',' Trying to fit with ',
     c              a_num,' parameters . . .'
            end if
*     m stores the interval that didn't fit well
            m = i

         end if
      end do

*     if more than 7 parameters were used, fit the whole rate
 900  if (a_num .GT. 7) then

         if (plevel .GE. 4) print *,'parmcheck: 5'

         do i = 1, a_num
*     a(i) = agood(i)
            ia(i) = 1
         end do

         if (plevel .GE. 2) then
            print *,'Final fit with all data points'
         end if
         call fitmarq(temp,rate,weight,N,a,ia,a_num,maxiter,
     c        maxfail,chistop,perstop,plevel,0)
      end if

*     Check if there is a low temp problem, if so set i .NE. 0
      i = 0
      if (temp(1) .GT. tadd(1)) then
         tmpr = evalrate(a,a_num,temp(1)*0.99,log_mval)
         if (tmpr .LT. 0d0) then
            if (plevel .GE. 1) then
               print *,'WARNING: Could not check for low temp problem.'
            end if
         end if

         if (tmpr .GT. rate(1)) i = 1
         do j = 1, a_num/7
            if (a(2+(j-1)*7) .GT. 0) i = 1
         end do
      end if

      j = 0
      if (temp(N) .LT. tadd(2)) then
         do k = 1, a_num/7
            if (a(6+(k-1)*7) .GT. 0) j = -1
         end do
      end if
      if (j .NE. 0) i = i + 2

      if (i .NE. 0) then
         if (plevel .GE. 4) print *,'parmcheck: 6'

         radd(1) = evalsub(a,tadd(1),1,log_mval)

*     Generate new data set
         do j = 1, N
            tmpra(j) = temp(j)
            tmpra2(j) = rate(j)
            tmpra3(j) = weight(j)
         end do
         
         E = 0
         if (MOD(i,2) .EQ. 1) then
            E = 1
            if (radd(1) .LT. 0d0) then
               E = 0
               if (plevel .GE. 1) then
                  print *,'WARNING: Could not correct low temp ',
     c                 'problem at temp=',tadd(1)
               end if
            else
               tmpra(j+1) = tadd(1)
               tmpra2(j+1) = radd(1)
               tmpra3(j+1) = radd(1) * 0.005
               if (plevel .GE. 2) then
                  print *,'Low temp problem detected. Correcting . . .'
                  print *,'Added point to data set: ',tadd(1),radd(1)
               end if
            end if
         end if

         if (i .GT. 1) then
            E = E + 1
            tmpra(j+E) = tadd(2)
            tmpra2(j+E) = radd(2)
            tmpra3(j+E) = radd(2) * 0.005
            if (plevel .GE. 2) then
               print *,'High temp problem detected. Correcting . . .'
               print *,'Added point to data set: ',tadd(2),radd(2)
            end if
         end if

         do j = 1, a_num
            ia(j) = 1
         end do

         if (plevel .GE. 4) print *,'parmcheck: 7'

         call fitmarq(tmpra,tmpra2,tmpra3,N+E,a,ia,a_num,maxiter,
     c        maxfail,chistop,perstop,plevel,0)

      end if
      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/parmcheck2
c     FUNCTION
*     Subroutine to fit with prevent low/high temp problems
c     SOURCE

      SUBROUTINE parmcheck2(a_num,a,maxvalue)

      implicit none
      integer       MAX_A
      parameter     (MAX_A = 49)

*      real(KIND=8)        evalsub,evalrate,maxperdiff
*      external      evalsub,evalrate,maxperdiff

      logical       loop
      integer       i,j,k,intsep(99999),a_num,ia(MAX_A)
      real(kind=8)   a(MAX_A),maxvalue,tadd(2),radd(2)
      real(kind=8)   chisq
      real(KIND=8)   temp2(99999),rate2(99999),weight2(99999)

*      integer       m,E
*      real(KIND=8)        agood(49),pgood,log_mval,tmpr

      integer        N,plevel,runfast,maxiter,maxfail
      real(kind=8)   temp(99999),rate(99999),weight(99999)
      real(KIND=8)   perstop,chistop
      
*      common        /logmval/ log_mval
      common        /pfitvar/ temp,rate,weight,N
      common        /printlevel/ plevel
      common        /parmchko/ perstop,chistop,runfast,
     c                         maxiter,maxfail

*     Check if a point at low temps needs to be added
      if (temp(1) > 1d-2) then
         tadd(1) = 1d-2
      else
         tadd(1) = -1d0
      end if

*     Check if a point at high temps needs to be added
      if (temp(N) < 10.0) then
         tadd(2) = 10d0
      else
         tadd(2) = -1d0
      end if

*     Break rate into intervals (but don't print message to screen)
      i = plevel
      plevel = 1
      CALL findint(intsep)
      plevel = i

*     If a point at low temps was added, get rate value to add
      IF (tadd(1) /= -1d0) THEN
*     Find out how many points to fit and store in j
         k = 1
         loop = .TRUE.
         DO WHILE (loop)       ! Loop until all conditions are met
            j = intsep(k)
            IF (j == 0) THEN
               loop = .FALSE.
               j = N
            END IF
            IF ((j >= 15) .AND. (temp(j) > tadd(1) * 8)) loop = .FALSE.
            k = k + 1
         END DO
*     Get value of rate at new temp value
         print *,'j=',j,' maxvalue=',maxvalue,' t=',temp(j)
         CALL fitint(tadd(1),radd(1),1,j,chisq,maxvalue)
*     If it didn't work, give up on fixing low temp problem
         IF (radd(1) == -1d0) THEN
            tadd(1) = -1d0
            WRITE(*,'(A)') 'WARNING: Could not fix low temp problem.'
         END IF
      END IF
      
*     If a point at high temps was added, get rate value to add
      IF (tadd(2) /= -1d0) THEN
*     Find out how many points to fit and store in j
*     Start from the next to last interval
         k = 0
         DO WHILE (intsep(k + 1) /= 0)
            k = k + 1
         END DO
         k = k - 1

         loop = .TRUE.
         DO WHILE (loop)       ! Loop until all conditions are met
            IF (k == 0) THEN
               loop = .FALSE.
               j = 1
            ELSE
               j = intsep(k)
            END IF
            IF (((N-j) >= 15) .AND. (temp(j) > tadd(2) / 8)) 
     c           loop = .FALSE.
            k = k - 1
         END DO
*     Get value of rate at new temp value
         CALL fitint(tadd(2),radd(2),j,N,chisq,maxvalue)
*     If it didn't work, give up on fixing high temp problem
         IF (radd(2) == -1d0) THEN
            tadd(2) = -1d0
            WRITE(*,'(A)') 'WARNING: Could not fix high temp problem.'
         END IF
      END IF
      
*     print added values to the screen
      IF (plevel .GE. 2) THEN
         IF (tadd(1) /= -1d0) THEN
            WRITE(*,'(2A,G12.4,A,G12.4)') 'Preventing low temp ',
     c           'problem by adding point ',tadd(1),', ',radd(1)
         END IF
         IF (tadd(2) /= -1d0) THEN
            WRITE(*,'(2A,G12.4,A,G12.4)') 'Preventing high temp ',
     c           'problem by adding point ',tadd(2),', ',radd(2)
         END IF
      END IF

*     Create new arrays with added values
      IF (tadd(1) /= -1d0) THEN
         temp2(1) = tadd(1)
         rate2(1) = radd(1)
         weight2(1) = 0.01 * radd(1)
         j = 1
      ELSE
         j = 0
      END IF

      DO i = 1,N
         temp2(i+j) = temp(i)
         rate2(i+j) = rate(i)
         weight2(i+j) = weight(i)
      END DO
      
      IF (tadd(2) /= -1d0) THEN
         j = j + 1
         temp2(N+j) = tadd(2)
         rate2(N+j) = radd(2)
         weight2(N+j) = 0.01 * radd(2)
      END IF

c$$$      print *,temp2(1),rate2(1)
c$$$      print *,temp2(2),rate2(2)
c$$$      print *,temp2(N+j-1),rate2(N+j-1)
c$$$      print *,temp2(N+j),rate2(N+j)

*     If some points were added, fit to them
      IF (j > 0) THEN
         IF (plevel .GE. 2) WRITE(*,*) 'Fitting with added points.'
*         print *,(a(i),i=1,a_num)
*     Initialize ia
         DO i = 1,MAX_A
            ia(i) = 1
         END DO
c$$$         CALL fitmarq(temp2,rate2,weight2,N+j,a,ia,a_num,maxiter,
c$$$     c        maxfail,chistop,perstop,plevel,0)
      END IF

      END
c     ***

******************************************************************************
c     ****if* subs_funcs.f/makebackup
c     FUNCTION
* Subroutine used to create backup file
c     SOURCE

      SUBROUTINE makebackup(fileunit,a,a_num,chisq,iter)

      implicit none
      integer       fileunit,a_num,i,iter
      real(KIND=8)        a(49),chisq
      character(LEN=100) backupfile
      common        /bfile/ backupfile

      open(unit=fileunit, file=backupfile, status='unknown')

      write(fileunit,'(I2)') a_num
      do i=1,a_num
         write(fileunit,'(E13.6)') a(i)
      end do
      write(fileunit,*) 'chi-squared = ',chisq
      write(fileunit,*) 'iteration = ',iter

      close(fileunit)

      end
c     ***

******************************************************************************
c     ****if* subs_funcs.f/getparm_exit
c     FUNCTION
* Subroutine used when exiting
* This subroutine saves the exit status in a file
c     SOURCE

      SUBROUTINE getparm_exit(stat, desc, len, exitfile)

      implicit none
      integer stat,len
      character(LEN=100) desc,exitfile

      open(unit = 20, file=exitfile)
      write(20,'(I3)') stat
      write(20,'(A)') desc(1:len)
      close(20)
      call exit(stat + 256)

      end
c     ***
