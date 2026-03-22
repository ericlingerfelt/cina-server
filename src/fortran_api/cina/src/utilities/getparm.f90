      program getparm

! Module inclusion
      use transform !ADD

      implicit none
      character(LEN=30)  PROGRAM_VER,GETPARM_VER,PARMFIT_VER,PIKAIA_VER
      character(LEN=30) COMPILE_DATE,MANUAL_VER,SUB_FUN_VER,SMARTFIT_VER
! PROGRAM_VER is program version. GETPARM_VER is getparm.f source version
! and is usually the same as the program version
      parameter     (PROGRAM_VER = '1.0')
      parameter     (MANUAL_VER = '1.0')
      parameter     (COMPILE_DATE = '$Date: 2008/04/22 13:36:33 $')
      common        /SUBVER/ SUB_FUN_VER
      common        /PARMVER/ PARMFIT_VER
      common        /PIKAIAVER/ PIKAIA_VER
      common        /GETPARMVER/ GETPARM_VER
      common        /SFITVER/ SMARTFIT_VER

!******************************************************************************
! Variable Declarations

! Function declarations
      integer       char2int,lnblnk
      real(KIND=8)       char2dbl,maxperdiff,evalrate,evalsub,chisquared
      real(KIND=8)        dchi
      external      char2int,char2dbl,maxperdiff,evalrate,evalsub
      external      chisquared,dchi,lnblnk
      integer       time
      external      time

!  Parameters or program constants
      integer       MAX_A,MAX_POINTS,A_SCALE
      real(KIND=8)        MAX_VALUE
      parameter     (MAX_A = 49)
      parameter     (MAX_VALUE = 1.0d307)
      parameter     (MAX_POINTS = 99999)
      parameter     (A_SCALE = 1000)

!  Temp variables
      integer       i,j,k,m
      character(LEN=100) tmps
      real(KIND=8)        tmpr,tmpra(MAX_POINTS),tmpra2(MAX_POINTS)
      real(KIND=8)        tmpra3(MAX_POINTS)

!  Program variables
      character(LEN=100) configfile,infile,outfile,parmfile
      character(LEN=100) backupfile,exitfile,id
      integer       N,ia(MAX_A),intsep(MAX_POINTS),max_av,max_pointsv
      integer       intnum,E,funcs,z, olda_num!ADD
      real(KIND=8)        temp(MAX_POINTS),rate(MAX_POINTS),log_mval
      real(KIND=8)        weight(MAX_POINTS),a(MAX_A),a_start(MAX_A)
      real(KIND=8)        rate_anal,diff,diff_max,alamda,chisq,max_dyda
      real(KIND=8)    covar(MAX_A,MAX_A),alpha(MAX_A,MAX_A),agood(MAX_A)
      real(KIND=8)        pgood, olda(MAX_A), oldper, time1, time2, dtime, newper!ADD

!  Pikaia variables
      real          ctrl(12),x7(7),x14(14),x21(21),x28(28),pfit
      external      pfit

!  Option flag variables
      integer       plevel,infmt,outfmt,ovwrite,pcount,scount,fitalg
      integer       a_num,maxfail,maxiter,startparm,runfast,chkparm
      real(KIND=8)        chistop,perstop,maxvalue,resetval

!  Common variables groups
      common        /mfitvar/ chisq,resetval,maxvalue,pcount,scount
      common        /pfitvar/ temp,rate,weight,N
      common        /bfile/   backupfile
      common        /logmval/ log_mval
      common        /printlevel/ plevel
      common        /maxsize/ max_av,max_pointsv
      common        /parmchko/ perstop,chistop,runfast,maxiter,maxfail
      common        /funcsvar/ funcs

!*****************************************************************************
! Variable Descriptions

!  Parameter or program constants
!       MAX_A = Maximum number of parameters
!   MAX_VALUE = Maximum value before a floating overflow occurs
!  MAX_POINTS = Maximum number of data points
!     A_SCALE = Maximum value of parameter for pikaia genetic algorithm

!  Program variables
!  configfile = pathname of configuration file
!      infile = pathname of input or data file
!     outfile = pathname of output file
!    parmfile = pathname of input parameters file
!  backupfile = pathname of backup file
!    exitfile = pathname of exit status file
!          id = Optional identification string

!           N = number of data points
!           E = number of data points added for parm checking
!        temp = array of temperatures
!        rate = array of rate points
!      weight = individual standard deviations or weights of each data point
!           a = array of parameters
!        olda = Temp used during the call to Marquardt with parm checking.!ADD
!     a_start = array of starting parameters
!          ia = array tells which parameters can be modified
!    log_mval = natural log of maxvalue
!       chisq = chi-squared value between fit and measured
!       agood = good copy of parameters used for parameter checking
!       pgood = max percent error using agood parameters
!      intnum = number of points in an interval for parm checking
!      max_av = MAX_A; the variable is needed to pass to common block
! max_pointsv = MAX_POINTS; the variable is needed to pass to common block
!       funcs = Tells marq routine which function to fit (0 is normal)
!           z = Junk index used during the call to Marquardt with parm checking.!ADD

!    max_dyda = Max absolute value of dyda before forced failure in cycle
!      alamda = used by Marquardt fitting subroutine
!       covar = used by Marquardt fitting subroutine
!       alpha = used by Marquardt fitting subroutine

!      intsep = Index of points that separate intervals in parm checking

!   rate_anal = analytical value of function at one temperature
!    diff_max = Max difference between tabular and analytical data
!        diff = difference between tabular and analytical data

!  Option flag variables
!      plevel = Print level (0 is minimal, 3 is maximum)
!     runfast = Run fast (0 emphasizes accuracy, 1 emphasizes speed)
!       infmt = Input file format (0 is standard, 1 is NACRE)
!      outfmt = Output file format (0 is standard, 1 is parameters only)
!     ovwrite = Overwrite output file? (0 is no, 1 is yes)
!      pcount = Print status interval
!      scount = Save to file interval
!      fitalg = Fitting algorithm (0 is genetic, 1 is Marquardt)
!     chkparm = Parameter checking (0 is no, 1 is yes)
!       a_num = Number of parameters in use
!    olda_num = Temp used during the call to Marquardt with parm checking.!ADD
!     chistop = Stop when chisquared is below this value
!     perstop = Stop when max percent error is below this value
!      oldper = Temp used during the call to Marquardt with parm checking.!ADD
!     maxfail = Maximum number of consecutive failures before stopping
!     maxiter = Maximum number of iterations before stopping
!    maxvalue = Maximum value allowed for preventing floating overflows
!   startparm = Method of obtaining starting parameters
!                 (0 is from file, 1 is using method 1, 2 is using method 2)
!    resetval = Reset alamda when it exceeds this, used for Marquardt

!*****************************************************************************
! Initialize variables to defaults or 0

      do i = 1,MAX_A
         ia(i) = 0
         a(i) = 0.0
         a_start(i) = 0.0
      end do

      do i = 1,MAX_POINTS
         temp(i) = 0.0
         rate(i) = 0.0
         weight(i) = 0.0
         intsep(i) = 0
      end do

      i = 0
      j = 0
      k = 0
      tmps = 'a'
      funcs = 0

      configfile = ' '
      infile = 'a'
      outfile = 'a'
      parmfile = 'a'
      backupfile = 'getparm_backup'
      exitfile = 'getparm_exit_status'
      id = ' '
      N = 0
      E = 0
      rate_anal = 0.0
      diff = 0.0
      diff_max = 0.0
      log_mval = 0.0
      max_av = MAX_A
      max_pointsv = MAX_POINTS

      plevel = 2
      runfast = 0
      infmt = 0
      outfmt = 0
      ovwrite = 0
      pcount = 1000
      scount = 1000
      fitalg = 1
      chkparm = 1
      a_num = 7
      chistop = 1.0
      perstop = 2.0
      maxfail = 100
      maxiter = 5000
      maxvalue = 1.0d100
      startparm = 1
      resetval = 1.0d250

! Get source file revision numbers
      call sub_ver
      call pik_ver
      call parm_ver
      call sfit_ver
      GETPARM_VER = '$Revision: 1.1.1.1 $'
      GETPARM_VER = GETPARM_VER(12:LEN_TRIM(GETPARM_VER)-2)

!*****************************************************************************
! Parse command-line arguments

      call getarg(1, tmps)

! i is arg number, j is 0 until the input_file is found
      i = 1
      j = 0
      k = len(tmps)
      do while (tmps .NE. ' ')
         if (tmps(1:1) .EQ. '-') then
            if ((tmps(2:3) .EQ. 'h ').OR.(tmps(2:7) .EQ. '-help ')) then
               call printhelp
               call getparm_exit(3,'Printed help screen',19,exitfile)
            else if (tmps(2:3) .EQ. 'v ') then
               print *,' '
               print *,'getparm version ',PROGRAM_VER
               print *,'compiled on ',COMPILE_DATE(8:)
               print *,'Source file versions:'
               print *,'  getparm.f       ',GETPARM_VER
               print *,'  parmfit.f       ',PARMFIT_VER
               print *,'  pikaia.f        ',PIKAIA_VER
               print *,'  subs_funcs.f    ',SUB_FUN_VER
               print *,'  smartfit.f      ',SMARTFIT_VER
               print *,'Manual version:   ',MANUAL_VER
               print *,' '
               call getparm_exit(3,'Printed version information',27,exitfile)
            else if (tmps(2:4) .EQ. 'cf ') then
               i = i + 1
               call getarg(i, tmps) 
               configfile = tmps
            else if (tmps(2:4) .EQ. 'bf ') then
               i = i + 1
               call getarg(i, tmps) 
               backupfile = tmps
            else if (tmps(2:4) .EQ. 'ef ') then
               i = i + 1
               call getarg(i, tmps) 
               exitfile = tmps
            else if (tmps(2:4) .EQ. 'p0 ') then
               plevel = 0
            else if (tmps(2:4) .EQ. 'p1 ') then
               plevel = 1
            else if (tmps(2:4) .EQ. 'p2 ') then
               plevel = 2
            else if (tmps(2:4) .EQ. 'p3 ') then
               plevel = 3
            else if (tmps(2:4) .EQ. 'p4 ') then
               plevel = 4
            else if (tmps(2:4) .EQ. 'id ') then
               i = i + 1
               call getarg(i, tmps)
               id = tmps
            else if (tmps(2:3) .EQ. 't ') then
               runfast = 1   
            else if (tmps(2:3) .EQ. 'n ') then
               infmt = 1
            else if (tmps(2:3) .EQ. 'o ') then
               outfmt = 1
            else if (tmps(2:4) .EQ. 'ov ') then
               ovwrite = 1
            else if (tmps(2:4) .EQ. 'pi ') then
               i = i + 1
               call getarg(i, tmps)
               pcount = char2int(tmps,100)
            else if (tmps(2:4) .EQ. 'si ') then
               i = i + 1
               call getarg(i, tmps)
               scount = char2int(tmps,100)
            else if (tmps(2:3) .EQ. 'g ') then
               fitalg = 0
            else if (tmps(2:3) .EQ. 'l ') then
               fitalg = 1
            else if (tmps(2:3) .EQ. 'p ') then
               chkparm = 0
            else if (tmps(2:3) .EQ. 'b ') then
               i = i + 1
               call getarg(i, tmps)
               a_num = char2int(tmps,100) * 7
            else if (tmps(2:3) .EQ. 'c ') then
               i = i + 1
               call getarg(i, tmps)
               chistop = char2dbl(tmps,100)
            else if (tmps(2:3) .EQ. 'e ') then
               i = i + 1
               call getarg(i, tmps)
               perstop = char2dbl(tmps,100)
            else if (tmps(2:3) .EQ. 'f ') then
               i = i + 1
               call getarg(i, tmps)
               maxfail = char2int(tmps,100)
            else if (tmps(2:3) .EQ. 'i ') then
               i = i + 1
               call getarg(i, tmps)
               maxiter = char2int(tmps,100)
            else if (tmps(2:3) .EQ. 'm ') then
               i = i + 1
               call getarg(i, tmps)
               maxvalue = char2dbl(tmps,100)
            else if (tmps(2:4) .EQ. 'sf ') then
               startparm = 0
               i = i + 1
               call getarg(i, tmps) 
               parmfile = tmps
            else if (tmps(2:4) .EQ. 's1 ') then
               startparm = 1
            else if (tmps(2:4) .EQ. 's2 ') then
               startparm = 2
            else if (tmps(2:3) .EQ. 'r ') then
               i = i + 1
               call getarg(i, tmps) 
               resetval = char2dbl(tmps,100)
            else
               print *,'getparm ERROR: unknown option ',tmps(1:30)
               call getparm_exit(2,'Invalid option',14,exitfile)
            end if
         else
            if (j .EQ. 0) then
               infile = tmps
            else if (j .EQ. 1) then
               outfile = tmps
            else
               print *,'getparm ERROR: extra argument - ',tmps(1:30)
               call getparm_exit(1,'Extra argument on command line',30,exitfile)
            end if
            j = j + 1
         end if
         i = i + 1
         call getarg(i, tmps)
      end do

      if (j .EQ. 0) then
         print *,'getparm ERROR: input file must be specified'
         call printhelp
         call getparm_exit(1,'Input file not specified',24,exitfile)
      else if (j .EQ. 1) then
         k = 100
! k is the index of the last character in infile variable
         do while (infile(k:k) .EQ. ' ')
            k = k - 1
! This condition should never occur, but to be safe
            if (k .LT. 2) then
               if (plevel .GE. 1) print *,'getparm ERROR: unexpected ',&
                    'Unexpected error assigning outfile.  Contact ',&
                    'programmer.'
               call getparm_exit(4,'Unexpected error assigning outfile.'&
                    //'  Contact programmer',55,exitfile)
            end if
         end do
         outfile = infile(1:k) // '.out'
      end if

!*****************************************************************************
! Load configuration file if specified
      if (configfile(1:1) .NE. ' ') then
! The status='old' causes an error to occur if the file does not exist
         open(unit=2, file=configfile, status='old')
! Loop through file until the end, then branch to 100
         do while (1 .EQ. 1)
            read(2,'(A)', END=100, ERR=100) tmps
            if (tmps(1:7) .EQ. 'PLEVEL=') then
               plevel = char2int(tmps(8:10),3)
            else if (tmps(1:8) .EQ. 'RUNFAST=') then
               runfast = char2int(tmps(9:11),3)
            else if (tmps(1:13) .EQ. 'INPUT FORMAT=') then
               infmt = char2int(tmps(14:16),3)
            else if (tmps(1:14) .EQ. 'OUTPUT FORMAT=') then
               outfmt = char2int(tmps(15:17),3)
            else if (tmps(1:10) .EQ. 'OVERWRITE=') then
               ovwrite = char2int(tmps(11:13),3)
            else if (tmps(1:15) .EQ. 'PRINT INTERVAL=') then
               pcount = char2int(tmps(16:99),84)
            else if (tmps(1:14) .EQ. 'SAVE INTERVAL=') then
               scount = char2int(tmps(15:99),85)
            else if (tmps(1:18) .EQ. 'FITTING ALGORITHM=') then
               fitalg = char2int(tmps(19:21),3)
            else if (tmps(1:19) .EQ. 'PARAMETER CHECKING=') then
               chkparm = char2int(tmps(20:22),3)
            else if (tmps(1:16) .EQ. 'START PARM SETS=') then
               a_num = char2int(tmps(17:19),3) * 7
            else if (tmps(1:19) .EQ. 'CHISQUARED TO STOP=') then
               chistop = char2dbl(tmps(20:99),80)
            else if (tmps(1:22) .EQ. 'PERCENT ERROR TO STOP=') then
               perstop = char2dbl(tmps(23:99),77)
            else if (tmps(1:21) .EQ. 'CONSECUTIVE FAILURES=') then
               maxfail = char2int(tmps(22:99),78)
            else if (tmps(1:19) .EQ. 'MAXIMUM ITERATIONS=') then
               maxiter = char2int(tmps(20:99),80)
            else if (tmps(1:14) .EQ. 'MAXIMUM VALUE=') then
               maxvalue = char2dbl(tmps(15:99),85)
            else if (tmps(1:23) .EQ. 'START PARAMETER METHOD=') then
               startparm = char2int(tmps(24:26),3)
            else if (tmps(1:21) .EQ. 'START PARAMETER FILE=') then
               parmfile = tmps(22:99)
            else if (tmps(1:12) .EQ. 'BACKUP FILE=') then
               backupfile = tmps(13:99)
            else if (tmps(1:17) .EQ. 'EXIT STATUS FILE=') then
               exitfile = tmps(18:99)
            else if (tmps(1:23) .EQ. 'IDENTIFICATIONN STRING=') then
               id = tmps(24:99)
            else if (tmps(1:12) .EQ. 'RESET VALUE=') then
               resetval = char2dbl(tmps(13:99),87)
            end if
         end do
      end if
      close(2)

!*****************************************************************************
! Print all the input information if print level is >= 3
100   if (plevel .GE. 2) print '(A,A,T30,A40)','Identification ','String: ',id

      if (plevel .GE. 3) then
         print '(A,T30,A40)','Config file: ',configfile
         print '(A,T30,A40)','Backup file: ',backupfile
         print '(A,T30,A40)','Exit status file: ',exitfile
         print '(A,T30,A40)','Input file: ',infile
         print '(A,T30,I1)','Input file format: ',infmt
         print '(A,T30,A40)','Output file: ',outfile
         print '(A,T30,I1)','Output file format: ',outfmt
         print '(A,T30,I1)','Overwrite output file: ',ovwrite
         print '(A,T30,I1)','Starting parameter method: ',startparm
         if (startparm .EQ. 0) print '(A,T30,A40)','Parameter file: ',parmfile
         print '(A,T30,I2)','Starting parmeters: ',a_num
         print '(A,T30,I1)','Print level: ',plevel
         print '(A,T30,I1)','Run fast: ',runfast
         print '(A,T30,I6)','Print backup interval: ',scount
         print '(A,T30,I6)','Print status interval: ',pcount
         print '(A,T30,D13.5)','Maximum allowed number: ',maxvalue
         print '(A,T30,I1)','Fitting algorithm: ',fitalg
         print '(A,T30,I1)','Parameter checking: ',chkparm
         print '(A,T30,I12)','Maximum iterations: ',maxiter
         print '(A,T30,I6)','Max consecutive failures: ',maxfail
         print '(A,T30,D13.5)','Max chisquared to stop: ',chistop
         print '(A,T30,D13.5)','Max percent error to stop: ',perstop
         if (fitalg .EQ. 1) print '(A,T30,D13.5)','Reset alamda value',resetval
      end if

!*****************************************************************************
! Load standard input file

      open(unit=9, file=infile, status='old')
      N = 0
! N records the number of data points
      if (infmt .EQ. 0) then
         do while (1 .EQ. 1)
            read(9,'(A)', END=200, ERR=200) tmps
! Find out where the temperature value in the file ends
            i = 1
! Count all leading spaces
            do while(tmps(i:i) .EQ. ' ')
               i = i + 1
            end do

            j = i
! Count until you find the space separating the temp and rate
            if (i .LT. 100) then
               do while(tmps(i:i) .NE. ' ')
                  i = i + 1
               end do
            end if

! Count the leading spaces before second number
            if (i .LT. 100) then
               do while(tmps(i:i) .EQ. ' ')
                  i = i + 1
               end do
            end if

            if (i .LT. 100) then
         continue
               temp(N+1) = char2dbl(tmps(j:i-1), i - j + 1)
               rate(N+1) = char2dbl(tmps(i:99),99 - i)
               N = N + 1
            end if
         end do

!*****************************************************************************
! Load NACRE input file

      else if (infmt .EQ. 1) then
         open(unit=9, file=infile, status='old')

! Ignore first line
         read(9,*,END=150,ERR=150) temp(1)

! Get number of data points
         read(9,*,END=150,ERR=150) N
         if (plevel .GE. 3) print *,'Reading in ',N,' points in NACRE file.'

! Ignore reaction type
         read(9,'(A)',END=150,ERR=150) tmps

! Make sure 4th line is 'T9'
         read(9,'(A)',END=150,ERR=150) tmps
         if (tmps(4:5) .ne. 'T9') then
            if (plevel .GE. 1) print *,'T9 was not found in correct position.'
            goto 150
         end if

         do i = 1,N
            read(9,*,END=150,ERR=150) temp(i),tmpr,rate(i)
         end do
      end if
      goto 200
 
150   if (plevel .GE. 1)print *,'getparm ERROR: The NACRE file is not in the correct format.'
      call getparm_exit(5,'NACRE file in wrong format.',27,exitfile)

200   close(9)
      if (N .LE. 3) then
         if (plevel .GE. 1)print *,'getparm ERROR: Fewer than four rate values were found. Aborting.'
         call getparm_exit(6,'Too few data points. Aborting.',30,exitfile)
      end if

!*****************************************************************************
! Print out temp and rate values and set log_mval to it's value

      if (plevel .GE. 3) then
         do i=1,N
            print *,temp(i),rate(i)
         end do
      end if
      if (plevel .GE. 2) then
         print *,N,' rate values were found.'
      end if

      log_mval = dlog(maxvalue)

!*****************************************************************************
! Get starting parameters from file

      if (startparm .EQ. 0) then
! Load starting parameters from file
         open(unit=10, file=parmfile, status='old')
         read(10,*, END=250, ERR=250 ) a_num

! Make sure the right number of parameters was entered
         if ((a_num.lt.7).OR.(MOD(a_num,7).ne.0)) then
            print '(A,A,I2,A)','getparm ERROR: The number of parameters'&
                 ,' entered (',a_num,') must be a nonzero multiple of 7.'
         call getparm_exit(8,'Wrong number or parameters in file',34,exitfile)
      end if


         if (plevel .GE. 2) print '(A,I2,A)','Reading ',a_num,&
              ' starting parameters from file.'
         do i=1,a_num
            read(10,*, END=250, ERR=250) a(i)
         end do
         goto 300

250      if (plevel .GE. 1) print *,'getparm ERROR: parameter file ',&
              'is in the wrong format.  Read manual.txt'
         call getparm_exit(7,'Parameter file in wrong format',30,exitfile)

300      close(10)

!*****************************************************************************
! Get starting parameters automatically

      else
         if (plevel .GE. 3) print *,'Obtaining starting parameters using method transform:'
        
         CALL rotate(a, a_num, infile)!ADD
         
      end if

!*****************************************************************************
! Print starting parameters and copy them into a_start

      do i=1,a_num
         if (plevel .GE. 3) print '(A,I2,A,D)','   a(',i,') = ',a(i)

         a_start(i) = a(i)
      end do

! Calculate standard deviations
      do i=1,N
         weight(i) = 0.01 * rate(i)
! Check if weight is too small
         if (weight(i) .lt. 1.0d-152) then
            if (plevel .GE. 1) print '(A,A)','getparm ERROR: The ',&
                 'smallest rate value allowable is 1.0d-150'
            call getparm_exit(9,'Rate value is too small.',24,exitfile)
         end if
      end do

!*****************************************************************************
! Fit reaction rate using Genetic algorithm

      if (fitalg .EQ. 0) then
! Initialize pikaia random number generator
         call rninit(time())

         do i = 1, 12
            ctrl(i) = -1
         end do

!*****************************************************************************
! Run genetic algorithm with parameter checking off

         if (chkparm .EQ. 0) then

            if (a_num .EQ. 7) then
               do i = 1,7
                  x7(i) = a(i) / A_SCALE + 0.5
                  if (x7(i) .GT. 1.0) x7(i) = 1.0
                  if (x7(i) .LT. 0.0) x7(i) = 0.0
               end do

! tmpr is the value from pfit and j is the status
               call pikaia(pfit,7,ctrl,x7,tmpr,j)

               do i = 1,7
                  a(i) = (x7(i) - 0.5) * A_SCALE
               end do

! Print the results
               write(*,*) ' status: ',j
               write(*,*) '      x: ',x7
               write(*,*) '      f: ',tmpr
               write(*,20) ctrl
20             format(   '    ctrl: ',6f9.5/10x,6f9.5)

            else if (a_num .EQ. 14) then
               continue
            end if

!*****************************************************************************
! Run genetic algorithm with parameter checking on

         else
            print *,'The genetic algorithm with parameter checking ',&
                 'is not implemented.'
         end if

!*****************************************************************************
! Fit reaction rate using Levenberg-Marquardt method

      else if (fitalg .EQ. 1) then

!*****************************************************************************
! Main loop for Marquardt method with parameter checking off

         if (chkparm .EQ. 0) then

! Make vector to indicate which parameters to fit
            do i = 1,a_num
               ia(i) = 1
            end do

         !RE   open(1030, file='time.dat', status='old', position='append')!ADD
         !RE   backspace 1030!ADD
         !RE   read(1030, *) dtime!ADD
         !RE   call cpu_time(time1)!ADD

            call fitmarq(temp,rate,weight,N,a,ia,a_num,maxiter,&
                 maxfail,chistop,perstop,plevel,chkparm)

         !RE   call cpu_time(time2)!ADD
         !RE   dtime=dtime+time2-time1!ADD
         !RE   write(1030, '(6f)') dtime, time2-time1!ADD

!*****************************************************************************
! Main loop for Marquardt method with parameter checking on

         else

!!$            open(1030, file='time.dat', status='old', position='append')!ADD
!!$            backspace 1030!ADD
!!$            read(1030, *) dtime!ADD
!!$
!!$            call cpu_time(time1)!ADD

            do i = 1,a_num!ADD
               ia(i) = 1!ADD
            end do!ADD

            call smartfit(a,a_num)

            oldper=maxperdiff(N,temp,rate,a,a_num,log_mval)
            newper=oldper

            ! There is a bug where oldper is negative, if so then make it positive so that fitmarq will run
            IF (oldper < 0.0) oldper = HUGE(oldper) * 0.001

!!$            print*, infile!ADD
            print*, oldper*100.0, perstop!ADD
            do z=1,a_num
               olda(z)=a(z)
!!$               print*,'non', olda(z)
            end do
            olda_num=a_num

!RE If smartfit can't fit the data satisfactorily, they'll call in their buddy fitmarq and rotate.
            IF (oldper*100 .GT. perstop) THEN
               print '(A)','PARALLEL algorithm did not work, trying MARQUARDT'
               call fitmarq(temp,rate,weight,N,a,ia,a_num,maxiter,&!ADD
                    maxfail,chistop,perstop,plevel,chkparm)
               newper = maxperdiff(N,temp,rate,a,a_num,log_mval)!ADD
               print*, newper * 100.0, perstop
            end if

!!$            do z=1,a_num
!!$               print*,'para', a(z)
!!$            end do

            if(newper .Lt. oldper)then
               print '(A)','Using MARQUARDT method parameters instead of PARALLEL method.'
               a_num=olda_num
               do z=1,a_num
                  a(z)=olda(z)
               end do
            end if

!!$            do z=1,a_num
!!$               print*,'best', a(z)
!!$            end do!ADDE

!!$            call cpu_time(time2)!ADD
!!$            dtime=dtime+time2-time1!ADD
!!$            write(1030, '(6f)') dtime, time2-time1!ADD

!               call parmcheck2(a_num,a,maxvalue)

         end if
      end if

!*****************************************************************************
! Round final parameters to 6 significant figures

1000  call rounda(6,a,a_num,plevel)

!      do i = 1,N
!         print *,dchi(temp(i),rate(i),weight(i),a,a_num,log_mval)
!      end do

!      print *,(a(i), i=1,a_num)

!*****************************************************************************
! Create a simple output file

      if (ovwrite .EQ. 0) then
         open(unit=12, file=outfile, status='new')
      else
         open(unit=12, file=outfile, status='unknown')
      end if

      write(12,'(I2)') a_num
      do i=1,a_num
         write(12,'(E13.6)') a(i)
      end do

!*****************************************************************************
! Create the standard output file

      if (outfmt .EQ. 0) then
         do i = 1, a_num
            if (a_num .lt. 10) then
              write(12,'(A,I1,A,E13.6,A,I1,A,E13.6)') 'a_start(', i,&
                   ')  = ', a_start(i),'       a_final(',i,')  = ',a(i)
            else
              write(12,'(A,I2,A,E13.6,A,I2,A,E13.6)') 'a_start(', i,&
                   ') = ', a_start(i),'       a_final(',i,') = ',a(i)
            end if
         end do

         write(12,'(A)') ' '
         write(12,'(A,T14,A,T28,A)') 'Temp(T9)','Rate',&
              'Difference in Analytical and Tabulated Rate'

         diff_max = -1.0

         do i = 1, N
            tmpr = evalrate(a,a_num,temp(i),log_mval)
            if (tmpr .LT. 0d0) then
               diff = maxvalue
            else
               diff = (tmpr - rate(i)) / rate(i)
            end if
            diff_max = max(diff_max, dabs(diff))
            write(12,'(G10.4,T14,G10.4,T28,E14.8)')temp(i),rate(i),diff
         end do

         write(12,'(A, G14.4, A)') 'Maximum difference is ',&
              diff_max * 100, ' percent.'

!     Find new chisquared.  It changed after rounding parameters
         tmpr = chisquared(N,temp,rate,weight,a,a_num,log_mval)

         if (plevel .GE. 2) then
            print '(A,G11.4)',' Chisquared after rounding is ',tmpr
            print '(A,A,G11.4,A)',' Maximum difference between ',&
                 'analytical function and rate is ',diff_max*100,' %'

         end if

         if (plevel .GE. 2) print '(A,A,T30,A40)','Identification ','String: ',id

         write(12,'(A,T30,A)') 'Identification string: ',TRIM(id)
         write(12,'(A,T30,G)') 'Chisquared: ',tmpr
         write(12,'(A,T30,A40)') 'Config file: ',configfile
         write(12,'(A,T30,A40)') 'Input file: ',infile
         write(12,'(A,T30,I1)') 'Input file format: ',infmt
         write(12,'(A,T30,A40)') 'Output file: ',outfile
         write(12,'(A,T30,I1)') 'Output file format: ',outfmt
         write(12,'(A,T30,I1)') 'Overwrite output file: ',ovwrite
         write(12,'(A,T30,I1)') 'Starting parameter method: ',startparm
         if (startparm .EQ. 0) then
            write(12,'(A,T30,A40)') 'Parameter file: ',parmfile
         end if
         write(12,'(A,T30,I2)') 'Starting parmeters: ',a_num
         write(12,'(A,T30,I1)') 'Print level: ',plevel
         write(12,'(A,T30,I1)') 'Run fast: ',runfast
         write(12,'(A,T30,I6)') 'Print backup interval: ',scount
         write(12,'(A,T30,I6)') 'Print status interval: ',pcount
         write(12,'(A,T30,D13.5)') 'Maximum allowed number: ',maxvalue
         write(12,'(A,T30,I1)') 'Fitting algorithm: ',fitalg
         write(12,'(A,T30,I1)') 'Parameter checking: ',chkparm
         write(12,'(A,T30,I12)') 'Maximum iterations: ',maxiter
         write(12,'(A,T30,I6)') 'Max consecutive failures: ',maxfail
         write(12,'(A,T30,D13.5)') 'Max chisquared to stop: ',chistop
         write(12,'(A,T30,D13.5)') 'Max percent error to stop: ',perstop
         if (fitalg .EQ. 1) write(12,'(A,T30,D13.5)') 'Reset alamda interval',resetval

      end if
      close (12)

      call getparm_exit(0,'No error',8,exitfile)

      end

!Added use transform, replaced makeparm, jury-rigged Marquardt with parameter checking (call to smartfit).
!Added olda, olda_num, z.
!C-s search for !ADD (single line or beginning of block) or !ADDE (end of block).
