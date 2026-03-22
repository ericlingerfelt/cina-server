* This file contains routines for the smartfit method
* The functions and subroutines in this file are:
*           SUBROUTINE sfit_ver
*           SUBROUTINE getset(a,sindex,level)
*           SUBROUTINE getranset(a,level)
*           SUBROUTINE easyfit(chi,p,a,a_num,maxiter,maxfail,chistop,perstop)
*           SUBROUTINE findint(intsep)
*   INTEGER FUNCTION   checkchi
*   INTEGER FUNCTION   statfit(e,N,a_num)
*           SUBROUTINE erasestat(sstati,mstati)
*
******************************************************************************

******************************************************************************

c     ****if* smartfit.f/sfit_ver
c     FUNCTION
c      Provides the version number of smartfit.f.
c     SOURCE

      SUBROUTINE sfit_ver

      implicit none
      character(LEN=20) SMARTFIT_VER
      common       /SFITVER/ SMARTFIT_VER
c      integer lnblnk
c      external lnblnk

      SMARTFIT_VER = '$Revision: 1.1.1.1 $'
      SMARTFIT_VER = SMARTFIT_VER(12:LEN_TRIM(SMARTFIT_VER)-2)

      end
c     ***

******************************************************************************
c     ****if* smartfit.f/getset
c     FUNCTION
*     Retrieve 7 parameters from the database
*     sindex is the parm set index, and level = 1 for 1st set,
*     2 for 2nd set, etc.
*     If sindex == 0 then the number of sets is returned into level
*     Note that it does matter what level is.
c     SOURCE

      SUBROUTINE getset(a,sindex,level)

      implicit none
      integer    sindex,level,i,k,plevel
      real(KIND=8)     a(49)
      common     /printlevel/ plevel

      INCLUDE 'parmsets.inc'

*     Check if level is out of range
      if ((level .LT. 1) .OR. (level .GT. 7)) then
         if (plevel .GE. 1) then
            print *,'WARNING: level is out of range in getset'
         end if
         return
      end if

*     Check if the number of sets was requested
      if (sindex .EQ. 0) then
         if (level .GE. 2) level = R_NUM
         if (level .EQ. 1) level = NR_NUM
         return
      end if

*     Check if sindex is out of range
      if (((level .EQ. 1) .AND. (sindex .GT. NR_NUM)) .OR. 
     c     ((level .GT. 1) .AND. (sindex .GT. R_NUM))) then
         if (plevel .GE. 1) then
            print *,'WARNING: sindex is out of range in getset'
         end if
         return
      end if

      k = (level - 1) * 7
      if (level .EQ. 1) then
         do i = 1, 7
            a(i+k) = nr(sindex,i)
         end do
      else
         do i = 1, 7
            a(i+k) = r(sindex,i)
         end do
      end if
      end
c     ***

******************************************************************************
c     ****if* smartfit.f/getranset
c     FUNCTION
* Routine to get a parameter set from random and update sstat
* It sets level == 0 if all sets are used
c     SOURCE

      SUBROUTINE getranset(a,level)

      implicit none
      integer     NR
      parameter   (NR = 20)
      integer     level,sstati(NR,2),i,j,n,t,u,c,plevel
      real(KIND=8)      a(49),sstatc(NR),r,a_out(49)
      common      /SSTAT/ sstati,sstatc
      common     /printlevel/ plevel

*     Check if level is out of range
      if ((level .LT. 1) .OR. (level .GT. 7)) then
         if (plevel .GE. 1) then
            print *,'WARNING: level is out of range in getranset'
         end if
         return
      end if

      c = 0
      do while (sstati(c+1,1) .NE. 0)
         c = c + 1
         if (c+1 .GT. NR) then
* if sstati is full
            level = 0
            return
         end if
      end do
* c is the number of sets (at this level) already retrieved

      t = level
      call getset(a,0,t)
* t is the total number of parm sets available

      call random_number(r)
* n is the new set index
      n = (t-1) * r + 1
      i = n
* Check if this one is already used
* when u is 0, the value of n is already used
      u = 0
      do while (u .EQ. 0)
         u = -1
         n = n + 1
         if (n .GT. t) n = 1
         do j = 1, c
            if (sstati(j,1) .EQ. n) u = 0
         end do
         if ((n .EQ. i) .AND. (u .EQ. 0)) then
* if all sets from database are used
            level = 0
            return
         end if
      end do
* n is now a unique new parm index

* store this new index in sstati
      sstati(c+1,1) = n
      sstati(c+1,2) = 0

      call getset(a,n,level)

* save these parameters
      do i = 1, level * 7
         a_out(i) = a(i)
      end do

* fit each one for a short amount of time
* store a value in sstatc for this new random set
      call easyfit(sstatc(c+1),0,a,7,250,100,1d-3,1d-3)
*      print '(I2,7E)',c+1,(a(i),i=1,7)

* return the saved parameters
      do i = 1, level * 7
         a(i) = a_out(i)
      end do

      end
c     ***

******************************************************************************
c     ****if* smartfit.f/easyfit
c     FUNCTION
* Easy way to fit data
* p is the number of points.  If p = 0 then all points are used.
c     SOURCE

      SUBROUTINE easyfit(chi,p,a,a_num,maxiter,maxfail,chistop,perstop)

      implicit none
      integer       a_num,maxiter,maxfail,i,ia(49),p
      real(KIND=8)        chi,a(49),chistop,perstop

      integer       plevel,N,pcount,scount
      real(KIND=8)        temp(99999),rate(99999),weight(99999),chisq
      real(KIND=8)        resetval,maxvalue
      common        /mfitvar/ chisq,resetval,maxvalue,
     c                        pcount,scount
      common        /pfitvar/ temp,rate,weight,N
      common        /printlevel/ plevel

      do i = 1, a_num
         ia(i) = 1
      end do

      i = N
      if (p .GT. 0) i = p

      call fitmarq(temp,rate,weight,i,a,ia,a_num,maxiter,
     c             maxfail,chistop,perstop,plevel-1,0)

      chi = chisq

      end
c     ***

******************************************************************************
c     ****if* smartfit.f/findint
c     FUNCTION
* Breaks the rate into intervals
c     SOURCE

      SUBROUTINE findint(intsep)

      integer     i,j,k,N,plevel,intsep(99999),tmpia(99999)
      real(KIND=8)      temp(99999),rate(99999),weight(99999)
      real(KIND=8)      tmpra(99999),tmpr

      common        /pfitvar/ temp,rate,weight,N
      common        /printlevel/ plevel

*     Calculate derivative of log10(rate)
      do i = 1, N-1
         tmpra(i) = (dlog10(rate(i+1))-dlog10(rate(i)))
     c        / (dlog10(temp(i+1))-dlog10(temp(i)))
      end do

*     Calculate second derivative and record the point index if positive
      j = 1
*     j is the next empty storage element in intsep
      do i = 1, N-2
         tmpr = (tmpra(i+1)-tmpra(i)) / 
     c        (dlog10(temp(i+1))-dlog10(temp(i)))
         if ((tmpr .GT. 0) .AND. (i .GT. 1)) then
            tmpia(j) = i
            j = j + 1
         end if
      end do
      tmpia(j) = N

*     go through tmpia and remove consecutive intervals (which is intervals
*     like 61 to 62)
      k = 0
      if (tmpia(1) .GT. 1) then
         k = k + 1
         intsep(1) = tmpia(1)
      end if
      do i = 2, j
         if (tmpia(i) .GT. tmpia(i-1) + 1) then
            k = k + 1
            intsep(k) = tmpia(i)
         end if
      end do
*     Check if it is time to add another parameter set
*     If this fit duplicates a previous fit, signal to try a different set

*      k = 1
*      intsep(1) = N

      if (plevel .GE. 2) print '(A,I6,A)','Rate is divided into ',k,
     c                                  ' intervals.'
* make sure the rest of intsep is zero
      do i = k+1, 99999
         intsep(i) = 0d0
      end do

      end
c     ***

******************************************************************************
c     ****if* smartfit.f/checkchi
c     FUNCTION
*     Subroutine to check if it's time to add another parameter set
c     SOURCE

      INTEGER FUNCTION checkchi(e,statfit,plevel)

      implicit    none
      integer     NR,statfit,plevel
      parameter   (NR = 20)
      integer     mstati(NR,2),i,j,k,e,low,high,old_k,m
      integer     runfast,maxiter,maxfail
      real(KIND=8)      mstatc(NR,4000),mstata(NR,49),tmpr
      real(KIND=8)      chistop,perstop
      common      /MSTAT/ mstati,mstatc,mstata
      common      /parmchko/ perstop,chistop,runfast,
     c     maxiter,maxfail

      checkchi = statfit
      i = 0
      do while (mstati(i+1,1) .GT. 0.0)
         i = i + 1
      end do
*     i is the number of entries in mstat
      tmpr = mstatc(e,mstati(e,2))
*     tmpr is the latest chisquared

*     search for tmpr in mstatc
      j = 1
      if (e .EQ. 1) j = 2

*     j is the entry in mstat that is being searched
      if (mstati(e,2) .GE. 2) then
         do while ((j .LE. i))
            IF (mstati(j,2) .LE. maxiter / 250) THEN
*     use bisection to search mstatc for tmpr
               low = 1
               high = mstati(j,2)
               old_k = 0
               k = high - 1
               do while (low .NE. high)
c$$$  print '(A,I3,A,I3,A,I3,A,I3,A,I3,A,I3,A,G,A,G)',
c$$$  c            'i=',i,' j=',j,' k=',k,' low=',low,' high=',high,
c$$$  c         ' old_k=',old_k,' tmpr=',tmpr,' mstatc(j,k)=',mstatc(j,k)
                  if (mstatc(j,k) .EQ. tmpr) then
*     make sure this is the latest one that's equal
                     do while ((mstatc(j,k+1) .EQ. tmpr) .AND.
     c                    (k .LT. mstati(j,2)))
                        k = k + 1
                     end do
*     print *,'match1 ',k
*     force exit
                     low = high

                     if (mstatc(j,k-1).EQ.mstatc(e,mstati(e,2)-1)) then
                        checkchi = 1
                        j = i + 1
                        if (plevel .Ge. 1) then
                         print *,'This fit duplicates a previous fit. ',
     c                        'Removing this set.'
                        end if
                        tmpr = mstatc(e,mstati(e,2))
                        do m = mstati(e,2)+1, (maxiter/250 + 1)
                           mstatc(e,m) = tmpr
                        end do
                        mstati(e,2) = maxiter / 250 + 1
                     end if
                  else if (mstatc(j,k) .LT. tmpr) then
                     high = k
                  else
                     low = k
                  end if

                  if (old_k .EQ. high) low = high
                  old_k = k
                  k = (low + high) / 2
                  if (k .EQ. old_k) k = k + 1
                  if (k .GT. high) low = high
               end do

               j = j + 1
               if (j .EQ. e) j = j + 1
            else
*               print *,'Skipped ',j,' in checkchi.'
               j = j + 1
            end if
         end do
      end if

*     If 9 parameter sets don't work then add another 7
c$$$  if (i .GE. 9) then
c$$$  checkchi = 2
c$$$  if (plevel .GE. 2) then
c$$$  print *,'Giving up on using these parameters.'
c$$$  end if
c$$$  end if

*     print *,'checkchi = ',checkchi,statfit

      end
c     ***

******************************************************************************
c     ****if* smartfit.f/statfit
c     FUNCTION
* Easy fitting routine that returns status information
* ( 0 means interval was fit, 1 means try a different set, 
*   2 means add a new set. )
* on entry e is the entry in mstati to fit
* on exit, e is the index to the entry just completed.  If an error
* occurs, e is 0

* Note that you must setup mstati and mstata before you call statfit
c     SOURCE

      integer FUNCTION statfit(e,N,a_num)

*     NR is the max number of sets that can be used
      implicit    none
      integer     NR
      parameter   (NR = 20)
      integer     i,j,plevel,mstati(NR,2),e,N,a_num,t,q
      integer     Ntotal,staticchi,runfast,maxiter,maxfail
      integer     tmpplevel,checkchi,qstart,m
      real(KIND=8)      a(49),mstata(NR,49),mstatc(NR,4000)
      real(KIND=8)      maxdiff,temp(99999),rate(99999),weight(99999)
      real(KIND=8)      log_mval,perstop,chistop,maxperdiff,tmpr
      external    maxperdiff,checkchi
      common      /printlevel/ plevel
      common      /MSTAT/ mstati,mstatc,mstata
      common      /logmval/ log_mval
      common      /pfitvar/ temp,rate,weight,Ntotal
      common      /parmchko/ perstop,chistop,runfast,
     c                         maxiter,maxfail

*     mstati is Medium STATus Index variable
*     the 1st dimension is an entry index
*     when the 2nd dimension is 1, the entry's parameter set index in the 
*        database is returned
*     when the 2nd dimension is 2, the entry's last chisquared index is 
*        returned
*     mstatc is Medium STATus Chisquared array
*     the 1st dimension is an entry index
*     the 2nd dimension is chisquared values
*     mstata is Medium STATus of A (or parameters)
*     the 1st dimension is an entry index
*     the 2nd dimension is the entry's best parameters

      do i = 1,a_num
         a(i) = mstata(e,i)
      end do

      if ((plevel .GE. 2).AND.(e.GE.1)) then
         print '(A,I3)','Trying parameter set ',e
      end if

      tmpplevel = plevel
      plevel = plevel - 2
      statfit = 0
      q = mstati(e,2) + 1
      qstart = q
      do while (q .NE. 0)
*        t is the index into mstatc to store chisquared
*         print *,'mstati(e,2)=',mstati(e,2)
         t = mstati(e,2) + 1
*        fit for 250 iterations
         call easyfit(mstatc(e,t),N,a,a_num,250,100,0d0,0d0)
*        record the index of the new chisquared
         mstati(e,2) = t
*        round chisquared to 12 digits
         call roundr(12,mstatc(e,t))
         maxdiff = 100 * maxperdiff(N,temp,rate,a,a_num,log_mval)

*        See if chisquared is static for 15*250 iterations
         staticchi = 0
         if ((t .GE. 15) .AND. ((q - qstart) .GE. 10)) then
            if (mstatc(e,t) .EQ. mstatc(e,t-14)) staticchi = -1
         end if

         if (tmpplevel .EQ. 2) then
            print '(A,I7,A,G14.3,A,G14.3)','Iterations: ',t*250,
     c     '  Chisquared: ',mstatc(e,t),'  Max percent diff: ',maxdiff
         else if (tmpplevel .GE. 3) then
            print '(A,I7,A,G14.3,A,G14.3,A,I3)','Iterations: ',t*250,
     c            '  Chisquared: ',mstatc(e,t),'  Max percent diff: ',
     c            maxdiff,'  Sets tried: ',e
         end if

*        if the parameters are too extreme quit and try different set
         if (maxdiff .LT. 0) then
            q = -1
            statfit = 1
            if (tmpplevel .GE. 2) print *,'Parameters too extreme.'

*        if the maximum number of iterations was reached, quit and try
*        different set
         else if (((q - qstart) .GE. maxiter/250/4) .OR. 
     c            (q*250 .GE. maxiter)) then
            q = -1
            if (tmpplevel .GE. 2) print *,'Tried this set long enough.'
            if (maxdiff .LE. perstop) then
               statfit = 0
            else
               statfit = 1
            end if

*        if a long fit has not progressed enough
         else if ((q .GT. 40).AND.(mstatc(e,t) .GT. 1d4)) then
            q = -1
            statfit = 1
            if (tmpplevel .GE. 2) then 
               print *,'Long fit is not progressing. ',
     c                 'Removing this set.'
            end if
            tmpr = mstatc(e,mstati(e,2))
            do m = mstati(e,2)+1, (maxiter/250 + 1)
               mstatc(e,m) = tmpr
            end do
            mstati(e,2) = maxiter / 250 + 1

*        if there is no recent progress
         else if (staticchi .ne. 0d0) then
            q = -1
            statfit = 1
            if (tmpplevel .GE. 2) print *,'No recent progress.'
         end if

*        check if it's time to try adding 7 parameters
         statfit = checkchi(e,statfit,tmpplevel)
         if (statfit .NE. 0) q = -1

*        if fit is less than perstop
         if ((q .GE. 3) .AND. (maxdiff .LE. perstop)) then
            q = -1
            statfit = 0
         end if

         q = q + 1
      end do

*     save parameters
      do i = 1, a_num
         mstata(e,i) = a(i)
      end do
      do i = a_num + 1, 49
         mstata(e,i) = 0d0
      end do
      plevel = tmpplevel
*      print *,(mstatc(e,i),i=1,mstati(e,2))

*      print *,'a= ',(mstata(e,i),i=1,a_num)
      if ((plevel .GE. 2) .AND. (statfit .EQ. 0)) then 
         print '(A,I6,A,G13.3,A)','Points 1 to ',N,' fit to ',
     c                            maxdiff,' %'
      end if

      end
c     ***

******************************************************************************
c     ****if* smartfit.f/erasestat
c     FUNCTION
*     Erase mstati and sstati
c     SOURCE

      SUBROUTINE erasestat(sstati,mstati)

      integer     NR
      parameter   (NR = 20)
      integer     sstati(NR,2),mstati(NR,2),i

      i = 1
      do while (mstati(i,1) .NE. 0)
         mstati(i,1) = 0
         i = i + 1
      end do
      i = 1
      do while (sstati(i,1) .NE. 0)
         sstati(i,1) = 0
         i = i + 1
      end do

      end
c     ***

******************************************************************************
c     ****if* smartfit.f/smartfit
c     FUNCTION
*     Main routine for smartfit
c     SOURCE

      SUBROUTINE smartfit(a,a_num)

      implicit none
      integer     NR,a_num,statfit
      parameter   (NR = 20)
      integer     sstati(NR,2),i,j,level,best,intsep(99999)
      integer     status,k,plevel,mstati(NR,2),tmpia(NR),m,p
      integer     runfast,maxiter,maxfail
      real(KIND=8)      sstatc(NR),a(49),mstata(NR,49),mstatc(NR,4000)
      real(KIND=8)      agood(49,9),tmpr,perstop,chistop
      external    statfit
      common      /printlevel/ plevel
      common      /SSTAT/ sstati,sstatc
      common      /MSTAT/ mstati,mstatc,mstata
      common      /parmchko/ perstop,chistop,runfast,
     c                         maxiter,maxfail

*     sstati is Short STATus Index variable
*     the 1st dimension is an entry index
*     when the 2nd dimension is 1, the entry's parameter set index in the
*     database is returned
*     when the 2nd dimension is 2, a flag indicating if this entry has been
*     used in a longer fit.  Longer fits are recorded in mstati, mstatc,
*     and mstata.  The flag is nonzero if this entry has been used
*     sstatc is Short STATus Chisquared variable
*     it stores the value of chisquared for the set indexed in sstati

*     Initialize variables
      do i = 1, NR
         do j = 1, 2
            sstati(i,j) = 0
            mstati(i,j) = 0
         end do
         do j = 1, 49
            mstata(i,j) = 0d0
         end do
         do j = 1, 4000
            mstatc(i,j) = 0d0
         end do
         sstatc(i) = 0d0
      end do

*     initialize random number generator
      call random_seed

*     break the rate into intervals
      call findint(intsep)

*     Set status and a_num to add a parameter set
      status = 2
      a_num = 0

*     Main loop
*     Meanings of status: 0 = success, 1 = try different set, 
*     2 = add 7 more parameters
*     k is the index to the current interval
      k = 1
      do while (intsep(k) .GT. 0)
*         print *,'status = ',status
         if (status .EQ. 2) then
 50         continue

*           search for best set and save it
            if (a_num .GT. 0) then
*              j holds the number of entries in mstat
               j = 0
               do while (mstati(j+1,1) .NE. 0)
                  j = j + 1
               end do
               tmpr = mstatc(1,mstati(1,2))
               best = 1
               do p = 2, j
                  if (mstatc(p,mstati(p,2)) .LT. tmpr) then
                     best = p
                     tmpr = mstatc(p,mstati(p,2))
                  end if
               end do

               if (plevel .GE. 2) then
                  print '(A,I3,G)','The best parameter set is ',best,
     c                 tmpr
               end if
               do p = 1, a_num
                 agood(p,(a_num/7 + 1)) = mstata(best,p)
               end do
*              record the chisquared
               agood(a_num/7,9) = mstatc(best,mstati(best,2))
            end if

*           erase mstati and sstati
            call erasestat(sstati,mstati)
*           retrieve the parameters of the previous success
            if (k .GT. 1) then
               do i = 1, a_num
                  a(i) = agood(i,1)
               end do
*               print *,'retrieving parms'
            end if
*           add new sets
            a_num = a_num + 7
            if (a_num .GT. 28) then
               if (plevel .GE. 1) then
                  print *,'WARNING: Can not add more than 28 ',
     c                 'parameters. Aborting fit in smartfit.'
               end if
               a_num = a_num - 7
               goto 200
            end if
            if (plevel .GE. 2) then
               print '(A,I2,A)','Trying to fit with ',a_num,
     c              ' parameters.'
            end if
         end if

         if (status .NE. 0) then
*            i = 1
*            do while (sstati(i,1) .GT. 0)
*               print *,'sstati1(',i,')=',sstati(i,1),sstati(i,2)
*               i = i + 1
*            end do

*           try to get 6 random parameter sets
            level = a_num / 7
            i = 1
            do while ((i .LE. 6) .AND. (level .GE. 1))
               call getranset(a,level)
               i = i + 1
            end do

*            if (status .EQ. 2) then
*               print *,level,a_num
*               print *,(a(i),i=1,a_num)
*            end if

*            i = 1
*            do while (sstati(i,1) .GT. 0)
*               print *,'sstati2(',i,')=',sstati(i,1),sstati(i,2)
*               i = i + 1
*            end do

*           find the first available entry
            i = 1
            best = 0
            do while (best .EQ. 0)
*              if none are available, pick the best entry in mstati and fit for longer
               if (sstati(i,1) .LE. 0) then
*                 find the number of entries in mstati
                  j = 0
                  do while (mstati(j+1,1) .NE. 0)
                     j = j + 1
                  end do
*                  print *,'there are ',j,' entries.'

*                 pick a random number (higher numbers have lower chisquared)
                  call random_number(tmpr)
                  p = nint(j * exp(-1.5 * (tmpr  ** 4)))
*                  print *,'tmpr=',tmpr,' entry=',p

*                 find the entry
                  do i = 1, NR
                     tmpia(i) = 0
*                     if (i .LE. j) print *,i,mstatc(i,mstati(i,2))
                  end do

*                 find the entries that reached maximum iterations
                  best = -1
                  do i = 1, j
                     if (mstati(i,2)*250 .GE. maxiter) then
                        tmpia(i) = -3
                     else
                        best = best + 1
                     end if
                  end do

*                 check if maximum iterations was reached for too many sets
*                  if (best .GE. 17) then
*                     if (plevel .GE. 2) print *,'Maximum ',
*     c                          'iterations reached for too many sets.'
*                     goto 50
*                  end if

*                 check to see if maximum iterations was reached for all sets
                  if (best .EQ. -1) then
                     if (plevel .GE. 2) print *,'Maximum ',
     c                          'iterations reached for all sets.'
                     goto 50
                  end if

                  do m = j, p, -1
*                    find the first usused entry in tmpia
                     best = 1
                     do while (tmpia(best) .NE. 0)
                        best = best + 1
                        if (best .GT. j) then
*                       find the maximum and use that as best
                           tmpr = 0
                           do i = 1, j
                              if ((mstatc(i,mstati(i,2)) .GT. tmpr) 
     c                            .AND. (tmpia(i) .NE. -3)) then
                                 best = i
                                 tmpr = mstatc(i,mstati(i,2))
                              end if
                           end do
                           tmpia(best) = 0
                        end if
                     end do
*                    tmpr is the max
                     tmpr = mstatc(best,mstati(best,2))
                     do i = 1, j
                        if (tmpia(i) .EQ. 0) then
                           if (mstatc(i,mstati(i,2)) .LT. tmpr) then
                              best = i
                              tmpr = mstatc(i,mstati(i,2))
                           end if
                        end if
                     end do
*                     print *,'lowest=',best,' m=',m
                     tmpia(best) = -1
                  end do
                  j = best
                  goto 100
               end if
               if (sstati(i,2) .EQ. 0) best = i
               i = i + 1
            end do
*           find the best entry in sstati that hasn't been used yet
            i = 1
            do while (sstati(i,1) .GT. 0)
               if ((sstatc(i) .LT. sstatc(best)) .AND.
     c              (sstati(i,2) .EQ. 0)) best = i
*               print *,i,best,sstatc(i),sstati(i,1),sstati(i,2)
               i = i + 1
            end do
         end if


*        set up mstati and mstata for statfit
*        search through mstati to find empty entry
         j = 0
         do while (mstati(j+1,1) .NE. 0)
            j = j + 1
            if (j+1 .GT. NR) then
*           if mstati is full
               if (plevel .GE. 1) then
                  print *,'ERROR: mstati is full in smartfit.'
               end if
               return
            end if
         end do

*        record that this set has been used
         sstati(best,2) = sstati(best,2) + 1

*        j is the entry index used for this loop
         j = j + 1
         mstati(j,1) = sstati(best,1)
         mstati(j,2) = 0.0
         level = a_num / 7
*         print *,'j=',j,' mstati(j,1)=',mstati(j,1)

*        Note that getset should only modify 7 parameters (even if a_num = 21)
         call getset(a,mstati(j,1),level)
         do i = 1, a_num
            mstata(j,i) = a(i)
         end do

 100     continue

*        fit for a medium amount of time
         status = statfit(j,intsep(k),a_num)

         if (status .EQ. 0) then
*           add another interval
            k = k + 1
*           erase mstati and sstati
            i = sstati(best,1)
            call erasestat(sstati,mstati)
*           add entry in sstati
            best = 1
            sstati(1,1) = i
            sstati(1,2) = 0
*           make a copy of the good parameters
            do i = 1, a_num
               agood(i,1) = a(i)
            end do
         end if
      end do

 200  continue
*     find the best entry
      tmpr = mstatc(1,mstati(1,2))
      best = 1
      do i = 2, j
         if (mstatc(i,mstati(i,2)) .LT. tmpr) then
            best = i
            tmpr = mstatc(i,mstati(i,2))
         end if
      end do

      if (plevel .GE. 2) then
         print '(A,I3,A,G13.6)','The best parameter set is ',best,
     c        ' with chisquared of ',tmpr
      end if
      do i = 1, a_num
         a(i) = mstata(best,i)
      end do

      do i = 1, a_num / 7
         print '(7E13.6)',(a(j+7*(i-1)),j=1,7)
      end do

*     find the best set overall (search agood)
      if (a_num .GT. 7) then
         tmpr = mstatc(best,mstati(best,2))
         do i = 2, a_num/7
            print *,'i=',i,' agood(i-1,9)=',agood(i-1,9),' tmpr=',tmpr
            m = (i-1)*7
            print '(A,I2,A,G)','Best ',m, 
     c           ' parameters with chisquared of ',agood(i-1,9)
            do j = 1, m/7
               print '(7E13.6)',(agood(m+7*(j-1),i),m=1,7)
            end do
            if (agood(i-1,9) .LT. tmpr) then
               a_num = (i-1)*7
               m = a_num
               if (plevel .GE. 2) then
                  print '(A,I0,A)','Best fit with ',m,
     c                 ' parameters was better than current.'
               end if
               tmpr = agood(i-1,9)
               do j = 1, m
                  a(j) = agood(j,i)
               end do
*               print *,'a modified'
            end if
         end do
      end if

*     if rate did not fit all intervals then fit all intervals for a short time
      j = 0
      do while (intsep(j+1) .NE. 0)
         j = j + 1
      end do
      
*      print *,'j=',j,' k=',k
      if (k .LT. j) then
         if (plevel .GE. 2) then
            print *,'Could not fit all intervals.'
            print *,'Trying to fit whole rate'
         end if
*     increase plevel by 1 so that the output is shown on the screen
         plevel = plevel + 1
         CALL easyfit(tmpr,0,a,a_num,maxiter,maxfail,chistop,perstop)
         plevel = plevel - 1
         print *,'New chisquared = ',tmpr
      end if

      end
c     ***
