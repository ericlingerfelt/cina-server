c********************
c BOTTLENECK FINDER PROGRAM
c********************
       program neck_15


c**********
c code outline
c**********
c   1. declare variables
c
c   2. initializations
c
c   3. read in first values of mass, num_nuclei, Z,N, fluxes
c     the fluxes are read into temporary values, and immediately integrated
c     and stored it in a global integ flux array
c     do NOT store the flux vs. timestep values
c
c   4. loop over reading ...
c      read in the next flux value, test for the end of the file
c      if end of file, go to analysis section
c
c   5. analysis section
c
c   5.1 loop over masses
c      find the super_flux, the largest flux there is
c
c   5.2 loop over mass
c       consider 4 mass values
c      5.2.a. find max integ flux of these four values, making sure to 
c             NOT consider the appropriate fluxes that do not jump over
c             the mass line of interest
c      5.2.b. find the 2nd highest flux in this group
c      5.2.c. do the threshold tests
c      5.2.c. do the gap test
c      5.2.d. assign results to global result matrix
c      5.2.e. local output within loop
c
c   6. output section
c       write out successes
c       group results of different failure modes and output
c
c
c

c**********
c   1. declare variables
c**********

       implicit none

c********************
c looping indices
c********************
       integer*4 i ! loop over mass value 1 - 150
       integer*4 j ! loop over nuclei along diagonal 1 - 20
       integer*4 k ! loop over reaction type 1 - 3
       integer*4 l ! loop over timesteps 1 - 6000
       integer*4 ii ! loop over local mass values 1 - 4
       integer*4 imass ! loop over masses
       integer*4 ll! general loop index

c********************
c timesteps - global
c********************
       integer*4 nmaxp  ! maximum number of timesteps to set array size
       parameter(nmaxp=6000)  ! be generous
       real*8 time(nmaxp)  ! the time of each of the timesteps in seconds
       integer*4 nmax   ! actual number of timesteps from CINA
       integer*4 tstep1,tstep2 !starting and stopping timestep from CINA
       real*8 sim_duration ! duration of the simulation

c********************
c timesteps - local
c********************
           real*8 temp_delta_t ! timestep for the integration
       real*8 delta_time  ! the time in seconds of a given timestep

c********************
c mass values - global
c********************
       integer*4 nmax_mass  ! maximum number of MASSES to set array size
       parameter(nmax_mass=150)  ! be generous
       integer*4 A_mass(nmax_mass) ! holds mass values read in from CINA
       integer*4 A_max  ! highest mass in the simulation from CINA
       integer*4 min_mass,max_mass ! masses considered in analysis
       integer*4 masscounter ! count the masses in the input file

c********************
c mass values - local
c********************
       integer*4 A_mass_l ! this is the mass being considered in the mass loop
       integer*4 mass   ! this is the mass value read from the input 
                        !   file [or standard input]
   
c********************
c nuclei along the diagonal - global
c********************
       integer*4 n_nuclei    !maximum # of nuclei along diagonal
       parameter(n_nuclei=20)
       integer*4 diag_nuclei(nmax_mass) ! number of nuclei 
! along each diagonal for
! all mass values 
       integer*4 z_diag_g(nmax_mass,n_nuclei) ! corresponding z values 
       integer*4 n_diag_g(nmax_mass,n_nuclei) ! corresponding z values 

c********************
c nuclei along the diagonal - local
c********************
       integer*4 z_diag(4,n_nuclei)  ! counter that labels Z values along a 
                                     !   diagonal
       integer*4 n_diag(4,n_nuclei)  ! counter that labels N values along a 
                                     !   diagonal
       integer*4 temp_num_nuclei  ! actual number of nuclei along a diagonal
       integer*4 num_nuclei(4)    ! array of the num_nuclei values for four 
                                  ! mass values, A, A+1, A+2, and A+3

c********************
c fluxes - global
c********************

                             !reaction flux integrated over timesteps
        real*8 full_flux(nmax_mass,n_nuclei,3) 
c                           |         |     | 
c                          mass value of the diagonal ... over ALL masses
c                                     |     | 
c                                      index of nucleus along mass diag
c                                           | 
c                                           reaction type: 
c                                            1=(p,gamma), 2=(alpha,p), 
c                                            3=(alpha,gamma)

        real*8 super_flux    ! the largest of the integrated fluxes

c********************
c fluxes - local
c********************

       real*8 active_flux(4,n_nuclei,3) ! flux integrated over timesteps
c                         | |        | 
c                         mass value of the diagonal ... A, A+1, A+2, and A+3
c                           |        | 
c                           the index of the nucleus along the mass diagonal
c                                    | 
c                                    reaction type: 1=(p,gamma), 2=(alpha,p), 
c                                            3=(alpha,gamma)

       real*8 temp_flux(3)  ! for reading in the fluxes, 1 per reaction type

       real*8 avg_flux   ! the average of flux over 2 adjacent timesteps

       real*8 max_integflux1           ! maximum flux over reaction type, 
                                       !   over nuclei on 4 diagonals
       real*8 max_integflux2            ! 2nd largest flux over reaction type, 
                                       !   over nuclei on 4 diagonals

   
       real*8 integ_thresh_hi,integ_thresh_lo !Lo & High Thresholds 
                                              ! for integrated reaction flux

       real*8 temptest  ! for the threshold tests
  
       real*8 gap_fac !sets the gap [ratio] in integrated fluxes 
                      ! for the gap test

       integer*4 z_max1  !Z of the nucleus that has the largest 
                         ! flux in group of 4 mass diagonals
       integer*4 n_max1  !corresponding N value
       integer*4 a_max1  !corresponding A value
       integer*4 ttype1  !corresponding label of the 
                         ! reaction type -- 1 = (p,gamma), 
                         !  2 = (alpha,p), 3 = (alpha,gamma)

       integer*4 z_max2  !Z of the nucleus that has the 2nd largest 
                         ! flux in group of 4 mass diagonals
       integer*4 n_max2  !corresponding N value
       integer*4 a_max2  !corresponding A value
       integer*4 ttype2  !corresponding label of the 
                         ! reaction type -- 1 = (p,gamma), 
                         !  2 = (alpha,p), 3 = (alpha,gamma)

       integer*4 gap_number !# of nuclei that pass the gap test for 
                         !  integrated flux, over all 4 mass diagonals
       integer*4 number2_hi,number2_lo !# of nuclei passing absolute 
                         !  test [thresholds] for integrated flux

       integer*4 junk, junk1, junk2, junk3
       real*8 trash, trash1, trash2, trash3

c********************
c flags and counters
c********************

       integer*4 iprintflag2 ! if zero, do not print out results 
                             ! from gap test, absol tests
       integer*4 iecho       ! if one, echo the input file
       integer*4 iterminal   ! if one, then run at terminal and look at
                             ! print statements; if zero, suppress prints

       integer*4 mark   ! indicates a new mass value follows if 
                        !   mark = 0, a flux value follows if 1,
                        !   OR the end of file is reached if mark = 2 
                        !   appears at the beginning of each input line


c********************
c FINAL RESULT
c********************
   
       integer*4 result(nmax_mass,10)
c
c result(i,j) where
c  i indices the mass value considered - from 1 to 200
c  j indicates the results and has 10 slots
c     1: z_max1 value
c     2: n_max1 value
c     3: ttype1 value
c     4: z_max2 value
c     5: n_max2 value
c     6: ttype2 value
c     7: gap test result
c     8: lo threshold test result
c     9: high threshold test result
c    10: CLASSIFICATION of result
c      1 if minor bottleneck - gap_number = 1, num2_lo > 0, num2_hi = 0
c      2 if major bottleneck - gap_number = 1, num2_hi = 1
c      3 if fail ONLY by gap test - gap_number > 1, num2_hi = 1
c      4 if fail ONLY by hi threshold test [too many] - gap = 1, num2_hi > 1
c      5 if fail ONLY by hi threshold test [too few] - gap = 1, num2_hi = 0
c      6 if fail ONLY by lo threshold test [none] - gap = 1, num2_lo = 0
c      7 if fail by gap AND hi thresh test [too many]
c      8 if fail by gap AND hi thresh test [too few]
c      9 if fail by gap AND lo thresh test
c      10 something wrong, gap_number = 0 
c



c**********
c   2. initializations
c**********

        do l=1,nmaxp
          time(l)=0.0d0
        enddo 
         
        nmax=0
        tstep1=0
        tstep2=0
        sim_duration = 0.0d0 
        temp_delta_t = 0.0d0
        delta_time = 0.0d0
        
        do i=1,nmax_mass
           A_mass(i) = 0
           diag_nuclei(i) = 0
           do j=1,n_nuclei
              z_diag_g(i,j) = 0 
              n_diag_g(i,j) = 0
           enddo
        enddo
        
        A_max = 0
        A_mass_l = 0 
           mass = 0
       min_mass = 0
           max_mass = 0
       masscounter = 0
           
         do i=1,4
          do j=1,n_nuclei
             z_diag(i,j) = 0
             n_diag(i,j) = 0
          enddo
        enddo
           
        do j=1,4
           num_nuclei(j) = 0
        enddo
           
           temp_num_nuclei = 0
       
            do i=1,nmax_mass
              do j = 1,n_nuclei
                do k = 1,3
                  full_flux(i,j,k)=0.0d0
                enddo
              enddo
            enddo
                
           super_flux = 0.0d0    ! the largest of the integrated fluxes
                
            do ii=1,4
              do j = 1,n_nuclei
                    do k = 1,3
                  active_flux(ii,j,k)=0.0d0
                    enddo
              enddo
            enddo
       temp_flux(1) = 0.0d0
       temp_flux(2) = 0.0d0
       temp_flux(3) = 0.0d0
       avg_flux = 0.0d0
       max_integflux1 = 0.0d0
       max_integflux2 = 0.0d0
       integ_thresh_hi = 0.0d0
       integ_thresh_lo = 0.0d0
       gap_fac = 0.0d0
       z_max1 = 0
       n_max1 = 0
       a_max1 = 0
       ttype1 = 0
       z_max2 = 0
       n_max2 = 0
       a_max2 = 0
       ttype2 = 0
       gap_number = 0
       number2_hi = 0
       number2_lo = 0

       junk = 0
       junk1 = 0
       junk2 = 0
       junk3 = 0
       trash = 0.0d0
       trash1 = 0.0d0
       trash2 = 0.0d0
       trash3 = 0.0d0

       iprintflag2 = 0
       iecho = 0
       iterminal = 0
       mark = 0
       
            do i=1,nmax_mass
              do ll = 1,10
                  result(i,10) = 0
                  enddo
                enddo

c**********
c   3. read in first values of mass, num_nuclei, Z,N, fluxes
c     the fluxes are read into temporary values, and immediately integrated
c     and stored it in a global integ flux array
c     do NOT store the flux vs. timestep values
c**********
c*******************
c Open Files
c*******************


       open(3,file='parameter.dat',status='old')    ! this param file will 
                                                    !  be written to disk 
                                                    !  by CINA and read as 
                                                    !unit 3

       if(iecho.eq.1) then
          open(9,file='echo.dat',status='old')      ! this file will 
       endif                                        !  echo the input 
                                                    !  file 

       if(iprintflag2.eq.1) then
          open(10,file='long_result.dat',status='old') ! LONG output file
       endif

c*******************
c first read statements
c*******************

       read(3,10)gap_fac,integ_thresh_hi,integ_thresh_lo  ! read parameters
 10    format(e12.6/e12.6/e12.6)

       if(iprintflag2.eq.1) then
          write(10,11)gap_fac,integ_thresh_hi,integ_thresh_lo  ! echo
 11    format('gap fac, thresh_hi,thresh_lo =',1pe12.6/1pe12.6/1pe12.6)
       endif

             !read first time step number, last timestep number, 
             !actual # of timesteps, lowest mass considered, highest mass cons., 
             ! largest mass in sim 

       read(5,12)tstep1,tstep2,nmax,min_mass,max_mass,A_max
       if(iecho.eq.1) write(9,12)tstep1,tstep2,nmax,
     |                           min_mass,max_mass,A_max  !echo input 

 12    format(i4/i4/i4/i4/i4/i4)
 

       if(iterminal.eq.1) then
       print*,'nmax and A_max =',nmax,A_max
       endif

       do 16 i=1,nmax 
          if(iterminal.eq.1) then
            print*,'read this time',i
          endif
         read(5,13)time(i)   ! read in the times of each of the timesteps
             if(iecho.eq.1) write(9,13)time(i)   ! echo
13       format(1pe19.12)

         if(iprintflag2.eq.1) then
           write(10,15)i,time(i)
15         format(i6,1pe19.12)
         endif

16     continue

c*****************
c define sim_duration
c*****************
                 sim_duration = time(nmax) - time(1)
        if(sim_duration.eq.0.0d0) then 
          if(iterminal.eq.1) then
            print*,'problem with zero duration of sim, abort'
          endif
          goto 9000  ! abort
        endif

c****************
c read in fluxes for the FIRST mass, the first time through
c****************

100    continue   ! loop back up if the first line has num_nuclei to be zero

       mass=0
       temp_num_nuclei=0
       mark=3        ! character at beginning of line, should never equal 3

c****************
c first check to see if first line of input is OK
c****************
         read(5,110)mark
         if(iecho.eq.1) write(9,110)mark
110      format(i1)

         if(mark.ne.0) then    ! big problem with input file

           if(iterminal.eq.1) then
              print*,'problem with input file,mark is not 0,abort'
           endif
           
           goto 9000

         else                 ! mark = 0, and the next line will have the 
                              ! mass number and the number along the diagonal

c****************
c now check to see if the mass is non-zero & if there are nuclei 
c along the diagonal -- do this second step for the first mass only
c****************

           read(5,130)mass,temp_num_nuclei ! read the mass number and 
                                      ! number of nuclei along the diagonal
           if(iecho.eq.1) write(9,130)mass,temp_num_nuclei
130        format(i3,1x,i3)

           if(mass.eq.0) then   ! big problem with input file

                if(iterminal.eq.1) then
                  print*,'problem with input file, 1st mass zero, abort'
                endif
                goto 9000

            elseif(mass.lt.min_mass) then ! confusion about the min mass

             if(iterminal.eq.1) then
               print*,'problem with input, 1st mass.lt.min_mass, abort'
             endif
             goto 9000
                        
            else          ! mass must be non-zero and >=  min_mass
                                     

                if(temp_num_nuclei.eq.0) then ! no nuclei along this diagonal
                                    ! in this case, read another line 
                                    ! in until you get one that is non-zero
                    goto 100   ! loop back up, read another
                else  ! for non-zero num_nuclei, OK
                endif !  checking nuclei along diagonal

                        endif ! checking for non-zero masses

         endif ! checking mark

         if(iprintflag2.eq.1) then
           write(10,132)mass,temp_num_nuclei
132        format('first read mass & num nuclei =',i3,1x,i3)
                   ! note:  both of which are non-zero to get here
         endif

c****************
c finally read in fluxes for the first mass, the first time through
c****************

         A_mass(1) = mass  ! this is special, the first mass
                                   ! that we read with non-zero num_nuclei
         diag_nuclei(1) = temp_num_nuclei

                 masscounter = 1  ! we have read one mass in                 

         do 160 j=1,diag_nuclei(1)  ! loop over nuclei along diagonal

            do 150 l=1,nmax      ! loop over timesteps

               ! read the three fluxes at each timestep for each nuclei
               ! store them in a temporary values, and immediately 
                           ! integrate them and store in a global array
                           
                              temp_flux(1)=0.0d0 !reinitialize before read
                              temp_flux(2)=0.0d0
                              temp_flux(3)=0.0d0

               read(5,140)junk,z_diag_g(1,j),
     |                         temp_flux(1),
     |                         temp_flux(2),
     |                         temp_flux(3)
               if(iecho.eq.1) write(9,140)junk,z_diag_g(1,j),
     |                         temp_flux(1),
     |                         temp_flux(2),
     |                         temp_flux(3)
140            format(i1,1x,i3,1x,2(1pe13.6,1x),1pe13.6)


                if(l.eq.1) goto 145  ! don't calculate integrated flux
                                                                          ! until second time step
                    
                                delta_time = time(l)  -  time(l-1)

                do 142 k=1,3                                        
                  if(temp_flux(k).lt.0.0d0) temp_flux(k) = 0.0d0
                  if(delta_time.lt.0.0d0) delta_time = 0
                  ! only consider positive fluxes and
                  ! positive time steps
                                                                                                                  

                   full_flux(1,j,k) =  full_flux(1,j,k) +
     |                              (temp_flux(k)*delta_time)

142             continue  ! end of loop over k 

145           continue  ! jump here after the first read only
                                    
150         continue  ! loop over time steps, 
                      ! this will finish three flux integrations, one 
                                          ! per reaction type, for a nucleus along diagonal

            n_diag_g(1,j)=A_mass(1) - z_diag_g(1,j)

160      continue  ! loop over nuclei in the diagonal





c**********
c   4. loop over reading ...
c      read in the next flux value, test for the end of the file
c      if end of file, go to analysis section
c**********

          do 300 i = 2,nmax_mass   ! loop over all masses

       mass=0
       temp_num_nuclei=0
       mark=3        ! character at beginning of line, should never equal 3

c****************
c first check to see if first line of input is OK
c****************
         read(5,210)mark
         if(iecho.eq.1) write(9,210)mark
210      format(i1)

         if(mark.eq.2) then    ! reached end of input file
             
                        if(i.lt.4) then

              if(iterminal.eq.1) then
                 print*,'LESS THAN 4 total masses, problem, abort'
                          endif
           
              goto 9000

            else  ! if there are more than 4 masses, 
                              ! then we go to the analysis stage

                          goto 302  ! jump outside of read loop
                                     
            endif  ! less than 4 mass check

         elseif(mark.eq.0) then    ! this is what we want, read more
                                   ! mark = 0, and the next line will have the 
                                   ! mass number and the number along the diagonal

            if(iterminal.eq.1) then
                print*,'have not reached the end, keep going'
                endif
                        goto 215      ! jump out of this test of mark                          

         else ! big problem with input file, mark is not 0 or 2

             if(iterminal.eq.1) then
                print*,'problem with input, mark .ne. 0 or 2, abort...'
             endif
             goto 9000             

         endif  ! test of mark

215      continue 

c****************
c now read in the mass and check to see if it is the expected value
c also read in the number along the diagonal
c****************

           read(5,230)mass,temp_num_nuclei ! read the mass number and 
                                      ! number of nuclei along the diagonal
           if(iecho.eq.1) write(9,230)mass,temp_num_nuclei
230        format(i3,1x,i3)

           if(mass.ne.( A_mass(i-1) + 1) ) then   
                                           ! big problem with input file
                                           ! masses are not sequential
                if(iterminal.eq.1) then
                  print*,'problem, non-sequential masses, abort'
                endif
                goto 9000

            else          ! mass is OK

                if(temp_num_nuclei.eq.0) then ! no nuclei along this diagonal
                                    ! that is OK, proceed 
                else  ! for non-zero num_nuclei, OK
                endif !  checking nuclei along diagonal

                        endif ! checking for incorrect masses

         if(iprintflag2.eq.1) then
           write(10,232)mass,temp_num_nuclei
232        format('next read mass & num nuclei =',i3,1x,i3)
         endif


c****************
c finally read in fluxes for the next mass
c****************

         A_mass(i) = mass  
         diag_nuclei(i) = temp_num_nuclei
                 masscounter = masscounter+1 ! count those masses
                                  
         do 260 j=1,diag_nuclei(i)  ! loop over nuclei along diagonal

            do 250 l=1,nmax      ! loop over timesteps

               ! read the three fluxes at each timestep for each nuclei
               ! store them in a temporary values, and immediately 
                           ! integrate them and store in a global array
                           
                              temp_flux(1)=0.0d0 !reinitialize before read
                              temp_flux(2)=0.0d0
                              temp_flux(3)=0.0d0

               read(5,240)junk,z_diag_g(i,j),
     |                         temp_flux(1),
     |                         temp_flux(2),
     |                         temp_flux(3)
               if(iecho.eq.1) write(9,140)junk,z_diag_g(i,j),
     |                         temp_flux(1),
     |                         temp_flux(2),
     |                         temp_flux(3)
240            format(i1,1x,i3,1x,2(1pe13.6,1x),1pe13.6)


                if(l.eq.1) goto 245  ! don't calculate integrated flux
                                     ! until second time step
                    
                                delta_time = time(l)  -  time(l-1)

                do 242 k=1,3                                        
                   if(temp_flux(k).lt.0.0d0) temp_flux(k) = 0.0d0
                   if(delta_time.lt.0.0d0) delta_time = 0
                      ! only consider positive fluxes and
                      ! positive time steps

                   full_flux(i,j,k) =  full_flux(i,j,k) +
     |                              (temp_flux(k)*delta_time)

242             continue  ! end of loop over k 

245           continue  ! jump here after the first read only
                                    
250         continue  ! loop over time steps, 
                      ! this will finish three flux integrations, one 
                      ! per reaction type, for a nucleus along diagonal

            n_diag_g(i,j)=A_mass(i) - z_diag_g(i,j)

260      continue  ! loop over nuclei in the diagonal


300     continue  ! loop over all masses

302     continue  ! jump out of here after mark = 2 is read in

        if(iprintflag2.ne.1) goto 330
              do i=1,nmax_mass
               do j = 1,n_nuclei
                    do k = 1,3
                 if( (A_mass(i).eq.13)
     |              .and.(j.eq.2)
     |              .and.(k.eq.1)) then
                           write(11,328)A_mass(i),j,k
                           write(11,329)full_flux(i,j,k)
                                   
328                format('Amass j k =',3(i4))
329                format('integ flux ',1x,1pe13.6)
                 endif 
                        enddo
                   enddo
                  enddo
330       continue

c**********
c   5. analysis section
c**********
c   5.1 loop over masses
c      find the super_flux, the largest flux there is
c**********

       super_flux=0.0d0
           
       do 550 i = 1,masscounter  ! loop over four different mass diagonals
           do 540 j=1,diag_nuclei(i) !loop along mass diagonal i    
              do 530 k=1,3  ! loop over reaction types
                                ! Find max of integ flux 
                 if(full_flux(i,j,k).ge.super_flux)then 
                     super_flux=full_flux(i,j,k)  
                 else
                 endif
 530           continue              !loop over reaction type
 540       continue             !loop along mass diagonal
 550    continue         ! loop over 4 different mass diagonals


c**********
c   5.2 loop over mass
c       assign the 4 mass values
c**********

       do 1000 i=1,masscounter  ! loop over all masses read in
         ! assign local variables from global ones
c              A_mass_l = A_mass(i)      ! assign the first local mass value
c
c                 z_diag(ii,diag_nuclein_nuclei)



c**********
c      5.2.a. find max integ flux of these four values, making sure to 
c             NOT consider the appropriate fluxes that do not jump over
c             the mass line of interest
c**********

       max_integflux1 = 0.0d0   ! initialize the max flux
          
c****************
c note: certain of the fluxes are NOT used when calculating
c the integrated flux, max_integrated flux, or any other 
c processing. these are:
c
c mass 1, A_mass(i): all of them
c mass 2, A_mass(i)+1: (p,g) not used
c mass 3, A_mass(i)+2: (p,g) not used
c mass 4, A_mass(i)+3: (p,g) and (a,p) not used
c
c so use an array active_flux(ii,j,k) that equals full_flux
c except is zero for the above special fluxes that we must ignore
c in calculating the max
c
c****************

c****************
c initialize the active_flux matrix
c****************
            do ii=1,4
              do j = 1,n_nuclei
                    do k = 1,3
                  active_flux(ii,j,k)=0.0d0 ! initialize
                        enddo
                  enddo
            enddo


c****************
c now define active_flux values for mass 1, A_mass(i)
c   explicitly write out the loops over mass and over reaction type
c****************

         if(diag_nuclei(i).eq.0) goto 560  !skip the loop
                 
         do j=1,diag_nuclei(i)  ! loop over nuclei along diagonal
               active_flux(1,j,1)=full_flux(i,j,1)
               active_flux(1,j,2)=full_flux(i,j,2)
               active_flux(1,j,3)=full_flux(i,j,3)
         enddo

560      continue

c****************
c now define active_flux values for mass 2, A_mass(i+1)
c   explicitly write out the loops over mass and over reaction type
c****************


         if(diag_nuclei(i+1).eq.0) goto 570  !skip the loop
                 
         do j=1,diag_nuclei(i+1)  ! loop over nuclei along diagonal
               active_flux(2,j,1)=0.0d0
               active_flux(2,j,2)=full_flux(i+1,j,2)
               active_flux(2,j,3)=full_flux(i+1,j,3)
         enddo

570      continue

c****************
c now define active_flux values for mass 3, A_mass(i+2)
c   explicitly write out the loops over mass and over reaction type
c****************


         if(diag_nuclei(i+2).eq.0) goto 580  !skip the loop
                 
         do j=1,diag_nuclei(i+2)  ! loop over nuclei along diagonal
               active_flux(3,j,1)=0.0d0
               active_flux(3,j,2)=full_flux(i+2,j,2)
               active_flux(3,j,3)=full_flux(i+2,j,3)
         enddo

580      continue


c****************
c now define active_flux values for mass 4, A_mass(i+3)
c   explicitly write out the loops over mass and over reaction type
c****************


         if(diag_nuclei(i+3).eq.0) goto 590  !skip the loop
                 
         do j=1,diag_nuclei(i+3)  ! loop over nuclei along diagonal
               active_flux(4,j,1)=0.0d0
               active_flux(4,j,2)=0.0d0
               active_flux(4,j,3)=full_flux(i+3,j,3)
         enddo

590      continue



c****************
c now find the max flux using the active_flux values 
c****************

       do 650 ii = 1,4  ! loop over four different mass diagonals
           
                   imass = i + ii - 1  ! this runs from i, i+1, i+2, i+3

                   if(diag_nuclei(imass).eq.0) goto 645 ! skip the tests
                                                ! no nuclei on this diagonal
           do 640 j=1,diag_nuclei(imass) !loop along mass diagonal ii    

              do 630 k=1,3  ! loop over reaction types

                                ! Find max of integ flux at A_mass along 
                                ! diagonal
                                ! and save the Z,N values of the nucleus 
                                ! which has the largest integrated flux
                                ! remember we cannot consider ALL the 
                                ! fluxes ... 
                 if(active_flux(ii,j,k).ge.max_integflux1)then 
                     max_integflux1=active_flux(ii,j,k)  
                     z_max1=z_diag_g(imass,j)   
                     n_max1=n_diag_g(imass,j)
                             a_max1=z_max1+n_max1
                     ttype1=k

                                 else
                                 endif  ! flux check 


 630           continue              !loop over reaction type

 640       continue             !loop along mass diagonal
 
 645    continue      ! jump here if no nuclei along this diagonal
 
 650    continue         ! loop over 4 different mass diagonals

c**********
c      5.2.b. find the 2nd highest flux in this group
c**********
       max_integflux2 = 0.0d0   ! initialize the max flux

       do 670 ii = 1,4  ! loop over four different mass diagonals
           
                   imass = i + ii - 1  ! this runs from i, i+1, i+2, i+3

                   if(diag_nuclei(imass).eq.0) goto 665 ! skip the tests
                                                ! no nuclei on this diagonal
           do 660 j=1,diag_nuclei(imass) !loop along mass diagonal ii    

              do 655 k=1,3  ! loop over reaction types

                                ! Find max of integ flux at A_mass along 
                                ! diagonal
                                ! and save the Z,N values of the nucleus 
                                ! which has the largest integrated flux
                                ! remember we cannot consider ALL the 
                                ! fluxes ... 

                 if( 
     |               (z_diag_g(imass,j).eq.z_max1).and.
     |               (n_diag_g(imass,j).eq.n_max1).and.
     |               (k.eq.ttype1)
     |              ) goto 652
                           ! this is the reaction with the MAX flux
                           ! so don't consider it for the test for
                           ! the 2nd largest flux, just do nothing

                 if(active_flux(ii,j,k).ge.max_integflux2)then 
                     max_integflux2=active_flux(ii,j,k)  
                     z_max2=z_diag_g(imass,j)   
                     n_max2=n_diag_g(imass,j)
                             a_max2=z_max1+n_max1
                     ttype2=k

                                 else
                                 endif  ! flux check 

 652            continue
 
 655           continue              !loop over reaction type

 660       continue             !loop along mass diagonal
 
 665    continue      ! jump here if no nuclei along this diagonal
 
 670    continue         ! loop over 4 different mass diagonals

 
c**********
c      5.2.c. do the threshold tests
c**********

       if(super_flux.eq.0.0d0) then

            if(iterminal.eq.1) then
              print*,'problem, superflux = 0'
            endif
            goto 9000  ! abort

        else
        endif  ! super_flux check
     
           number2_hi = 0      ! initialize for this round of tests
           number2_lo = 0
           temptest = 0.0d0

       do 770 ii = 1,4  ! loop over four different mass diagonals
           
                   imass = i + ii - 1  ! this runs from i, i+1, i+2, i+3

                   if(diag_nuclei(imass).eq.0) goto 765 ! skip the tests
                                                ! no nuclei on this diagonal
           do 760 j=1,diag_nuclei(imass) !loop along mass diagonal ii    

              do 755 k=1,3  ! loop over reaction types

                                ! Find max of integ flux at A_mass along 
                                ! diagonal
                                ! and save the Z,N values of the nucleus 
                                ! which has the largest integrated flux
                                ! remember we cannot consider ALL the 
                                                                ! fluxes ... 

                 temptest = active_flux(ii,j,k) / super_flux
                                 
                 if(temptest.ge.integ_thresh_lo) then
                                 
                       number2_lo=number2_lo+1
                 endif
                                 
                 if(temptest.ge.integ_thresh_hi) then
 
                      number2_hi=number2_hi+1
                 endif

 755           continue              !loop over reaction type

 760       continue             !loop along mass diagonal
 
 765    continue      ! jump here if no nuclei along this diagonal
 
 770    continue         ! loop over 4 different mass diagonals


c**********
c      5.2.c. do the gap test
c**********

           gap_number = 0     ! initialize for this round of tests
           temptest = 0.0d0
           
       do 870 ii = 1,4  ! loop over four different mass diagonals
           
                   imass = i + ii - 1  ! this runs from i, i+1, i+2, i+3

                   if(diag_nuclei(imass).eq.0) goto 865 ! skip the tests
                                                ! no nuclei on this diagonal
           do 860 j=1,diag_nuclei(imass) !loop along mass diagonal ii    

              do 855 k=1,3  ! loop over reaction types

                                ! Find max of integ flux at A_mass along 
                                ! diagonal
                                ! and save the Z,N values of the nucleus 
                                ! which has the largest integrated flux
                                ! remember we cannot consider ALL the 
                                                                ! fluxes ... 

                 temptest = gap_fac*max_integflux1
                                
                                if(active_flux(ii,j,k).ge.temptest) then
                        
                    gap_number=gap_number+1

                else
                                endif



 855           continue              !loop over reaction type

 860       continue             !loop along mass diagonal
 
 865    continue      ! jump here if no nuclei along this diagonal
 
 870    continue         ! loop over 4 different mass diagonals


c**********
c      5.2.d. assign results to global result matrix
c**********

c************
c assign the values to the result matrix
c************

        result(i,1)=z_max1
        result(i,2)=n_max1
        result(i,3)=ttype1
        result(i,4)=z_max2
        result(i,5)=n_max2
        result(i,6)=ttype2
        result(i,7)=gap_number
        result(i,8)=number2_lo
        result(i,9)=number2_hi

c    10: CLASSIFICATION of result
c      1 if minor bottleneck - gap_number = 1, num2_lo > 0, num2_hi = 0
c      2 if major bottleneck - gap_number = 1, num2_hi = 1
c      3 if fail ONLY by gap test - gap_number > 1, num2_hi = 1
c      4 if fail ONLY by hi threshold test [too many] - gap = 1, num2_hi > 1
c      5 if fail ONLY by hi threshold test [too few] - gap = 1, num2_hi = 0
c        note: this is redundant, if num2_lo >0, minor bottleneck
c                                 if num2_lo = 0, then case 6
c      6 if fail ONLY by lo threshold test [none] - gap = 1, num2_lo = 0
c      7 if fail by gap AND hi thresh test [too many]
c      8 if fail by gap AND hi thresh test [too few]
c      9 if fail by gap AND lo thresh test
c      10 something wrong, gap_number = 0 

      if((gap_number.eq.1).and.(number2_lo.gt.0).and.(number2_hi.eq.0)) 
     | then
                   result(i,10)=1    ! minor bottleneck
 
      elseif((gap_number.eq.1).and.(number2_hi.eq.1)) then
                   result(i,10)=2    ! major bottleneck

      elseif((gap_number.gt.1).and.(number2_hi.eq.1)) then
                   result(i,10)=3    ! fail ONLY by gap test

      elseif((gap_number.eq.1).and.(number2_hi.gt.1)) then
                   result(i,10)=4    ! fail ONLY by hi threshold test [too many]

c      elseif((gap_number.eq.1).and.(number2_hi.eq.0)) then
c                   result(i,10)=5    ! fail ONLY by hi threshold test [too few]

      elseif((gap_number.eq.1).and.(number2_lo.eq.0)) then
                   result(i,10)=6    ! fail ONLY by lo threshold test [too few]

      elseif((gap_number.gt.1).and.(number2_hi.gt.1)) then
                   result(i,10)=7  ! fail by gap test AND hi threshold test [many]

      elseif((gap_number.gt.1).and.(number2_hi.eq.0)) then
                   result(i,10)=8  ! fail by gap test AND hi threshold test [few]

      elseif((gap_number.gt.1).and.(number2_lo.eq.0)) then
                   result(i,10)=9    ! fail by gap test AND lo threshold test

      elseif((gap_number.lt.1)) then
                   result(i,10)=10    ! something wrong in gap test

      else
      endif



c**********
c      5.2.e. local output within loop
c**********
        if(iprintflag2.ne.1) goto 940
       
             write(10,910)A_mass(i)
             write(10,911)z_max1,n_max1,ttype1
             write(10,912)max_integflux1
910      format(//'mass location',i3)
911      format('max flux at z,n of ',i3,i3,' of type',i2)
912      format('   of value ',1pe12.6)

             write(10,921)z_max2,n_max2,ttype2
             write(10,922)max_integflux2
921      format('2nd largest flux at z,n of ',i3,i3,' of type',i2)
922      format('   of value ',1pe12.6)

         write(10,932)integ_thresh_lo,integ_thresh_hi,
     |                 number2_lo,number2_hi
932      format('thresh lo, thresh hi',2(1x,1pe13.6)/
     |          'thresh test results lo  hi ',i3,i3)

         write(10,934)gap_fac,gap_number
934      format('gap factor, gap number',1x,1pe13.6,1x,i3)

                 if(result(i,10).eq.1) then
            write(10,936)result(i,1),result(i,2),
     |        result(i,1)+result(i,2),result(i,3)
936        format('MINOR bottleneck for Z N A at ',3(1x,i3),
     |             ' of type = ',1x,i3//)


                 else if(result(i,10).eq.2) then
            write(10,938)result(i,1),result(i,2),
     |        result(i,1)+result(i,2),result(i,3)
938        format('MAJOR bottleneck for Z N A at ',3(1x,i3),
     |             ' of type = ',1x,i3//)
             else
                 endif 
                 
940      continue

1000    continue   ! main loop in mass


c**********
c   6. output section
c       write out successes
c       group results of different failure modes and output
c**********
c

c**********
c       write out to file 15
c**********
      if(iprintflag2.ne.1) goto 1045

      do 1040 i=1,masscounter

         write(15,1010)A_mass(i),result(i,10)
1010     format('mass ',1x,i3,' result = ',1x,i3)
        
                 if(result(i,10).eq.1) then
            write(15,1020)result(i,1),result(i,2),
     |        result(i,1)+result(i,2),result(i,3)
1020        format('MINOR bottleneck for Z N A at ',3(1x,i3),
     |             ' of type = ',1x,i3/)


                 else if(result(i,10).eq.2) then
            write(15,1030)result(i,1),result(i,2),
     |        result(i,1)+result(i,2),result(i,3)
1030        format('MAJOR bottleneck for Z N A at ',3(1x,i3),
     |             ' of type = ',1x,i3/)

         else
                 endif
                 
1040     continue ! output loop

1045     continue

c**********
c       write out to unit 6 to pipe to CINA
c**********

      do 1060 i=1,masscounter

                 if(result(i,10).eq.2) then
            write(6,1050)A_mass(i),result(i,1),result(i,2),
     |        result(i,1)+result(i,2),result(i,3),result(i,10)
1050        format(6(1x,i4))
         else
                 endif
                 
1060   continue


      do 1080 i=1,masscounter

                 if(result(i,10).eq.1) then
            write(6,1070)A_mass(i),result(i,1),result(i,2),
     |        result(i,1)+result(i,2),result(i,3),result(i,10)
1070        format(6(1x,i4))
         else
                 endif

1080    continue


         do 1120 i=1,masscounter
                ! fail ONLY by gap test
                 if(result(i,10).eq.3) then   
            write(6,1110)A_mass(i),result(i,10)
1110        format(2(1x,i4))
                 endif
1120     continue


         do 1140 i=1,masscounter
                 ! fail ONLY by hi threshold test [too many]
                 if(result(i,10).eq.4) then 
            write(6,1130)A_mass(i),result(i,10)
1130        format(2(1x,i4))

                 endif
1140     continue


          do 1160 i=1,masscounter
                ! fail ONLY by hi threshold test [too few]
                 if(result(i,10).eq.5) then
            write(6,1150)A_mass(i),result(i,10)
1150        format(2(1x,i4))
                 endif
1160     continue



         do 1180 i=1,masscounter
                ! fail ONLY by lo threshold test [too few]
                 if(result(i,10).eq.6) then
            write(6,1170)A_mass(i),result(i,10)
1170        format(2(1x,i4))
                 endif
1180     continue



         do 1200 i=1,masscounter
                 ! fail by gap test AND hi threshold test [many]
                 if(result(i,10).eq.7) then
            write(6,1190)A_mass(i),result(i,10)
1190        format(2(1x,i4))
                 endif
1200     continue



         do 1220 i=1,masscounter
                 ! fail by gap test AND hi threshold test [few]
                 if(result(i,10).eq.8) then
            write(6,1210)A_mass(i),result(i,10)
1210        format(2(1x,i4))
                 endif
1220     continue


         do 1240 i=1,masscounter
                 ! fail by gap test AND lo threshold test
                 if(result(i,10).eq.9) then
            write(6,1230)A_mass(i),result(i,10)
1230        format(2(1x,i4))
                 endif
1240     continue


         do 1260 i=1,masscounter
                 ! something wrong in gap test
                 if(result(i,10).eq.10) then
            write(6,1250)A_mass(i),result(i,10)
1250        format(2(1x,i4))
                 endif
1260     continue


9000     continue  ! abort


         stop
                 end
                 
