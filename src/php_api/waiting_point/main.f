c*********************
c Main Program
c*********************

      program main
       implicit none

c***********************
c declare variables for the basic and w1
c***********************

       integer*4 nmaxp
       parameter(nmaxp=1300)        !number of time steps
       integer*4 Z  !the maximum number of protons
       integer*4 N  !the maximum number of neutrons
       parameter(Z=55)
       parameter(N=55)
       integer*4 max_stable
       parameter(max_stable=100)
       integer*4 num            !the number of nuclei for comparison
       parameter(num=10)
       integer*4 nfac              !the number of factors (or conditions)
       parameter(nfac=11)
       integer*4 n_reac       !number of reactions to consider
       parameter(n_reac=3)
       integer*4 n_c       ! number of nuclei for creation(w7)
       parameter(n_c=6)
       integer*4 n_trial
       parameter(n_trial=9)
       integer*4 n_rejection
       parameter(n_rejection=3)
       integer*4 n_acceptance
       parameter(n_acceptance=8)
       integer*4 n_pam
       parameter(n_pam=4)

       integer*4 i,j,k,l

       integer*4 zo !the number of protons
       integer*4 no !the number of neutrons

       real*8 b(nmaxp)            !abundance of B
       real*8 x(num,nmaxp)        !abundance of other nuclei for comparison
       real*8 Bm                 !the maximum value of B
       real*8 Xm(num)            !the maximu abundance of the nuclei
       real*8 M      !the maximum value of abundance of all(w3)
       integer*4 order(num)         !the order of abundances(AT(3))
       real*8 new_x(num)       !(AT3)
       real*8 ratio(num)       ! ratios of order abundances
       real*8 M_ratio          ! the minimum ratio
       integer*4 place
       real*8 xm_new(num)     ! for test1_1(w2)
       integer*4 n_sum(num+1)          !usimg for comparison
       integer*4 n_sum2

       real*8 Lb     !lifetime of B[sec] (w5)
       real*8 r_eff  !effective reaction rate for effective lifetime
       real*8 L_eff
       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)
       real*8 result

       integer*4 nmax
       integer*4 n_nuclei
       integer*4 value_z(nmaxp)
       integer*4 value_n(nmaxp)
       real*8 y(Z,N,nmaxp)       !read
       integer*4 timestep(nmaxp)
       real*8 temp(nmaxp)
       real*8 density(nmaxp)    !flux
       real*8 y_p(nmaxp)        !flux
       real*8 y_alpha(nmaxp)    !flux

       integer*4 num_stable    !number of stable nuclei
       integer*4 z_stable(max_stable)!stable
       integer*4 n_stable(max_stable)!stable

       real*8 border         !major path
       integer*4 f(n_c+2,2)  !major path

       real*8 x_creation(nmaxp) !for AT5

c       integer*4 turn_on(nfac)    !option
c       character*10  nuc(n_trial)
c       character(len=256) :: buf
       integer*4 junk
       real*8 dummy
       integer*4 n_zero
       real*8 c_save

       real*8 time(nmaxp)
       real*8 flux1(Z,N,nmaxp)
       real*8 flux2(Z,N,nmaxp)
       real*8 flux3(Z,N,nmaxp)
       real*8 flux4(Z,N,nmaxp)
       real*8 flux5(Z,N,nmaxp)
       real*8 flux6(Z,N,nmaxp)
       real*8 flux7(Z,N,nmaxp)
       real*8 flux0(Z,N,nmaxp)

       real*8 reacc_beta(nmaxp)
       real*8 reacd_beta(nmaxp)
       real*8 new_reacc(n_c,nmaxp)
       real*8 new_reacd(n_c,nmaxp)

       real*8 flux_in(n_c+1,nmaxp)
       real*8 flux_out(n_c+1,nmaxp)
       real*8 flux(n_c+2,nmaxp)
       real*8 integral_flux(n_c+2)
       real*8 total_integflux
       real*8 reac_eff(nmaxp)
       real*8 each_Lb(nmaxp)
       real*8 each_L(nmaxp)
       real*8 integ_L
       real*8 sum_flux(nmaxp)
       real*8 wt
       real*8 total_flux(nmaxp)
       real*8 total_fluxin(nmaxp)
       real*8 total_fluxout(nmaxp)
       real*8 total_max,total_min
       integer*4 time_initial,time_final
       integer*4 time_max
       real*8 fluxin_max
       integer*4 t_m
       integer*4 num_crazy
       integer*4 z_crazy(max_stable),n_crazy(max_stable)
       real*8 Lb_crazy(max_stable)
       real*8 ave_y(Z,N)
       real*8 nom_fac(nmaxp)

       real*8 conclusion(Z,N)
       real*8 RT1(Z,N),RT2(Z,N),RT3(Z,N),RT4(Z,N)
       real*8 AT1(Z,N),AT2(Z,N),AT3(Z,N)
       real*8 decision(Z,N,nfac)
       LOGICAL ext 

       common/decision1/decision
       common/final/conclusion,RT1,RT2,RT3,RT4,AT1,AT2,AT3
       common /read_block/ n_nuclei,nmax,value_z,value_n
       common /read_abundance/ y      !read
       common /read_time/ temp,timestep,dummy,junk
       common /read_flux/ density,y_p,y_alpha
       common /read_stable/ num_stable,z_stable,n_stable
       common /abundance/ b,x,Bm,Xm
       common /main_path/ border,f
       common /loop2/ n_sum2
       common /each_result/ w,pam,weight
       common /lifetime/ r_eff,L_eff,Lb
       common /AT1/ M
       common /gap/ xm_new,new_x,ratio,M_ratio,place,n_zero,order
       common /loop1/ n_sum
       common /AT5/ x_creation
       common /center/ zo,no
       common/final_result/result
       common/keep/c_save

       common/integral/time
       common/read_reaction2/ flux1,flux2,flux3,flux4
     !,flux5,flux6,flux7,flux0
       common/time_new_reaction/new_reacc,new_reacd,reacc_beta
     !,reacd_beta
       common/time_flux/flux,flux_in,flux_out
       common/integral_of_flux/integral_flux,total_integflux
       common/integral_life/reac_eff,each_Lb,each_L,integ_L
       common/waiting_time/sum_flux,wt
       common/flux2/total_flux,total_fluxin,total_fluxout
       common/timestep_at_max/t_m
       common/comparison_totalflux/total_max,total_min
       common/timewindow_integral/time_initial,time_final
       common/peak_fluxin/fluxin_max,time_max
       common/read_crazy/Lb_crazy,num_crazy,z_crazy,n_crazy
       common/average/ave_y
       common/factor/nom_fac
c**************************************************************************
c       if(iargc() /= 1) then
c         write(*,*) 'USAGE: outcode inputfile'
c       endif

c       call getarg(1,buf)
c       open(1,file=buf,status='old')
       inquire(file='parameter.dat', EXIST=ext)
       if(ext .EQV. .FALSE.) call exit(1)
       open(10,file='parameter.dat',status='old')
       inquire(file='list_stable.dat', EXIST=ext)
       if(ext .EQV. .FALSE.) call exit(1)
       open(3,file='list_stable.dat',status='old')
       inquire(file='list_crazy.dat', EXIST=ext)
       if(ext .EQV. .FALSE.) call exit(1)
       open(12,file='list_crazy.dat',status='old')
c       open(11,file='results.out',status='replace')

       call initialize_loop1
       call read1

c***************************************************


        do 3100 k=1,Z
         do 3110 l=1,N
           if(ave_y(k,l).eq.0)then
             goto 3350
           else
             zo=k
             no=l
           endif

           call declaration

           call integration_flux
           call integration_lifetime
           call abundance_test
c           call major_path1
c           call row_test1_1
c           call integration_lifetime_AT
           call abundance_AT
           call lifetime_AT
           call flux_AT

           call logic_tree
c           call logic_tree2

           call output_fake
3350       continue
3110       continue
3100       continue

           call final_decision

c           print*,nom_fac(1)
           stop
         end
