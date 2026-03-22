       subroutine initialize_loop1  
!not yet common blocks for main initialized variables***************

       implicit none

       common/read_block/n_nuclei,nmax,value_z,value_n
       common/read_reaction/ r1,r2,r3,r4
     !,r5,r6,r7,r8,r9,r0,r10,r11
     !,r12,r13,old_Qbc,old_Qd,old_Qe,old_Ld
       common/read_abundance/y      !read
       common/read_flux/density,y_p,y_alpha
       common/read_stable/num_stable,z_stable,n_stable
       common /read_time/ temp,timestep,dummy,junk
       common/abundance/b,x,Bm,Xm
       common /reaction/ r,rc,rd,new_rc,new_rd,rc_beta,rd_beta,rc1
     !,rd1,rbc,rcb
       common/some_flux/flux_in,flux_out,flux,sum_in,sum_out,sum_flux
       common/main_path/border,f
       common/loop2/n_sum2
       common/lifetime/r_eff,L_eff,Lb
       common/Q_value/Qbc,Qd,Qe,Ld
       common/AT1/M
       common/loop1/n_sum
       common/AT5/x_creation
       common/final_result/result
       common/center/zo,no
       common /each_result/ w,pam,weight
       common /gap/ xm_new,new_x,ratio,M_ratio,place,n_zero,order
       common/integral/time
       common/read_reaction2/ reac1,reac2,reac3,reac4
     !,reac5,reac6,reac7,reac8,reac9,reac0,reac10,reac11
     !,reac12,reac13
       common/time_reaction/reacc,reacd
       common/time_new_reaction/new_reacc,new_reacd,reacc_beta
     !,reacd_beta
       common/time_flux/fluxt,fluxt_in,fluxt_out
       common/integral_of_flux/integral_flux,total_integflux
       common/integral_life/reac_eff,each_Lb,each_L,integ_L
       common/waiting_time/sum_fluxt,wt
       common/read_crazy/Lb_crazy,num_crazy,z_crazy,n_crazy

       integer*4 nmaxp,Z,N,max_stable,n_pam,num,n_reac,n_c,nfac
       parameter(nmaxp=1300)
       parameter(Z=24)
       parameter(N=30)
       parameter(max_stable=100)
       parameter(n_pam=4)
       parameter(num=10)
       parameter(n_reac=3)
       parameter(n_c=6)
       parameter(nfac=11)
c       parameter(n_trial=9)

       integer*4 n_nuclei
       integer*4 nmax
       integer*4 value_z(nmaxp)
       integer*4 value_n(nmaxp)
       integer*4 n_zero
       real*8 r1(Z,N)        
       real*8 r2(Z,N)
       real*8 r3(Z,N)
       real*8 r4(Z,N)
       real*8 r5(Z,N)
       real*8 r6(Z,N)
       real*8 r7(Z,N)
       real*8 r8(Z,N)
       real*8 r9(Z,N)
       real*8 r0(Z,N)
       real*8 r10(Z,N)
       real*8 r11(Z,N)
       real*8 r12(Z,N)
       real*8 r13(Z,N)
       real*8 old_Qbc(Z,N)
       real*8 old_Qd(Z,N)
       real*8 old_Qe(Z,N)
       real*8 old_Ld(Z,N)
       real*8 y(Z,N,nmaxp)       !read
       real*8 density(nmaxp)    !flux
       real*8 y_p(nmaxp)        !flux
       real*8 y_alpha(nmaxp)    !flux
       integer*4 num_stable    !number of stable nuclei
       integer*4 z_stable(max_stable)!stable
       integer*4 n_stable(max_stable)!stable
       integer*4 i,j,k,l
       integer*4 timestep(nmaxp)
       real*8 temp(nmaxp)
       integer*4 junk
       real*8 dummy
c*********************************************
       integer*4 zo !the number of protons
       integer*4 no !the number of neutrons 
       integer*4 n_sum2
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
       real*8 Lb     !lifetime of B[sec] (w5)
       real*8 r_eff  !effective reaction rate for effective lifetime
       real*8 L_eff
       real*8 Ld     !lifetime of D
       real*8 Qe     !theQ-value of x(10)
       real*8 Qbc                 !Q-value from B to C
       real*8 Qd                  !Q value for proton capture of D
       real*8 rbc
       real*8 rcb                         !reaction rate of the beta-decay
       real*8 r(n_reac)              !other n_reac rates from B to X
       real*8 rc(n_c)      !the input reaction rate
       real*8 rd(n_c)     !the output reaction rate
       real*8 rc_beta
       real*8 rd_beta
       real*8 Rc1             !the maximum reaction rate of input
       real*8 Rd1             !the maximu reaction rate of output
       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)
       real*8 result
       real*8 new_rc(n_c)        !flux &AT2
       real*8 new_rd(n_c)        !flux &AT2
       real*8 r_big(n_reac)   !for description
       real*8 flux_in(n_c+1)     !flux
       real*8 flux_out(n_c+1)    !flux
       real*8 flux(n_c+2)        !flux      
       real*8 sum_in             !flux
       real*8 sum_out            !flux
       real*8 sum_flux           !flux

       real*8 border         !major path
       integer*4 f(n_c+2,2)  !major path

       real*8 x_creation(nmaxp) !for AT5

c       integer*4 turn_on(nfac)    !option
c       character*10  nuc(n_trial)

       real*8 time(nmaxp)
       real*8 reac1(Z,N,nmaxp)        
       real*8 reac2(Z,N,nmaxp)
       real*8 reac3(Z,N,nmaxp)
       real*8 reac4(Z,N,nmaxp)
       real*8 reac5(Z,N,nmaxp)
       real*8 reac6(Z,N,nmaxp)
       real*8 reac7(Z,N,nmaxp)
       real*8 reac8(Z,N,nmaxp)
       real*8 reac9(Z,N,nmaxp)
       real*8 reac0(Z,N,nmaxp)
       real*8 reac10(Z,N,nmaxp)
       real*8 reac11(Z,N,nmaxp)
       real*8 reac12(Z,N,nmaxp)
       real*8 reac13(Z,N,nmaxp)

       real*8 reacc(n_c,nmaxp)
       real*8 reacd(n_c,nmaxp)
       real*8 reacc_beta(nmaxp)
       real*8 reacd_beta(nmaxp)
       real*8 new_reacc(n_c,nmaxp)
       real*8 new_reacd(n_c,nmaxp)

       real*8 fluxt_in(n_c+1,nmaxp)
       real*8 fluxt_out(n_c+1,nmaxp)
       real*8 fluxt(n_c+2,nmaxp)
       real*8 integral_flux(n_c+2)
       real*8 total_integflux
       real*8 reac_eff(nmaxp)
       real*8 each_Lb(nmaxp)
       real*8 each_L(nmaxp)
       real*8 integ_L
       real*8 sum_fluxt(nmaxp)
       real*8 wt
       integer*4 num_crazy
       integer*4 z_crazy(max_stable),n_crazy(max_stable)
       real*8 Lb_crazy(max_stable)
c***********************************************************************


       Qbc=0.0
       Qd=0.0
       Lb=0.0
       r_eff=0.0
       L_eff=0.0
       Ld=0.0
       n_sum2=0
       n_sum(1)=0
       Bm=0       
       rbc=0.0d0
       rcb=0.0d0
       rc_beta=0.0d0
       rd_beta=0.0d0
       result=0.0
       num_stable=0
       num_crazy=0
       n_nuclei=0
       nmax=0
       wt=0.0d0

          do 3005 k=1,Z
          do 3010 l=1,N
             r1(k,l)=0.0d0
             r2(k,l)=0.0d0
             r3(k,l)=0.0d0
             r4(k,l)=0.0d0
             r5(k,l)=0.0d0
             r6(k,l)=0.0d0
             r7(k,l)=0.0d0
             r8(k,l)=0.0d0
             r9(k,l)=0.0d0
             r0(k,l)=0.0d0
             r10(k,l)=0.0d0
             r11(k,l)=0.0d0
             r12(k,l)=0.0d0
             r13(k,l)=0.0d0
             old_Qbc(k,l)=0.0d0
             old_Qd(k,l)=0.0d0
             old_Qe(k,l)=0.0d0

             do 3015 i=1,nmaxp
                y(k,l,i)=0.0d0
3015         continue
3010      continue
3005   continue

       do 10 i=1,nmaxp
          value_z(i)=0.0d0
          value_n(i)=0.0d0
          timestep(i)=0.0d0
          temp(i)=0.0d0
          density(i)=0.0d0
          y_p(i)=0.0d0
          y_alpha(i)=0.0d0
          time(i)=0.0d0
10       continue


         do 4000 i=1,max_stable
            z_stable(i)=0
            n_stable(i)=0
            z_crazy(i)=0
            n_crazy(i)=0
            Lb_crazy(i)=0.0d0
4000       continue

       do 3 i=1,nmaxp 
         b(i)=0.0d0
3      continue         

       do 5 j=1,num
           Xm(j)=0.0d0
           n_sum(j+1)=0
           order(j)=0
5      continue
          
       do 7 i=1,nmaxp
          x_creation(i)=0.0d0
          do 9 j=1,num
             x(j,i)=0.0d0
9        continue
7      continue

       do 11 i=1,nfac
          do 13 j=1,3
             pam(i,j)=0.0d0
             weight(i,j)=0.0d0
13       continue
11     continue

       do 15 i=1,n_reac
          r(i)=0.0d0
          r_big(i)=0.0d0
15     continue

  
         do 605 j=1,n_c
          rc(j)=0.0d0
          rd(j)=0.0d0
          flux(j)=0.0d0
605    continue

         sum_in=0.0d0
         sum_out=0.0d0

        do 3505 i=1,n_c+1
           flux_in(i)=0.0d0
3505    continue

         do 3510 i=1,n_c
            flux_out(i)=0.0d0
3510     continue


       border=1.0E-10
       do 80 i=1,n_c+2
          f(i,1)=0
          f(i,2)=0
80       continue


        do 75 j=1,nmaxp
           fluxt(1,j)=0.0d0
          do 65 i=1,n_c+1
           fluxt(i+1,j)=0.0d0
           fluxt_in(i,j)=0.0d0
           fluxt_out(i,j)=0.0d0
65        continue
75       continue

       do 105 i=1,nmaxp
          time(i)=0.0d0
          sum_fluxt(i)=0.0d0
        do 115 j=1,Z
         do 125 k=1,N
            reac0(j,k,i)=0.0d0
            reac1(j,k,i)=0.0d0
            reac2(j,k,i)=0.0d0
            reac3(j,k,i)=0.0d0
            reac4(j,k,i)=0.0d0
            reac5(j,k,i)=0.0d0
            reac6(j,k,i)=0.0d0
            reac7(j,k,i)=0.0d0
            reac8(j,k,i)=0.0d0
            reac9(j,k,i)=0.0d0
            reac10(j,k,i)=0.0d0
            reac11(j,k,i)=0.0d0
            reac12(j,k,i)=0.0d0
            reac13(j,k,i)=0.0d0
125        continue
115       continue
105       continue

c**************************************************************
         f(1,1)=3
         f(2,1)=7
         f(3,1)=8
         f(4,1)=4
         f(5,1)=1
         f(6,1)=5
         f(7,1)=6
         f(8,1)=2

       do 710 j=1,nfac
        weight(j,1)=0.0
        weight(j,2)=0.5
        weight(j,3)=1.0
710    continue

        
          return
          end
