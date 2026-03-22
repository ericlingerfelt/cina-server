       subroutine declaration

       implicit none
c*****************************************************


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

       real*8 border         !major path
       integer*4 f(n_c+2,2)  !major path

       real*8 x_creation(nmaxp) !for AT5

       integer*4 turn_on(nfac)    !option
       character*10  nuc(n_trial)
       integer*4 n_zero

       real*8 time(nmaxp)

       real*8 reacc_beta(nmaxp)
       real*8 reacd_beta(nmaxp)
       real*8 new_reacc(n_c,nmaxp)
       real*8 new_reacd(n_c,nmaxp)
       real*8 flux1(Z,N,nmaxp)        
       real*8 flux2(Z,N,nmaxp)
       real*8 flux3(Z,N,nmaxp)
       real*8 flux4(Z,N,nmaxp)
       real*8 flux5(Z,N,nmaxp)
       real*8 flux6(Z,N,nmaxp)
       real*8 flux7(Z,N,nmaxp)
       real*8 flux0(Z,N,nmaxp)
       real*8 flux_in(n_c+1,nmaxp)
       real*8 flux_out(n_c+1,nmaxp)
       real*8 flux(n_c+2,nmaxp)
       real*8 integral_flux(n_c+2)
       real*8 total_integflux
       real*8 reac_eff(nmaxp)
       real*8 each_Lb(nmaxp)
       real*8 each_L(nmaxp)
       real*8 integ_L
       integer*4 t_m
       integer*4 num_stable,z_stable(max_stable),n_stable(max_stable)
       real*8 nom_fac(nmaxp)

       common /read_block/ n_nuclei,nmax,value_z,value_n
       common /read_abundance/ y      !read
       common /abundance/ b,x,Bm,Xm
       common /main_path/ border,f
       common /loop2/ n_sum2
       common /each_result/ w,pam,weight
       common /lifetime/ r_eff,L_eff,Lb
       common /gap/ xm_new,new_x,ratio,M_ratio,place,n_zero,order
       common /loop1/ n_sum
       common /AT5/ x_creation
       common /center/ zo,no
       common/final_result/result
       common/time_new_reaction/new_reacc,new_reacd,reacc_beta
     !,reacd_beta
       common/read_reaction2/ flux1,flux2,flux3,flux4
     !,flux5,flux6,flux7,flux0
       common/integral/time

       common/time_flux/flux,flux_in,flux_out
       common/integral_of_flux/integral_flux,total_integflux
       common/integral_life/reac_eff,each_Lb,each_L,integ_L
       common/timestep_at_max/t_m
       common /read_stable/ num_stable,z_stable,n_stable
       common/factor/nom_fac

 
c*****************************************************

       r_eff=0.0
       L_eff=0.0
       n_sum2=0
       n_sum(1)=0
       Bm=0  
       result=0.0
       integ_L=0.0d0

       do 3 i=1,nmax 
         b(i)=0.0d0
3      continue         

       do 5 j=1,num
           Xm(j)=0.0d0
           n_sum(j+1)=0
           order(j)=0
5      continue
          
       do 7 i=1,nmax
          x_creation(i)=0.0d0
          do 9 j=1,num
             x(j,i)=0.0d0
9        continue
7      continue

         do 900 i=1,nfac
            w(i)=0.0d0
900        continue


       border=1.0E-10
       do 80 i=1,n_c+2
          f(i,2)=0
80       continue      
       


        do 75 j=1,nmax
           flux(1,j)=0.0d0
           reac_eff(j)=0.0d0
           each_L(j)=0.0d0
           each_Lb(j)=0.0d0
          do 65 i=1,n_c+1
           flux(i+1,j)=0.0d0
           flux_in(i,j)=0.0d0
           flux_out(i,j)=0.0d0
65        continue
75       continue



        do 3020 i=1,nmax  
          b(i)=y(zo,no,i)
          x(1,i)=y(zo+1,no,i)
          x(2,i)=y(zo-1,no+1,i)
          x(3,i)=y(zo-1,no,i)
          x(4,i)=y(zo+1,no+2,i)
          x(5,i)=y(zo+2,no+2,i)
          x(6,i)=y(zo+1,no-1,i)
          x(7,i)=y(zo-1,no-2,i)
          x(8,i)=y(zo-2,no-2,i)
          x(9,i)=y(zo+2,no-1,i)
          x(10,i)=y(zo,no-2,i)
3020   continue



       Bm=0.0d0
       do 50 i=1,nmax
            if(Bm.LT.b(i))then
               Bm=b(i)
               t_m=i
           else
           endif
50     continue

       do 60 j=1,num 
          do 55 i=1,nmax
             if(Xm(j).LT.x(j,i))then
              Xm(j)=x(j,i)
             else
             endif
55        continue      
60     continue


c*****************
c Integration
c*****************
       do 105 i=1,nmax                  
         reacc_beta(i)=flux0(zo,no,i)  !this is actually a reaction rate
         flux(1,i)=flux1(zo,no,i)
         flux(2,i)=flux2(zo,no,i)
         flux(3,i)=flux3(zo,no,i)
         flux(4,i)=flux4(zo,no,i)
         flux(5,i)=flux5(zo,no,i)
         flux(6,i)=flux6(zo,no,i)
         reacd_beta(i)=flux7(zo,no,i)    !this is also a rate!
         each_Lb(i)=0.69314718/reacd_beta(i)
105     continue


        do 120 i=1,nmax
         flux(7,i)=reacc_beta(i)*x(6,i)/nom_fac(1)
         flux(8,i)=-reacd_beta(i)*B(i)/nom_fac(1)
120     continue

         Lb=0.69314718/reacd_beta(1)

      

        do 110 i=1,n_c
         do 130 j=1,nmax
            if(flux(i,j).le.0)then
              new_reacd(i,j)=-flux(i,j)/b(i)
            else
            endif
130          continue
110     continue



c**********************************
c For Stable Nuclei
c**********************************
        do 300 i=1,num_stable
           if((zo.eq.z_stable(i)).and.(no.eq.n_stable(i)))then
              Lb=5000
           do 310 j=1,nmax
              each_Lb(i)=5000
310          continue
           else 
           endif
300      continue

c***********************************************************
        if(reacd_beta(1).lt.0.69314718/5000)then
           Lb=5000
           do 320 i=1,nmax
             each_Lb(i)=5000
320        continue
         else
         endif



         return
         end
