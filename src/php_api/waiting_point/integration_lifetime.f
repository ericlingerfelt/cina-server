       subroutine integration_lifetime

       implicit none

       common/integral/time
       common/abundance/b,x,Bm,Xm
       common/read_block/n_nuclei,nmax,value_z,value_n
       common/read_flux/density,y_p,y_alpha
       common/center/zo,no
       common/time_new_reaction/new_reacc,new_reacd,
     !reacc_beta,reacd_beta
       common/integral_life/reac_eff,each_Lb,each_L,integ_L
       common /each_result/ w,pam,weight
       common /lifetime/ r_eff,L_eff,Lb


       
       integer*4 nmaxp,Z,N,n_c,num,n_pam,nfac
       parameter(nmaxp=1300)
       parameter(Z=55)
       parameter(N=55)
       parameter(n_c=6)
       parameter(num=10)
       parameter(n_pam=4)
       parameter(nfac=11)
       integer*4 i,j,k,l
       integer*4 zo,no
       real*8 time(nmaxp)                             
       integer*4 n_nuclei,nmax
       integer*4 value_z(nmaxp),value_n(nmaxp)
       real*8 density(nmaxp),y_p(nmaxp),y_alpha(nmaxp)
       real*8 x(num,nmaxp)
       real*8 b(nmaxp)
       real*8 bm
       real*8 xm(num)
       real*8 new_reacc(n_c,nmaxp)
       real*8 new_reacd(n_c,nmaxp)
       real*8 reacc_beta(nmaxp)
       real*8 reacd_beta(nmaxp)
       real*8 reac_eff(nmaxp)
       real*8 each_Lb(nmaxp)
       real*8 each_L(nmaxp)
       real*8 integ_L

       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)
       real*8 Lb     !lifetime of B[sec] (w5)
       real*8 r_eff  !effective reaction rate for effective lifetime
       real*8 L_eff
       
c********************
c initialization
c********************

c**********************
c integration of flux
c**********************


       do 120 j=1,nmax
          do 110 i=1,n_c
           reac_eff(j)=reac_eff(j)+new_reacd(i,j)
110       continue
120      continue

       do 130 i=1,nmax
          each_L(i)=1/((1/each_Lb(i))+reac_eff(i))
130      continue
       
       integ_L=0.0d0
       do 140 i=2,nmax
         integ_L=integ_L+(each_L(i-1)+each_L(i))*(time(i)-time(i-1))/2   
140     continue



        if(integ_L.gt.pam(2,1)*(time(nmax)-time(1)))then
           w(2)=weight(2,3)
        else
           w(2)=weight(2,1)
        endif

       return
       end





