       subroutine write_result

       implicit none



       common /abundance/ b,x,Bm,Xm
       common /center/ zo,no
       common/integral_of_flux/integral_flux,total_integflux
       common/integral_life/reac_eff,each_Lb,each_L,integ_L
       common /read_flux/ density,y_p,y_alpha
       common /read_block/ n_nuclei,nmax,value_z,value_n
       common/integral/time
       common/comparison_totalflux/total_max,total_min
       common /each_result/ w,pam,weight



       integer*4 nmaxp,num,n_c
       parameter(nmaxp=1300)
       parameter(num=10)
       parameter(n_c=6)
       integer*4 n_pam
       parameter(n_pam=4)
       integer*4 nfac              !the number of factors (or conditions)
       parameter(nfac=11)
       integer*4 n_nuclei,nmax,value_z,value_n
       real*8 b(nmaxp)
       real*8 x(num,nmaxp)
       real*8 bm
       real*8 xm(num)
       real*8 integral_flux(n_c+2)
       real*8 total_integflux
       real*8 reac_eff(nmaxp)
       real*8 each_Lb(nmaxp)
       real*8 each_L(nmaxp)
       real*8 integ_L
       real*8 density(nmaxp)
       real*8 y_p(nmaxp)
       real*8 y_alpha(nmaxp)
       integer*4 zo,no
       real*8 time(nmaxp)
       real*8 total_max,total_min

       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)
c*********************************
c old write statement
c*********************************

c********************************
c new write statement
c********************************

       write(9,100)zo,no 
100    format(i2,1X,i2,1X)

       if((Bm.gt.pam(3,1)).and.(total_max+pam(1,1)*total_min.gt.0)
     !.and.(integ_L.gt.pam(2,1)*(time(nmax)-time(1))))then
         write(9,110)Bm,integ_L/(time(nmax)-time(1)),total_max,total_min
     !,total_integflux
 110     format('IS A WAITING POINT***********************'/
     !          'abundance',1pe12.6/
     !          'lifetime',1pe12.6/
     !          'max&min of flux',1pe12.6,1X,1pe12.6/
     !          'integration of flux',1pe12.6)
       else
         write(9,120)Bm,integ_L/(time(nmax)-time(1)),total_max,total_min
     !,total_integflux
 120     format('is not a waiting point.'/
     !          'abundance',1pe12.6/
     !          'lifetime',1pe12.6/
     !          'max&min of flux',1pe12.6,1X,1pe12.6/
     !          'integration of flux',1pe12.6)
       endif


       return 
       end
