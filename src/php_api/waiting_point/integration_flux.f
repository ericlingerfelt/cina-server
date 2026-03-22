       subroutine integration_flux

       implicit none

       common/integral/time
       common/abundance/b,x,Bm,Xm
       common/read_block/n_nuclei,nmax,value_z,value_n
       common/read_flux/density,y_p,y_alpha
       common/center/zo,no
       common/time_flux/flux,flux_in,flux_out
       common/integral_of_flux/integral_flux,total_integflux
       common/waiting_time/sum_flux,wt
       common /each_result/ w,pam,weight
       common/flux2/total_flux,total_fluxin,total_fluxout
       common/comparison_totalflux/total_max,total_min
       common/timewindow_integral/time_initial,time_final


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
       real*8 flux_in(n_c+1,nmaxp)
       real*8 flux_out(n_c+1,nmaxp)
       real*8 flux(n_c+2,nmaxp)
       real*8 integral_flux(n_c+2)
       real*8 total_integflux
       real*8 sum_flux(nmaxp)
       real*8 wt
       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)
       real*8 total_flux(nmaxp)
       real*8 total_fluxin(nmaxp)
       real*8 total_fluxout(nmaxp)
       real*8 total_max,total_min
       integer*4 time_initial,time_final
c********************
c initialization
c********************

c**********************
c integration of flux
c**********************
          total_integflux=0.0d0
          total_max=0.0d0
          total_min=0.0d0
          time_initial=0
          time_final=0

        do 130 i=1,nmax
           total_flux(i)=0.0d0
           total_fluxin(i)=0.0d0
           total_fluxout(i)=0.0d0
130       continue

        do 145 i=1,n_c+2
          integral_flux(i)=0.0d0
145      continue

         do 180 i=1,nmax
          do 170 j=1,n_c+2
            total_flux(i)=total_flux(i)+flux(j,i)
170        continue
180      continue

        do 150 i=2,nmax
           total_integflux=total_integflux+(total_flux(i-1)
     !+total_flux(i))*(time(i)-time(i-1))/2
150       continue

c***************************
c Use the initial abundance and the final abundance to calculate the integration of total flux
c***************************
c          total_integflux=b(nmax)-b(1)
c************************************************************************

          do 250 i=1,nmax
             if(total_max.lt.total_flux(i))then
                total_max=total_flux(i)
                time_initial=i
             else
             endif
             
             if(total_min.gt.total_flux(i))then
                total_min=total_flux(i)
                time_final=i
             else
             endif
250         continue


       if(total_max+pam(1,1)*total_min.ge.0)then
          w(1)=weight(1,3)
       else
          w(1)=weight(1,1)
       endif




c*******************
c waiting time     I should change a lot!
c*******************

         wt=0
         do 190 i=2,nmax
            if((sum_flux(i).gt.0).and.(sum_flux(i-1).lt.0))then
               wt=time(i)
            else
            endif
190       continue

       return
       end





