       subroutine logic_tree

       implicit none


       common/integral/time
       common/abundance/b,x,Bm,Xm
       common/read_block/n_nuclei,nmax,value_z,value_n
       common/center/zo,no
       common/time_flux/flux,flux_in,flux_out
       common/integral_of_flux/integral_flux,total_integflux
       common/waiting_time/sum_flux,wt
       common /each_result/ w,pam,weight
       common/flux2/total_flux,total_fluxin,total_fluxout
       common/comparison_totalflux/total_max,total_min
       common/timewindow_integral/time_initial,time_final
       common/peak_fluxin/fluxin_max,time_max
       common /loop2/ n_sum2
       common/integral_life/reac_eff,each_Lb,each_L,integ_L


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
       integer*4 time_initial,time_final,time_max
       real*8 fluxin_max
       integer*4 n_sum2
       real*8 reac_eff(nmaxp)
       real*8 each_Lb(nmaxp)
       real*8 each_L(nmaxp)
       real*8 integ_L

c       print*,zo ,no ,Bm,integ_L/time(nmax)-time(1),total_max,total_min

       if((total_max+total_min.gt.1.0e-6).and.(bm.gt.1e-6).and.
     !(w(2).eq.1))then
          if(total_integflux.gt.1.0e-4)then
c             print*,zo ,no ,' HAS A POSITIVE TOTAL FLUX',w(5),w(6),w(7)
c             print*,'values',bm,integ_L/(time(nmax)-time(1)),each_Lb(1),
c     !total_max,total_min,total_integflux
             w(4)=weight(4,3)
          else
             if(b(1).lt.b(nmax))then
c                print*,zo ,no ,' Warning!!'
c                print*,'the initial abundance',b(1)
c                print*,'the final abundance',b(nmax)
c                print*,'the maximum of total flux',total_max
c                print*,'the minimum total flux',total_min
c                print*,'the integration of total flux',total_integflux
                if((b(nmax)-b(1)).gt.1e-4) then
                  w(4)=weight(4,3)
                else 
                  w(4)=weight(4,1)
                endif
             else 
                if(bm.eq.b(1))then
c                  print*,zo ,no ,' Reject-always decreasing'
                  w(4)=weight(4,1)
                else
                     if(Bm.LT.1.2*b(1))then
c                        print*,zo ,no ,' Reject-oscillating'
                        w(4)=weight(4,1)
                     else
c                        print*,zo ,no ,' possible waiting point-new
c     !window'
                        w(4)=weight(4,2)
                     endif
                endif
            endif
           endif
       else
       endif

c       if(integ_L/(time(nmax)-time(1)).lt.1e-5)then
c             print*,zo ,no ,integ_L/(time(nmax)-time(1))
c         else
c         endif

       return
       end
