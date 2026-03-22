       subroutine abundance_test
     
       implicit none

       common /abundance/ b,x,Bm,Xm
       common /each_result/ w,pam,weight

       integer*4 nmaxp,num,nfac,n_pam
       parameter(nmaxp=1300)        !number of time steps
       parameter(num=10)
       parameter(nfac=11)
       parameter(n_pam=4) 
       real*8 b(nmaxp)            !abundance of B
       real*8 x(num,nmaxp)        !abundance of other nuclei for comparison 
       real*8 Bm                 !the maximum value of B
       real*8 Xm(num)            !the maximu abundance of the nuclei
       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)

       if(bm.gt.pam(3,1))then
          w(3)=weight(3,3)
       else
          w(3)=weight(3,1)
       endif
       return
       end
