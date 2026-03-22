       subroutine abundance_AT

       implicit none


       integer*4 nmaxp
       parameter(nmaxp=1300)        !number of time steps
       integer*4 num            !the number of nuclei for comparison
       parameter(num=10)
       integer*4 nfac              !the number of factors (or conditions)
       parameter(nfac=11)
       integer*4 n_pam
       parameter(n_pam=4)

       real*8 b(nmaxp)            !abundance of B
       real*8 x(num,nmaxp)        !abundance of other nuclei for comparison
       real*8 Bm                 !the maximum value of B
       real*8 Xm(num)            !the maximu abundance of the nuclei
       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)

       common /abundance/ b,x,Bm,Xm
       common /each_result/ w,pam,weight


       if(bm.gt.1e-4)then
          w(7)=weight(7,3)
        else if(bm.gt.1e-4)then
           w(7)=weight(7,2)
        else
           w(7)=weight(7,1)
        endif

       return
       end
