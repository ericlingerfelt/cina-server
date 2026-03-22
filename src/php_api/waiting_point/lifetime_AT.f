       subroutine lifetime_AT

       implicit none

       integer*4 nmaxp
       parameter(nmaxp=1300)        !number of time steps
       integer*4 nfac              !the number of factors (or conditions)
       parameter(nfac=11)
       integer*4 n_pam
       parameter(n_pam=4)

       integer*4 nmax
       integer*4 n_nuclei
       integer*4 value_z(nmaxp)
       integer*4 value_n(nmaxp)
       real*8 reac_eff(nmaxp)
       real*8 each_Lb(nmaxp)
       real*8 each_L(nmaxp)
       real*8 integ_L
       real*8 time(nmaxp)
       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)

       common /read_block/ n_nuclei,nmax,value_z,value_n
       common/integral_life/reac_eff,each_Lb,each_L,integ_L
       common/integral/time
       common /each_result/ w,pam,weight

       if(integ_L/(time(nmax)-time(1)).gt.1)then
          w(6)=weight(6,3)
        else if(integ_L/(time(nmax)-time(1)).gt.0.2)then
           w(6)=weight(6,2)
        else
           w(6)=weight(6,1)
        endif


       return 
       end
