       subroutine flux_AT

       implicit none


       integer*4 nmaxp
       parameter(nmaxp=1300)        !number of time steps
       integer*4 nfac              !the number of factors (or conditions)
       parameter(nfac=11)
       integer*4 n_c       ! number of nuclei for creation(w7)
       parameter(n_c=6)
       integer*4 n_trial
       integer*4 n_pam
       parameter(n_pam=4)

       real*8 time(nmaxp)
       real*8 total_max,total_min
       real*8 integral_flux(n_c+2)
       real*8 total_integflux
       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)
       integer*4 nmax
       integer*4 n_nuclei
       integer*4 value_z(nmaxp)
       integer*4 value_n(nmaxp)

       common/integral/time
       common/comparison_totalflux/total_max,total_min
       common/integral_of_flux/integral_flux,total_integflux
       common /each_result/ w,pam,weight
       common /read_block/ n_nuclei,nmax,value_z,value_n

       if(total_integflux.gt.(0.5)*total_max*(time(nmax)-time(1)))then
          w(5)=weight(5,3)
        else if(total_integflux.gt.(0.1)*total_max*
     !(time(nmax)-time(1)))then
           w(5)=weight(5,2)
        else
           w(5)=weight(5,1)
        endif

       return
       end
