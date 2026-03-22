       subroutine output_fake
      
       implicit none


       common/final_result/result
       common/center/zo,no     
       common /each_result/ w,pam,weight
       common/decision1/decision
       common /read_block/ n_nuclei,nmax,value_z,value_n
       
       integer*4 nfac,n_pam
       parameter(nfac=11)
       parameter(n_pam=4)
       integer*4 nmaxp
       parameter(nmaxp=1300)        !number of time steps
       integer*4 Z  !the maximum number of protons
       integer*4 N  !the maximum number of neutrons
       parameter(Z=55)
       parameter(N=55)

       real*8 w(nfac)
       real*8 weight(nfac,3)
       real*8 pam(nfac,n_pam)
       integer*4 i,j,k,l
       integer*4 nmax
       integer*4 n_nuclei
       integer*4 value_z(nmaxp)
       integer*4 value_n(nmaxp)
       real*8 result
       integer*4 zo
       integer*4 no
       real*8 decision(Z,N,nfac)

       result=0.0
       result=w(1)*w(2)*w(3)*w(4)*(w(5)+w(6)+w(7))

       decision(zo,no,1)=result

       do 10 i=1,7
         decision(zo,no,i+1)=w(i)
10      continue
               

       return
       end
