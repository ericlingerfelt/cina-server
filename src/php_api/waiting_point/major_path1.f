       subroutine major_path1

       implicit none


       common/read_block/n_nuclei,nmax,value_z,value_n
       common /abundance/ b,x,Bm,Xm
       common/some_flux/flux_in,flux_out,flux,sum_in,sum_out,sum_flux
       common/main_path/border,f
       common /center/ zo,no

 

       integer*4 n_c,nmaxp,num,n_reac,n_pam,nfac
       parameter(n_c=6)
       parameter(nmaxp=1300)
       parameter(num=10)
       parameter(n_reac=3)
       parameter(n_pam=4)
       parameter(nfac=11)

       integer*4 nmax,n_nuclei,value_z,value_n
       integer*4 i
       real*8 flux(n_c+2)
       real*8 flux_in(n_c+1),flux_out(n_c+1)
       real*8 sum_in,sum_out,sum_flux
       real*8 x(num,nmaxp)
       real*8 xm(num)
       real*8 border
       integer*4 f(n_c+2,2)

       real*8 Bm
       real*8 b(nmaxp)
       integer*4 zo,no


           do 550 i=1,n_c+2
              if(flux(i).ge.border)then
                 f(i,2)=1
              else if(flux(i).le.(-border))then
                 f(i,2)=-1
              else
                 f(i,2)=0
              endif
550         continue

           do 560 i=1,n_c+2
              if(f(i,2).eq.0)then
                 xm(f(i,1))=0.0d0
              else
              endif
560         continue

             return
             end
