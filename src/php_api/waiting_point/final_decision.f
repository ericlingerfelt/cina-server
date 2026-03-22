       subroutine final_decision
      
       implicit none

 
       integer*4 nmaxp,nfac
       parameter(nmaxp=1300)        !number of time steps
       integer*4 Z  !the maximum number of protons
       integer*4 N  !the maximum number of neutrons
       parameter(Z=55)
       parameter(N=55)
       parameter(nfac=11)

       integer*4 i,j,k,l
       real*8 conclusion(Z,N)
       real*8 RT1(Z,N),RT2(Z,N),RT3(Z,N),RT4(Z,N)
       real*8 AT1(Z,N),AT2(Z,N),AT3(Z,N)
       integer*4 nmax
       integer*4 n_nuclei
       integer*4 value_z(nmaxp)
       integer*4 value_n(nmaxp)
       real*8 decision(Z,N,nfac)

       common/final/conclusion,RT1,RT2,RT3,RT4,AT1,AT2,AT3
       common /read_block/ n_nuclei,nmax,value_z,value_n
       common/decision1/decision


c          do j=1,n_nuclei
c            read(9,110,end=10)value_z(j),value_n(j),
c     !            conclusion(value_z(j),value_n(j)), 
c     !            RT1(value_z(j),value_n(j)),
c     !            RT2(value_z(j),value_n(j)),
c     !            RT3(value_z(j),value_n(j)),
c     !            RT4(value_z(j),value_n(j)),
c     !            AT1(value_z(j),value_n(j)),
c     !            AT2(value_z(j),value_n(j)),
c     !            AT3(value_z(j),value_n(j))
cc          enddo 
c10       continue

110    format(i2,1X,i2,2X,7(F4.2,3X),F4.2) 

       do 20 j=1,Z
          do 30 k=1,N
          if((decision(j,k,1).ne.1000).and.(decision(j,k,1).ne.0))then
             write(6,810)j,k,
     !            decision(j,k,1),
     !            decision(j,k,2),
     !            decision(j,k,3),
     !            decision(j,k,4),
     !            decision(j,k,5),
     !            decision(j,k,6),
     !            decision(j,k,7),
     !            decision(j,k,8)
           else
           endif
30        continue
20       continue

c       do 40 j=1,Z
c          do 50 k=1,N
c          if((decision(j,k,1).ne.1000).and.(decision(j,k,1).eq.0))then
c             write(11,810)j,k,
c     !            decision(j,k,1),
c     !            decision(j,k,2),
c     !            decision(j,k,3),
c     !            decision(j,k,4),
c     !            decision(j,k,5),
c     !            decision(j,k,6),
c     !            decision(j,k,7),
c     !            decision(j,k,8)
c           else
c           endif
c50        continue
c40       continue

     
810     format(i2,1X,i2,2X,7(F4.2,3X),F4.2)


       return
       end
