
       subroutine read1

       implicit none

       integer*4 nmaxp,Z,N,nfac,num,n_pam,n_c
       integer*4 max_stable
       parameter(nmaxp=1300)
       parameter(Z=55)
       parameter(N=55)
       parameter(max_stable=100)
       parameter(nfac=11)
       parameter(num=10)
       parameter(n_pam=4)
       parameter(n_c=6)

       integer*4 n_nuclei
       integer*4 nmax
       integer*4 value_z(nmaxp)
       integer*4 value_n(nmaxp)
       real*8 y(Z,N,nmaxp)       !read
       integer*4 timestep(nmaxp)
       real*8 temp(nmaxp)
       real*8 density(nmaxp)    !flux
       real*8 y_p(nmaxp)        !flux
       real*8 y_alpha(nmaxp)    !flux
       integer*4 num_stable    !number of stable nuclei
       integer*4 z_stable(max_stable)!stable
       integer*4 n_stable(max_stable)!stable
       integer*4 i,j,k,l
       integer*4 junk
       real*8 dummy

       real*8 time(nmaxp)
       real*8 flux1(Z,N,nmaxp)        
       real*8 flux2(Z,N,nmaxp)
       real*8 flux3(Z,N,nmaxp)
       real*8 flux4(Z,N,nmaxp)
       real*8 flux5(Z,N,nmaxp)
       real*8 flux6(Z,N,nmaxp)
       real*8 flux7(Z,N,nmaxp)
       real*8 flux0(Z,N,nmaxp)

       real*8 weight(nfac,3)
       real*8 w(nfac)
       real*8 pam(nfac,n_pam)
       integer*4 num_crazy
       integer*4 z_crazy(max_stable),n_crazy(max_stable)
       real*8 Lb_crazy(max_stable)
       real*8 border         !major path
       integer*4 f(n_c+2,2)  !major path
       real*8 ave_y(Z,N)
       real*8 nom_fac(nmaxp)

   
       common/read_block/n_nuclei,nmax,value_z,value_n
       common/read_abundance/y      !read
       common/read_flux/density,y_p,y_alpha
       common/read_stable/num_stable,z_stable,n_stable
       common/read_crazy/Lb_crazy,num_crazy,z_crazy,n_crazy
       common/integral/time       
       common/read_reaction2/ flux1,flux2,flux3,flux4
     !,flux5,flux6,flux7,flux0
       common /each_result/ w,pam,weight
       common /main_path/ border,f

       common/average/ave_y
       common/factor/nom_fac


c********************************************************************

        read(5,21)n_nuclei,nmax
21     format(i4/i4)

       if(nmaxp.lt.nmax)then
          print*,'Must be fewer than ',nmaxp,' timesteps'
          call exit(1);
        else
        endif


        do i=1,nmax
           read(5,31,end=12)timestep(i),time(i),temp(i),density(i)
     !,y_p(i),y_alpha(i),nom_fac(i)
          do j=1,n_nuclei
            read(5,8,end=10)value_z(j),value_n(j),
     !            y(value_z(j),value_n(j),i), 
     !            flux0(value_z(j),value_n(j),i),
     !            flux1(value_z(j),value_n(j),i),
     !            flux2(value_z(j),value_n(j),i),
     !            flux3(value_z(j),value_n(j),i),
     !            flux4(value_z(j),value_n(j),i),
     !            flux5(value_z(j),value_n(j),i),
     !            flux6(value_z(j),value_n(j),i),
     !            flux7(value_z(j),value_n(j),i)
c            write(*,*)j, ' - ', value_z(j), ',',value_n(j)
          enddo 
10       continue

        enddo
12      continue
        
33     format(1x,i2,2x,i2,1x,18(1pe12.6,2x),1pe12.6)
#ifdef NEW
31     format(i4,1x,e19.12,1X,4(e13.6,1x),e13.6)
8      format(i3,1x,i3,1x,e13.6,1x,7(e13.6,1x),e13.6)
#else
31     format(i4,1x,1pe18.12,1X,4(1pe12.6,1x),1pe12.6)
8      format(1x,i2,2x,i2,1x,1pe12.6,1x,7(1pe13.6,1x),1pe13.6)
#endif

c*****************
c average
c*****************

        do 215 j=1,Z
         do 225 k=1,N
          do 235 i=1,nmax
            ave_y(j,k)=ave_y(j,k)+y(j,k,i)
235        continue
225        continue
215       continue
c***************
c Stable Nuclei
c***************


         read(3,43)num_stable
43       format(i2)

         do i=1,num_stable
            read(3,45,end=47)z_stable(i),n_stable(i)
         enddo
47       continue

45       format(i2,1x,i2)

c**************
c crazy nuclei
c**************

         read(12,150)num_crazy
150      format(i2)

         do i=1,num_crazy
            read(12,170,end=160)z_crazy(i),n_crazy(i),Lb_crazy(i)
         enddo
160      continue
170      format(i2,1X,i2,2X,1pe8.2)

c**************
c parameter
c**************
         read(10,250)border
250      format(e12.6)

         do i=1,4
          read(10,260,end=270)pam(i,1)
         enddo
270      continue


260      format(e12.6)




         return
         end
