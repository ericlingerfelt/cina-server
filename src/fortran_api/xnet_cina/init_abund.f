C
C
C   $Author: bucknerk $
C   $Log: init_abund.f,v $
C   Revision 1.4  2008/07/25 16:53:30  bucknerk
C   fixing i hope
C
C   Revision 1.2  2008/07/03 20:36:47  bucknerk
C   adjusted files and tweaked comments. I think it is all compiled now. I have to test it but that can wait till Monday.
C
C
C
C   These are the updated versions of xnet from Rafe Hix via Luke Roberts.
C   They include code for two sparse solvers and the original 'dense' version of
C   the matrix code.
C
C
C
c----------------------------------------------------------------------
c  Initial Abundances
c
c  This program prepares a set of initial abundances for the network
c----------------------------------------------------------------------
       program init_abund
c----------------------------------------------------------------------
c  The program provides a number of options for calculating initial 
c  abundances.
c    1) Solar (including enhancements)
c    2) Ye limited (specify to nuclei and the value of Ye)
c    3) Mixtures of known compositions
c----------------------------------------------------------------------
      use nuclear_data
      real(8), dimension(:), allocatable :: y
      character (LEN=80) :: data_dir,data_desc,out_file,out_desc
c-----
c  Determine the set of nuclei for which abundances are desired
      Write(6,"(a)") "Specify Nuclear Data Directory"
      Read(5,"(a80)") data_dir
c-----
c  Read in the Nuclear Data
      idout=6
      call read_nuclear_data(data_dir,data_desc)
      Allocate(y(ny))
c-----
c  Select the Abundance Calculation Method
      Do 
        Write(6,"(a)") 
     &    "Select the Method for Calculating the Initial Abundances"
        Write(6,"(a)") "1) Scaled Solar"
        Write(6,"(a)") "2) For 2 species with given Ye"
        Write(6,"(a)") "3) Mixture of known compostions"
        Read(5,"(i1)") imethod
        Select Case (imethod)
          Case(1)
            call solar_abund(y,out_desc)
          Case(2)
            call fixed_ye(y,out_desc)
c         Case(3)
c           call mix_abund(y,out_desc)
          Case Default
            Write(6,"(i2,a)") imethod,
     &          " is an invalid selection. Try Again."
            Cycle
        End Select
        Exit
      Enddo
      Write(6,"(a)") "Select filename for output."
      Read(5,"(a80)") out_file
c     Write(6,"(a)") "Provide one line description of abundance set."
c     Read(5,"(a80)") out_desc
      Open(9,file=out_file)
      Write(9,"(a80)") out_desc
      Write(9,"(4(a5,es14.7,1x))") (nname(i),y(i),i=1,ny)
      Stop
      End

      Subroutine solar_abund(y,out_desc)
c-----------------------------------------------------------------------------
c  This routine calculates abundance based on the solar abundances, read in 
c  from file sol2d.
c-----------------------------------------------------------------------------
      use nuclear_data
      character(len=5) :: name
      character(len=80) :: out_desc
      real(8), dimension(ny) :: y
      real(8)  :: ytest,zscale,scale_factor
      integer :: ztest
c-----
c  Initialize the abundances
      y=0.0
c-----
c  Read in the solar abundance data, selecting only those abundances 
c  included in our nuclear set.
      Open(8,file="sol2d")
      Do i=1,300 
        Read(8,"(a5,6x,es14.7)",END=15) name,ytest
        call index_from_name(name,itest)
        If(itest/=0) y(itest)=ytest
      Enddo
   15 Close(8)
c-----
c  Inquire about scaling
      Write(6,"(a)") 
     &"This program allows you to scale (by a multiplicative factor)" 
      Write(6,"(a)") "the abundances with Z > Zscale."
      Write(6,"(a)") "Scale_Factor?"
      Read(5,*) scale_factor
      Write(6,"(a)") "Zscale?"
      Read(5,*) ztest
      zscale=float(ztest)
      Write(out_desc,"(a,es9.3,a,i3)")  'Solar Abundances scaled by ',
     &      scale_factor,' for Z > ',ztest
c----
c  Scale the abundances and renormalize
      Where(zz>zscale)
        y=scale_factor*y
      Endwhere
      call norm(y)
      Return
      End

      Subroutine fixed_ye(y,out_desc)
c-----------------------------------------------------------------------------
c  This routine calculates abundances for 2 user specified species from 
c  a user supplied Ye.  All other abundaces are set to 0.0.
c-----------------------------------------------------------------------------
      use nuclear_data
      character(len=5) :: name
      character(len=80) :: out_desc
      real(8), dimension(ny) :: y
c-----
c  Initialize the abundances
      y=0.0
c-----
c  Loop in case of invalid selections
      Do 
c-----
c  Select the 2 nuclei with non-zero abundance 
        Write(6,"(a)") 
     &      'Select the first species by name (si28 for example)'
        Read(5,"(a)") name
        call index_from_name(name,i1)
        If(i1==0) Then
          Write(6,"(a,a)") name,' not in selected nuclear set!'
          Cycle
        Endif
        Write(6,"(a)") 'Select the second species (si30 for example)'
        Read(5,"(a)") name
        call index_from_name(name,i2)
        If(i2==0) Then
            Write(6,"(a,a)") name,' not in selected nuclear set!'
            Cycle
        Endif
        If(i1==i2) Then
            Write(6,"(a)") 'Duplicate nuclei selected!'
            Cycle
        Endif
c-----
c  Select the value of Ye
        Write(6,"(a)") 'Select the Value of Ye'
        Read(5,*) ye
        y(i1)=(zz(i2)-aa(i2)*ye)/(aa(i1)*zz(i2)-aa(i2)*zz(i1))
        y(i2)=(1.0-aa(i1)*y(i1))/aa(i2)
        If(y(i1)>=0.0.and.y(i2)>=0.0) Then
            Exit
        Else
            Write(6,"(a,f5.4,a,a,a,a)") 
     &       'Ye=',ye,' is impossible with ',nname(i1),' and ',nname(i2)
            Cycle
        Endif
      Enddo
      Write(out_desc,"(a,a,a,a,a,f5.4)") 'Mix of',nname(i1),' and ',
     &      nname(i2),' with Ye= ',ye 
      Return 
      End

      subroutine norm(yy)
c---------------------------------------------------------------------------- 
c  This routine renormalizes the abundances to guarantee mass conservation.
c---------------------------------------------------------------------------- 
      use nuclear_data
      real(8) :: xtot,rxt
      real(8), dimension(ny) :: yy
      xtot=sum(yy*aa)   
      rxt=1.0/xtot      
      yy=yy*rxt  
      Return                                               
      End                                                 
