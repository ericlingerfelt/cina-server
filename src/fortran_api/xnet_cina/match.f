C
C
C   $Author: bucknerk $
C   $Log: match.f,v $
C   Revision 1.5  2008/07/25 16:53:30  bucknerk
C   fixing i hope
C
C   Revision 1.3  2008/07/03 20:36:48  bucknerk
C   adjusted files and tweaked comments. I think it is all compiled now. I have to test it but that can wait till Monday.
C
C   Revision 1.2  2007/07/12 18:53:13  bucknerk
C
C
C   These are the updated versions of xnet from Rafe Hix via Luke Roberts.
C   They include code for two sparse solvers and the original 'dense' version of
C   the matrix code.
C
C
C
c-----------------------------------------------------------------------------
c  This file containes the data structures and input routines for matching
c  reactions which involve the same nuclei (forward and reverse reactions as 
c  well as reactions with multiple components)
c-----------------------------------------------------------------------------

      module match_data
c-----------------------------------------------------------------------------
c  This module contains the data necessary to match up reactions.
c-----------------------------------------------------------------------------
      integer                               :: mflx
      integer, dimension(:,:), allocatable  :: nflx ! nflx(7,mflx)
      integer, dimension(:),   allocatable  :: iwflx,ifl1,ifl2,ifl3
      real(8),  dimension(:),   allocatable  :: qflx
      character(4), dimension(:), allocatable :: descx
      end module match_data

      Subroutine read_match_data(data_dir)
c-----------------------------------------------------------------------------
c  This routine reads in the reaction matching data and allocates the 
c  necessary arrays.
c-----------------------------------------------------------------------------
      use cross_sect_data
      use match_data
      integer  :: i,nr(3)
      character (LEN=*) :: data_dir
      Open(10,FILE=trim(data_dir)//"/match_data",FORM="unformatted")
      Read(10) mflx,nr(1:3)
      Write(50,*) 'Match',mflx
      Do i=1,3
        If(nr(i)/=nreac(i)) Write(6,*) 'NR mismatch',i,nr(i),nreac(i)
      Enddo
      Allocate (ifl1(nr(1)),ifl2(nr(2)),ifl3(nr(3)))
      Read(10) ifl1,ifl2,ifl3
      Allocate (nflx(7,mflx),qflx(mflx),iwflx(mflx),descx(mflx))
      Read(10) nflx,qflx,iwflx,descx
      Return
      End

