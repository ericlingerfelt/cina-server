!***********************************************************************
!Moved from the original directory and added the CVS info
!
!   $Author: bucknerk $
!   $Id: inverse.f,v 1.3 2008/07/25 16:59:57 bucknerk Exp $
!   $Log: inverse.f,v $
!   Revision 1.3  2008/07/25 16:59:57  bucknerk
!   fixed some minor errors
!
!   Revision 1.2  2008/06/10 13:48:29  bucknerk
!   cosmetic
!
!   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
!   all in one place
!
!   Revision 1.2  2007/07/17 19:12:40  bucknerk
!   Don't need this any more.
!
!   
! mmp98
!
! This program calculates inverse reaction parameters
!
!
!                                   Written by Andy Chae (kchae@utk.edu)
! CVS $Revision: 1.3 $
!***********************************************************************
        use fileio
        use io
        use inv_parm
        implicit none

        integer i, num_para, max_num
        character*100 inputfile, outputfile
        character*250 err_msg
        parameter (max_num=99)
        real*8 array(max_num, 4), a(max_num), a_inv(max_num)
        real*8 del_a(max_num)

        ! Create a fileiooptions variable for using fileio module
        type(fileiooptions)    opt

        ! Get command line options
        CALL getarg(1,opt%file_in)
        CALL getarg(2,inputfile)
        CALL getarg(3,outputfile)
        ! Make sure files were specified
        IF (opt%file_in == ' ') then
          CALL printerror('Input file (from inputread) '               
     |     //'must be the first command line option',1)
        endif
        IF (inputfile == ' ') then
          CALL printerror('Input file (with parameters) '
     |     //'must be the second command line option',1)
        endif
        IF (outputfile == ' ') then
          CALL printerror(
     |      'Output file must be the third command line option',1)
        endif

        ! Open INPUTREAD file for obtaining data points and protons and neutrons
        ! Use the fileio module and specify the required options
        !opt%file_in = 'f.inrd'    ! get from command line option instead
        opt%data_format = 'INPUTREAD'
        CALL file2array(array,i,opt)

!***********************************************************************
!  Read data from (reaction type).fit - from Jason's program
!  This document provides parameters for the reaction
! 
!
!  The status='old' causes an error to occur if the file dows not exist
!***********************************************************************
 

        open(unit=10, file=inputfile, status='unknown')

! Read data from the first line
        read(10,*,END=250) num_para        ! number of parameters

        do i=1, num_para
                read(10,*,END=250) a(i)
        end do
        goto 260

! Error messages for loading 
250        print*, ''
        print*, 'ERROR: (reaction type).fit is not in correct format,'
        print*, ''
        print*, '        or does not exist.'
        print*, ''        
        print*, '       The calculations will be performed without the'
        print*, ''
        print*, '        reaction rate parameters.'
        print*, ''

! Get inverse parameters
 260        CALL get_inv_parm(a,num_para,a_inv,opt%r,del_a,i,err_msg)
        if (i /= 0) CALL printerror(
     |      'Could not obtain inverse parameters: '//TRIM(err_msg),1)

!
! output
!

        open(unit=20, file=outputfile, status='unknown')
        write(20,'(A)') 'The number of parameters is'
        write(20,'(I)') num_para
        write(20,'(A)') ' '

        write(20,'(A)') '* Parameters for the reaction'
        do i=1, num_para
                write(20,'(0pe13.6)') a(i)
        end do
        write(20,'(A)') ' '

        write(20,'(A)') '* Parameters for the inverse reaction'
        do i=1, num_para
                write(20,'(0pe13.6)') a_inv(i)
        end do

        write(20,'(A)') ' '

        do i=1, num_para
           if (del_a(i).ne.0.0) then
              write(20,'(A,I0,A,I0,A,0pe13.6)') 'a',i,' -> a',i,' +',
     |          del_a(i)
           end if
        end do

        end
