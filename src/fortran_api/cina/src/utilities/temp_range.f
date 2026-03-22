************************************************************************
*Moved from the original directory and added the CVS info
*
*   $Author: bucknerk $
*   $Id: temp_range.f,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
*   $Log: temp_range.f,v $
*   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
*   all in one place
*
*   Revision 1.3  2007/07/19 20:43:41  bucknerk
*
*   Updated Makefile should now hadle everything.  Did not really change the
*   others just vi and save
*
*   Revision 1.2  2007/07/17 19:12:40  bucknerk
*   Don't need this any more.
*
*   
* mmp98
*
* This program converts tables of S(E) and sigma(E) to rate(T).
*
*
*				   Written by Andy Chae (kchae@utk.edu)
* CVS $Revision: 1.1.1.1 $
************************************************************************
	use bind
	use fileio
	use reactionstrings
	use io
	use convert
	implicit none

	real*8 bind_val,  mu, mn, mp, z1, z2, a1, a2
	real*8 t_min, t_max, st, et
	integer i, np1, np2, nn1, nn2, numip, max_num

	parameter (max_num=999999)
	parameter (mp=1.00728d0)	! mass of proton in amu
	parameter (mn=1.00865d0)	! mass of neutron in amu

	real*8 ecm(max_num), array(max_num, 4)

	! Create a fileiooptions variable for using fileio module
	type(fileiooptions)            :: opt

	! Get command line options
	CALL getarg(1,opt%file_in)
	! Make sure an input file was specified
	IF (opt%file_in == ' ') CALL printerror(
     |     'Input file must be the first command line option',1)

	! Open INPUTREAD file for obtaining data points and protons and neutrons
	! Use the fileio module and specify the required options
	!opt%file_in = 'f.inrd'    ! get from command line option instead
	opt%data_format = 'INPUTREAD'
	CALL file2array(array,numip,opt)

	! Make sure the number of reactants is 2
	IF (getreac_num(opt%r) .NE. 2) CALL printerror(
     |     'Number of reactants must be 2',1)
	
	np1 = getreac_z(opt%r,1)
	np2 = getreac_z(opt%r,2)
	nn1 = getreac_n(opt%r,1)
	nn2 = getreac_n(opt%r,2)

	IF (np1 == 0 .OR. np2 == 0) THEN
	   t_min=0.01d0
	   t_max=10.0d0
	   print*, t_min, t_max
	END IF

	! Added to prevent gamov window calculation for neutron captures

	z1=dfloat(np1)
	z2=dfloat(np2)

	bind_val=binden(np1, nn1)
	bind_val=bind_val*1.07354d-6	! Unit conversion to amu
	a1=dfloat(np1)*mp+dfloat(nn1)*mn-bind_val

	bind_val=binden(np2, nn2)
	bind_val=bind_val*1.07354d-6	! Unit conversion to amu
	a2=dfloat(np2)*mp+dfloat(nn2)*mn-bind_val

	mu=(a1*a2)/(a1+a2)  ! reduced atomic weight

	select case (lowercase(opt%data_type))
	case ("s(e)")
		do i=1, numip
			ecm(i)=array(i,1)
		end do
	case ("cs(e)")
		do i=1, numip
			ecm(i)=array(i,1)
		end do
	case default 
		call printerror("Unknown datatype",1)
	end select

* Calculate the minimum and the maximum vlaues for temperature grid.

*
* Finding the minimum temperature
*

	t_min=0.01d0	! Temporary minimum value
	!print *,'ecm(1)=',ecm(1)
100	st=122d0*((z1**2.)*(z2**2.)*mu)**(1./3.)*(t_min**(2./3.))
     |	-118.4273d0*((z1**2.)*(z2**2.)*mu)**(1./6.)*(t_min**(5./6.))
	if (st.lt.ecm(1)) then
		t_min=t_min+0.01d0
		! Prevent infinite loop when using wrong reaction
		if (t_min > 10.0) call printerror(
     |	          "Error calculating min temperature value.  "//
     |      "Make sure the reaction string for this file is correct.",1)
		!print *,'t_min=',t_min,' st=',st
		goto 100
	end if
 
*
* Finding the maximum temperature
*

	t_max=10.0d0	! Temporary maximum value
	!print *,'ecm(1)=',ecm(numip)
150	et=122d0*((z1**2.)*(z2**2.)*mu)**(1./3.)*(t_max**(2./3.))
     |	+118.4273d0*((z1**2.)*(z2**2.)*mu)**(1./6.)*(t_max**(5./6.))
	if (et.gt.ecm(numip)) then
		t_max=t_max-0.01d0
		! Prevent infinite loop when using wrong reaction
		if (t_max < 0.01) call printerror(
     |            "Error calculating max temperature value.  "//
     |      "Make sure the reaction string for this file is correct.",1)
		!print *,'t_max=',t_max,' et=',et
		goto 150
	end if
	print*, t_min, t_max

	stop
	end
