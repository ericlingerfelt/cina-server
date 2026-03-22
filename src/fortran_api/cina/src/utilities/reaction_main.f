************************************************************************
*Moved from the original directory and added the CVS info
*
*   $Author: bucknerk $
*   $Id: reaction_main.f,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
*   $Log: reaction_main.f,v $
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
	use reaction
	implicit none

	character*100 tmps, reac_in, reac_ref, reactype, out_file
	real*8 pi, na, boltz, bind_val, mu, bgam, mn, mp, sum
	real*8 z1, z2, egam, tmpreal, last, half, rt, lt
	real*8 old, delta, dy, epeak, speak, thl1, thl2, thl3, thl4
	real*8 t_min, t_max, st, et, q, a1, a2, a3, a4, width, dsfac_max
	real*8 widL, widR, boxl, boxh, box
	integer i, j, k, ii, tmpint, numpt, numip 
	integer np1, np2, np3, np4, nn1, nn2, nn3, nn4
	integer max_num, idx, kd, max_indx, ptL, ptR, idx_res, num_res
	integer ileft, widthL, widthR

	external func
	parameter (max_num=999999)
	parameter (na=6.0221367d+23)	! Avogadro's number
	parameter (boltz=8.617385d-8)	! Boltzmann constant in keV/K
	parameter (mp=1.00728d0)	! mass of proton in amu
	parameter (mn=1.00865d0)	! mass of neutron in amu

	real*8 T(max_num), T9(max_num), NACRE_rate(max_num)
	real*8 eam(max_num), ecm(max_num), sfac(max_num), rate(max_num)
	real*8 dif(max_num), sig(max_num)
	real*8 dsfac(max_num), ddsfac(max_num), h(max_num), xx(max_num)
	real*8 yy(max_num), sfac_i(max_num), ecm_i(max_num), array(max_num, 4)
	real*8 narrow_e(max_num), narrow_s(max_num), peak_widths(max_num)
	real*8 narrow_d(max_num), narrow_dd(max_num)
	real*8 tmpar1(max_num), tmpar2(max_num), tmpar3(max_num)
	real*8 tmpar4(max_num), tmpar5(max_num), tmpar6(max_num)
	real*8 energy(max_num,6), sfactor(max_num,6)
	integer flag(max_num), num_points(max_num), points(max_num)
	! Create a fileiooptions variable for using fileio module
	type(fileiooptions)            :: opt

	! Get command line options
	CALL getarg(1,opt%file_in)

	! Make sure an input file was specified
	IF (opt%file_in == ' ') CALL printerror(
     |	'Input file must be the first command line option',1)

	CALL getarg(2,out_file)
	! Make sure an output file was specified
	IF (out_file == ' ') CALL printerror(
     |     'Temperature grid points file must be the '//
     |     'second command line option',1)

	CALL getarg(3,out_file)
	! Make sure an output file was specified
	IF (out_file == ' ') CALL printerror(
     |     'Output file must be the third command line option',1)

	! Open INPUTREAD file for obtaining data points and protons and neutrons
	! Use the fileio module and specify the required options
	!opt%file_in = 'f.inrd'    ! get from command line option instead
	opt%data_format = 'INPUTREAD'
	CALL file2array(array,numip,opt)

*
* parameters for interpolaion methods
*

	thl1=8.0d-2	! threshold for 4-point cubic
	thl2=7.0d-2	! threshold for spline
	thl3=1.4d-6	! threshold for integration
	thl4=1.0d0	! threshold for interpolation

	pi=4.0d0*datan(1.0d0)

	! Make sure the number of reactants is 2
	IF (getreac_num(opt%r) .NE. 2) CALL printerror(
     |     'Number of reactants must be 2',1)
	
	np1 = getreac_z(opt%r,1)
	np2 = getreac_z(opt%r,2)
	np3 = getprod_z(opt%r,1)
	np4 = getprod_z(opt%r,2)
	nn1 = getreac_n(opt%r,1)
	nn2 = getreac_n(opt%r,2)
	nn3 = getprod_n(opt%r,1)
	nn4 = getprod_n(opt%r,2)

	z1=dfloat(np1)
	z2=dfloat(np2)

*
* Particle #1 - together with particle #2, this is the 'incident'
* 		particle
*

	bind_val=binden(np1, nn1)
	bind_val=bind_val*1.07354d-6	! Unit conversion to amu
	a1=dfloat(np1)*mp+dfloat(nn1)*mn-bind_val	! in amu

*
* Particle #2
*

	bind_val=binden(np2, nn2)
	bind_val=bind_val*1.07354d-6	! Unit conversion to amu
	a2=dfloat(np2)*mp+dfloat(nn2)*mn-bind_val	! in amu

*
* Particle #3
*

	bind_val=binden(np3, nn3)
	bind_val=bind_val*1.07354d-6	! Unit conversion to amu
	a3=dfloat(np3)*mp+dfloat(nn3)*mn-bind_val	! in amu

*
* Particle #4
*

	bind_val=binden(np4, nn4)
	bind_val=bind_val*1.07354d-6	! Unit conversion to amu
	a4=dfloat(np4)*mp+dfloat(nn4)*mn-bind_val	! in amu

	q=((a1+a2)-(a3+a4))*931.494d0
	print*, 'q value (in MeV) =', q
	mu=(a1*a2)/(a1+a2)  ! reduced atomic weight
	bgam=0.989d0*z1*z2*mu**(0.5d0)*10.0d0**(3.0d0/2.0d0)
			! Rolfs and Rodney pp.158 (4.18)
			! bgam^2 is called Gamow energy

	select case (lowercase(opt%data_type))
	case ("s(e)")
		do i=1, numip
			ecm(i)=array(i,1)
			sfac(i)=array(i,3)
		end do
	case ("cs(e)")
		do i=1, numip
			ecm(i)=array(i,1)
			sig(i)=array(i,3)
			sfac(i)=(ecm(i)*sig(i))*dexp(bgam/dsqrt(ecm(i)))
				! sig(i) is in [barn]=[10^-24 cm^2]
		end do
	case default 
		call printerror("Unknown datatype",1)
	end select

* Calculate the minimum and the maximum vlaues for temperature grid.

*
* Finding the minimum temperature
*

	t_min=0.01d0	! Temporary minimum value
100	st=122.0d0*((z1**2.0d0)*(z2**2.0d0)*mu)**(1.0d0/3.0d0)*(t_min
     |	**(2.0d0/3.0d0))-118.4273d0*((z1**2.0d0)*(z2**2.0d0)*mu)
     |	**(1.0d0/6.0d0)*(t_min**(5.0d0/6.0d0))
	if (st.lt.ecm(1)) then
		t_min=t_min+0.01d0
		goto 100
	end if
 
*
* Finding the maximum temperature
*

	t_max=10.0d0	! Temporary maximum value
150	et=122.0d0*((z1**2.0d0)*(z2**2.0d0)*mu)**(1.0d0/3.0d0)*(t_max
     |	**(2.0d0/3.0d0))+118.4273d0*((z1**2.0d0)*(z2**2.0d0)*mu)
     |	**(1.0d0/6.0d0)*(t_max**(5.0d0/6.0d0))
	if (et.gt.ecm(numip)) then
		t_max=t_max-0.01d0
		goto 150
	end if
	print*, t_min, t_max

*
* Finding derivatives
*

	call deriv(numip, ecm, sfac, dsfac)
	call deriv(numip, ecm, dsfac, ddsfac)

*
* Read in temperature grid from file using fileio module
*

	CALL getarg(2,opt%file_in)
	opt%data_format = 'LOOSE_COLUMN'
	! opt%file_in = 'tgrid.txt'   ! get from command line option
	opt%t_units_in = 1.0
	opt%r_units_in = 1.0
	opt%data_type = 'R(T)'
	opt%fmt_options = '1000'
	CALL file2array(array,numpt,opt)

	do i=1,numpt
		T9(i)=array(i,1)
		T(i)=T9(i)*10.0d0**9.0d0
		egam=1.22d0*(z1**2.0d0*z2**2.0d0*mu*(T(i)/10.0d0**6.0d0)
     |						**2.0d0)**(1.0d0/3.0d0)
		eam(i)=egam
	enddo


************************************************************************
* Finding local maxima
************************************************************************

*
* Finding local maxima
*

300	num_res=0		! Number of resonances.
	do i=2, numip-2
		if ((sfac(i).gt.sfac(i-1)).and.(sfac(i).ge.sfac(i+1))) then
			num_res=num_res+1
			flag(num_res)=i
		end if
	end do
	if (num_res.eq.0) goto 600

*
* Assigning the local maxima to a new array. The derivatives are re-calculated.
* 

	do i=1, num_res
		if (dsfac(flag(i)).gt.0.0d0) then
			call polint(ecm(flag(i)-2),sfac(flag(i)-2),3,ecm(flag(i))+1.0d-4,
     |									tmpreal,dy)
			points(i)=flag(i)
		else if (dsfac(flag(i)).lt.0.0d0) then
			call polint(ecm(flag(i)),sfac(flag(i)),3,
     |					ecm(flag(i))+1.0d-4,tmpreal,dy)
			points(i)=flag(i)-1
		end if
		narrow_e(i)=ecm(flag(i))
		narrow_s(i)=sfac(flag(i))
		narrow_d(i)=dsfac(flag(i))
		narrow_dd(i)=ddsfac(flag(i))
		narrow_d(i)=(tmpreal-sfac(flag(i)))/1.0d-4
	end do

*
* Criteria for NON-resonant reactions
*

310	do i=1, num_res
		if ((dabs(narrow_d(i)).lt.1.3d0).or.
     |				(dabs(narrow_dd(i)).lt.0.01d0)) then
			if ((sfac(flag(i))/sfac(flag(i)-1).gt.2.0d0).and.
     |				(sfac(flag(i))/sfac(flag(i)+1).gt.2.0d0)) goto 320
			do j=i, num_res-1
				narrow_e(j)=narrow_e(j+1)
				narrow_s(j)=narrow_s(j+1)
				narrow_d(j)=narrow_d(j+1)
				narrow_dd(j)=narrow_dd(j+1)
				flag(j)=flag(j+1)
				points(j)=points(j+1)
			end do
			num_res=num_res-1
			goto 310
		end if
320	end do	
	if (num_res.eq.0) goto 600

*
* Finding the value at the peaks and the value of the width
*

	delta=0.01d0
	do j=1, num_res
		epeak=ecm(points(j))
330		epeak=epeak+delta
		if (epeak.ge.ecm(points(j)+1)-3.0d0*delta) then
			epeak=ecm(points(j)+1)-3.0d0*delta
			call splint(ecm,sfac,ddsfac,numip,epeak,speak)
			goto 340
		end if

		call splint(ecm,sfac,ddsfac,numip,epeak,speak)
		do k=1, points(j)
			tmpar1(k)=ecm(k)
			tmpar2(k)=sfac(k)
		end do

		tmpar1(points(j)+1)=epeak
		tmpar2(points(j)+1)=speak

		do k=1, numip-points(j)
			tmpar1(points(j)+k+1)=ecm(points(j)+k)
			tmpar2(points(j)+k+1)=sfac(points(j)+k)
		end do
		call deriv(numip+1, tmpar1, tmpar2, tmpar3)
		call deriv(numip+1, tmpar1, tmpar3, tmpar4)
		if ((tmpar3(points(j))*tmpar3(points(j)+1)).ge.0.0d0) goto 330
340		half=speak/dexp(1.0d0)

		tmpint=points(j)+1
350		if (tmpar2(tmpint).gt.half) then
			tmpint=tmpint+1
			goto 350
		end if
		tmpint=tmpint-1

		widR=tmpar1(tmpint)
360		widR=widR+delta
		call splint(tmpar1,tmpar2,tmpar4,numip+1,widR,tmpreal)
		if (tmpreal.ge.half) then
			if (widR.ge.tmpar1(numip+1)) then
				widR=tmpar1(numip+1)
				goto 370
			end if
			goto 360
		end if

370		tmpint=points(j)+1
380		if (tmpar2(tmpint).gt.half) then
			tmpint=tmpint-1
			goto 380
		end if
		tmpint=tmpint+1

		widL=tmpar1(tmpint)
390		widL=widL-delta
		call splint(tmpar1,tmpar2,tmpar4,numip+1,widL,tmpreal)
		if (tmpreal.ge.half) then
			if (widL.le.tmpar1(1)) then
				widL=tmpar1(1)
				goto 400
			end if
			goto 390
		end if

400		width=widR-widL
		peak_widths(j)=width
	end do

410	do i=1, num_res
		if (peak_widths(i).gt.20.0d0) then	! Criteria for resonance
			do j=i, num_res-1
				narrow_e(j)=narrow_e(j+1)
				narrow_s(j)=narrow_s(j+1)
				narrow_d(j)=narrow_d(j+1)
				narrow_dd(j)=narrow_dd(j+1)
				flag(j)=flag(j+1)
				points(j)=points(j+1)
				peak_widths(j)=peak_widths(j+1)
			end do	
			num_res=num_res-1
			goto 410		
		end if
	end do

*
* Assigning energies and sfactors to new arrays
*

	do i=1, points(1)
		energy(i,1)=ecm(i)
		sfactor(i,1)=sfac(i)
		num_points(1)=points(1)
	end do

	if (num_res.gt.1) then
		print*, 'more...'
		do i=1, num_res-1
			do j=points(i)+1, points(i+1)
				energy(j-points(i),i+1)=ecm(j)
				sfactor(j-points(i),i+1)=sfac(j)
				num_points(i+1)=points(i+1)-points(i)
			end do
		end do
	end if

	do i=points(num_res)+1, numip 
		energy(i-points(num_res),num_res+1)=ecm(i)
		sfactor(i-points(num_res),num_res+1)=sfac(i)
		num_points(num_res+1)=numip-points(num_res)
	end do

*
* Recalculating the peak value using 4-point cubic polynomial
*

500	delta=0.001d0
	do i=1, num_res
		if (sfactor(num_points(i),i).gt.sfactor(1,i+1)) then
			epeak=energy(num_points(i),i)
510			epeak=epeak+delta
			call polint(energy(num_points(i)-2,i),
     |				sfactor(num_points(i)-2,i),3,epeak,speak,dy)
			do j=1, num_points(i)
				tmpar1(j)=energy(j,i)
				tmpar2(j)=sfactor(j,i)
			end do

			tmpar1(num_points(i)+1)=epeak
			tmpar2(num_points(i)+1)=speak

			do j=1, num_points(i+1)
				tmpar1(j+num_points(i)+1)=energy(j,i+1)
				tmpar2(j+num_points(i)+1)=sfactor(j,i+1)
			end do

			call deriv(num_points(i)+num_points(i+1)+1, tmpar1, tmpar2, tmpar3)
			if ((tmpar3(num_points(i))*tmpar3(num_points(i)+1)).gt.0.0d0) then
				goto 510
			end if
		else
			epeak=energy(1,i+1)
520			epeak=epeak-delta
			call polint(energy(1,i+1),sfactor(1,i+1),3,epeak,speak,dy)
			do j=1, num_points(i)
				tmpar1(j)=energy(j,i)
				tmpar2(j)=sfactor(j,i)
			end do

			tmpar1(num_points(i)+1)=epeak
			tmpar2(num_points(i)+1)=speak

			do j=1, num_points(i+1)
				tmpar1(j+num_points(i)+1)=energy(j,i+1)
				tmpar2(j+num_points(i)+1)=sfactor(j,i+1)
			end do

			call deriv(num_points(i)+num_points(i+1)+1, tmpar1, tmpar2, tmpar3)
			if ((tmpar3(num_points(i)+1)*tmpar3(num_points(i)+2)).gt.0.0d0) then
				goto 520
			end if
		end if

*
* Making new arrays that include the values at the peaks
*
		
		energy(num_points(i)+1,i)=epeak
		sfactor(num_points(i)+1,i)=speak
		
		do j=1, num_points(i+1)
			tmpar1(j)=energy(j,i+1)
			tmpar2(j)=sfactor(j,i+1)
		end do

		energy(1,i+1)=epeak
		sfactor(1,i+1)=speak

		do j=1, num_points(i+1)
			energy(j+1,i+1)=tmpar1(j)
			sfactor(j+1,i+1)=tmpar2(j)
		end do

		num_points(i)=num_points(i)+1
		num_points(i+1)=num_points(i+1)+1
	end do


************************************************************************
* Interpolation
************************************************************************

600	print*, 'resonances =', num_res		! Number of resonances
	kd=1				! Number of interpolated pointss
	ecm_i(kd)=ecm(1)	! The original data should be a part of new array
	sfac_i(kd)=sfac(1)

	if (num_res.eq.0) then		! NON-resonance.
		do i=1,numip-1
			kd=kd+1
			j=1
			ileft=kd	! Temporary index. If irrational interpolation occurs, this index would be used.
			box=dabs(sfac(i+1)-sfac(i))*thl4
			if (sfac(i+1).gt.sfac(i)) then
				boxl=sfac(i)-box	! This value, together with boxh, confines the interpolated value for s-factor.
				boxh=sfac(i+1)+box
			else
				boxl=sfac(i+1)-box
				boxh=sfac(i)+box
			end if
610			delta=(ecm(i+1)-ecm(i))/2.0d0**j	! At first, find the interpolated value at the mid point.
			ecm_i(kd)=ecm(i)+delta
			call splint(ecm,sfac,ddsfac,numip,ecm_i(kd),sfac_i(kd))	! Cubic spline method is used for non-resonant reactions.
			tmpreal=(sfac_i(kd)-sfac(i))/sfac(i)*1.0d2
			tmpreal=dabs(tmpreal)
			if (tmpreal.gt.thl2) then
				j=j+1
				goto 610
			else
				do ii=1,2**j-1
					kd=kd+1
					ecm_i(kd)=ecm_i(kd-1)+delta
					call splint(ecm,sfac,ddsfac,numip,ecm_i(kd),sfac_i(kd))
					if ((sfac_i(kd).lt.boxl).or.(sfac_i(kd).gt.boxh)) then
						kd=ileft
						ecm_i(kd)=ecm(i+1)
						sfac_i(kd)=sfac(i+1)
						goto 620
					end if
				end do
			end if	
620		end do
	else
		do i=1, num_res+1

			j=1
			kd=kd+1
			ileft=kd
			box=dabs(sfactor(2,i)-sfactor(1,i))*thl4
			if (sfactor(2,i).gt.sfactor(1,i)) then
				boxl=sfactor(1,i)-box
				boxh=sfactor(2,i)+box
			else
				boxl=sfactor(2,i)-box
				boxh=sfactor(1,i)+box
			end if
630			delta=(energy(2,i)-energy(1,i))/2.0d0**j
			ecm_i(kd)=energy(1,i)+delta
			call polint(energy(1,i),sfactor(1,i),4,ecm_i(kd),sfac_i(kd),dy)
			tmpreal=(sfac_i(kd)-sfactor(1,i))/sfactor(1,i)*1.0d2
			tmpreal=dabs(tmpreal)
			if (tmpreal.gt.thl1) then
				j=j+1
				goto 630
			else
				do ii=1,2**j-1
					kd=kd+1
					ecm_i(kd)=ecm_i(kd-1)+delta
					call polint(energy(1,i),sfactor(1,i),4,ecm_i(kd),sfac_i(kd),dy)
					if ((sfac_i(kd).lt.boxl).or.(sfac_i(kd).gt.boxh)) then
						kd=ileft
						ecm_i(kd)=energy(2,i)
						sfac_i(kd)=sfactor(2,i)
						goto 640
					end if
				end do
			end if	

640			do k=2,num_points(i)-2
				j=1
				kd=kd+1
				ileft=kd
				box=dabs(sfactor(k+1,i)-sfactor(k,i))*thl4
				if (sfactor(k+1,i).gt.sfactor(k,i)) then
					boxl=sfactor(k,i)-box
					boxh=sfactor(k+1,i)+box
				else
					boxl=sfactor(k+1,i)-box
					boxh=sfactor(k,i)+box
				end if
650				delta=(energy(k+1,i)-energy(k,i))/2.0d0**j
				ecm_i(kd)=energy(k,i)+delta
				call polint(energy(k-1,i),sfactor(k-1,i),4,ecm_i(kd),sfac_i(kd),dy)
				tmpreal=(sfac_i(kd)-sfactor(k,i))/sfactor(k,i)*1.0d2
				tmpreal=dabs(tmpreal)
				if (tmpreal.gt.thl1) then
					j=j+1
					goto 650
				else
					do ii=1,2**j-1
						kd=kd+1
						ecm_i(kd)=ecm_i(kd-1)+delta
						call polint(energy(k-1,i),sfactor(k-1,i),4,
     |									ecm_i(kd),sfac_i(kd),dy)
						if ((sfac_i(kd).lt.boxl).or.(sfac_i(kd).gt.boxh)) then
							kd=ileft
							ecm_i(kd)=energy(k+1,i)
							sfac_i(kd)=sfactor(k+1,i)
							goto 660
						end if
					end do
				end if	
660			end do

			kd=kd+1
			j=1
			ileft=kd
			box=dabs(sfactor(num_points(i),i)-sfactor(num_points(i)-1,i))*thl4
			if (sfactor(num_points(i),i).gt.sfactor(num_points(i)-1,i)) then
				boxl=sfactor(num_points(i)-1,i)-box
				boxh=sfactor(num_points(i),i)+box
			else
				boxl=sfactor(num_points(i),i)-box
				boxh=sfactor(num_points(i)-1,i)+box
			end if
670			delta=(energy(num_points(i),i)-energy(num_points(i)-1,i))/
     |										2.0d0**j
			ecm_i(kd)=energy(num_points(i)-1,i)+delta
			call polint(energy(num_points(i)-3,i),
     |					sfactor(num_points(i)-3,i),4,ecm_i(kd),sfac_i(kd),dy)
			tmpreal=(sfac_i(kd)-sfactor(num_points(i)-1,i))/
     |						sfactor(num_points(i)-1,i)*1.0d2
			tmpreal=dabs(tmpreal)
			if (tmpreal.gt.thl1) then
				j=j+1
				goto 670
			else
				do ii=1,2**j-1
					kd=kd+1
					ecm_i(kd)=ecm_i(kd-1)+delta
					call polint(energy(num_points(i)-3,i),
     |						sfactor(num_points(i)-3,i),4,ecm_i(kd),sfac_i(kd),dy)
					if ((sfac_i(kd).lt.boxl).or.(sfac_i(kd).gt.boxh)) then
						kd=ileft
						ecm_i(kd)=energy(num_points(i),i)
						sfac_i(kd)=sfactor(num_points(i),i)
						goto 680
					end if
				end do
			end if	
680		end do			! Loop for num_res
	end if	


************************************************************************
* Calculating reaction rate 
************************************************************************

800	j=0
	print*, kd
	do j=1,numpt	! Loop for temperatures.
		sum=0
		do i=1,kd-1
			h(i)=ecm_i(i+1)-ecm_i(i)
			yy(i)=(sfac_i(i+1)+sfac_i(i))/2.0d0
			xx(i)=(ecm_i(i+1)+ecm_i(i))/2.0d0
			old=sum
			sum=sum+yy(i)*h(i)*dexp(-xx(i)/(boltz*T(j))-bgam/
     |							(xx(i)**(1.0d0/2.0d0)))
			if (i.eq.1) goto 820
			if (((sum-old)/old).lt.thl3) goto 840
820		enddo
840		rate(j)=(((8.0d0/(pi*mu))**(1.0d0/2.0d0))/
     |				((boltz*T(j))**(1.5d0)))*sum

* Conversion factors and calculating diffrences
* X10^-24 : barn to cm^2
* 4.0027x10^-5 : keV^1/2 to erg^1/2
* 1.28863x10^-12 : amu^1/2 to g^1/2

		rate(j)=na*rate(j)*10.0d0**(-24.0d0)*4.00274d-5/1.28863d-12
		write(*, '(A, I3, A)') 'Reaction rate ',
     |				dfloat(j)/numpt*100.,' % Complete.'
	enddo

900	j=0
	open(unit=30, file=out_file, status='unknown')
	write(30,'(I6)') kd
	write(30,'(2x,A,12x,A,12x,A)') 'T9','my','E_gam' 

	do j=1, numpt
	   write(30,'(2(1pe13.4E3,1X),1pe13.4E3)') T9(j), rate(j), eam(j)
	enddo

	stop
	end


************************************************************************
************************************************************************


************************************************************************
*
* Function func
*
************************************************************************

	Function func(k, bgam, E, T)

	real*8 func, k, bgam, E, T
	func=dexp(-E/(k*T)-bgam/(E**(1./2.)))

	end Function


************************************************************************
*
* Subroutine trapzd
*
************************************************************************

	SUBROUTINE trapzd(func,a,b,s,n,bgam,k,T,numip,ecm,sfac,ddsfac)
	integer n, numip
	real*8 a, b, s, func, bgam, k, T, tmpreal
	real*8 ecm(numip), sfac(numip), ddsfac(numip)
	external func

	integer it, j
	real*8 del, sum, tnm, x
	if (n.eq.1) then
		s=0.5d0*(b-a)*(func(k,bgam,a,T)*sfac(1)+func(k,bgam,b,T)*sfac(numip))
	else
		it=2**(n-2)
		tnm=dfloat(it)
		del=(b-a)/tnm
		x=a+0.5*del
		sum=0.0d0
		do j=1, it
			call splint(ecm,sfac,ddsfac,numip,x,tmpreal)
			sum=sum+func(k, bgam, x, T)*tmpreal
			x=x+del
		end do
		s=0.5d0*(s+(b-a)*sum/tnm)
	end if
	return
	end


************************************************************************
*
* Subroutine qtrap_spline
*
************************************************************************

	SUBROUTINE qtrap_spline(func,a,b,s,bgam,k,T,numip,ecm,sfac,ddsfac)
	integer jmax, numip
	real*8 a, b, func, s, eps, bgam, k, T
	real*8 ecm(numip), sfac(numip), ddsfac(numip)
	external func
	parameter (eps=1.0d-6, jmax=30)
	
	integer j
	real*8 olds
	olds=-1.0d30
	do j=1, jmax
		call trapzd(func, a, b, s, j, bgam, k, T, numip, ecm, sfac, ddsfac)
		if (dabs(s-olds).lt.eps*dabs(olds)) return
		if (s.eq.0..and.olds.eq.0..and.j.gt.6) return
		olds=s
	end do
	pause 'too many steps in qtrap'
	end


************************************************************************
*
* Subroutine qtrap_cubic
*
************************************************************************

	SUBROUTINE qtrap_cubic(func,a,b,s,bgam,k,T,numip,ecm,sfac,ptL)
	integer jmax, numip, ptL
	real*8 a, b, func, s, eps, bgam, k, T
	real*8 ecm(numip), sfac(numip), ddsfac(numip)
	external func
	parameter (eps=1.0d-6, jmax=30)
	
	integer j
	real*8 olds
	olds=-1.0d30
	do j=1, jmax
		call trapzd_cubic(func,a,b,s,j,bgam,k,T,numip,ecm,sfac,ptL)
		if (dabs(s-olds).lt.eps*dabs(olds)) return
		if (s.eq.0..and.olds.eq.0..and.j.gt.6) return
		olds=s
	end do
	pause 'too many steps in qtrap'
	end


************************************************************************
*
* Subroutine trapzd_cubic
*
************************************************************************

	SUBROUTINE trapzd_cubic(func,a,b,s,n,bgam,k,T,numip,ecm,sfac,ptL)
	integer n, numip, start, ptL
	real*8 a, b, s, func, bgam, k, T, tmpreal
	real*8 ecm(numip), sfac(numip), ddsfac(numip), dy
	external func

	integer it, j, i
	real*8 del, sum, tnm, x
	if (n.eq.1) then
		s=0.5d0*(b-a)*(func(k,bgam,a,T)*sfac(1)+func(k,bgam,b,T)*sfac(numip))
	else
		it=2**(n-2)
		tnm=dfloat(it)
		del=(b-a)/tnm
		x=a+0.5*del
		sum=0.0d0
		do j=1, it

			if (x.lt.ecm(2)) start=1
			do i=2, ptL-1
				if ((x.gt.ecm(i)).and.(x.lt.ecm(i+1))) start=i-1
			end do
			if ((x.gt.ecm(ptL)).and.(x.lt.ecm(ptL+1))) start=ptL-2
			if ((x.gt.ecm(ptL+1)).and.(x.lt.ecm(ptL+3))) start=ptL+1
			do i=ptL+3, numip-2
				if ((x.gt.ecm(i)).and.(x.lt.ecm(i+1))) start=i-1
			end do
			if ((x.gt.ecm(numip-1)).and.(x.lt.ecm(numip))) start=numip-3



			call polint(ecm(start),sfac(start),4,x,tmpreal,dy)
			sum=sum+func(k, bgam, x, T)*tmpreal
			x=x+del
		end do
		s=0.5d0*(s+(b-a)*sum/tnm)
	end if
	return
	end

