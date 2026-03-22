************************************************************************
*
*Moved from the original directory and added the CVS info
*
*   $Author: bucknerk $
*   $Id: polint.f,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
*   $Log: polint.f,v $
*   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
*   all in one place
*
*   Revision 1.2  2007/07/17 19:12:40  bucknerk
*   Don't need this any more.
*
*   
* SUBROUTINE polint
*
* This subroutine is used for Polynomial Interpolation
* Copied from 'Numerical Recipes in Fortran' pp. 103-104
*
************************************************************************

	SUBROUTINE polint(xa, ya, n, x, y, dy)
	integer n, NMAX
	real*8 dy, x, y, xa(n), ya(n)
	PARAMETER (NMAX=10)
	integer i, m, ns
	real*8 den, dif, dift, ho, hp, w, c(NMAX), d(NMAX)

	ns=1
	dif=abs(x-xa(1))

	do i=1,n
		dift=abs(x-xa(i))
		if (dift.lt.dif) then
			ns=i
			dif=dift
		endif
		c(i)=ya(i)
		d(i)=ya(i)
	enddo

	y=ya(ns)
	ns=ns-1

	do m=1, n-1
		do i=1, n-m
			ho=xa(i)-x
			hp=xa(i+m)-x
			w=c(i+1)-d(i)
			den=ho-hp
			if (den.eq.0.0d0) pause 'failure in polint'
			den=w/den
			d(i)=hp*den
			c(i)=ho*den
		enddo
		if (2*ns.lt.n-m) then
			dy=c(ns+1)
		else
			dy=d(ns)
			ns=ns-1
		endif
		y=y+dy
	enddo

	return
	end
