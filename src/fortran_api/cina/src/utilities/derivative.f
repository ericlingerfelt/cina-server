************************************************************************
* Moved from the original directory and added the CVS info
*
* $Author: bucknerk $
* $Id: derivative.f,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
* $Log: derivative.f,v $
* Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
* all in one place
*
* Revision 1.2  2007/07/17 19:12:40  bucknerk
* Don't need this any more.
*
*
* SUBROUTINE deriv
*
* Subroutine for calculating derivatives.
*
* Input : num, x, y
* Output : dy
*
************************************************************************

	SUBROUTINE deriv(num, x, y, dy)

	integer num, i, j
	real*8 x(num), y(num), dy(num), delta, tempr
	real*8 thresh, xx(num), yy(num)

	parameter (thresh=1.0d-4)

	delta=(x(2)-x(1))*thresh
	xx(1)=x(1)+delta
	call polint(x(1), y(1), 4, xx(1), yy(1), tempr)

	do i=2, num-2
		delta=(x(i+1)-x(i))*thresh
		xx(i)=x(i)+delta
		call polint(x(i-1), y(i-1), 4, xx(i), yy(i), tempr)
	end do	

	delta=(x(num)-x(num-1))*thresh
	xx(num-1)=x(num-1)+delta
	call polint(x(num-3), y(num-3), 4, xx(num-1), yy(num-1), tempr)

	delta=(x(num)-x(num-1))*thresh
	xx(num)=x(num)+delta
	call polint(x(num-3), y(num-3), 4, xx(num), yy(num), tempr)

	do i=1, num
		dy(i)=(yy(i)-y(i))/(xx(i)-x(i))
	end do

	return
	end
