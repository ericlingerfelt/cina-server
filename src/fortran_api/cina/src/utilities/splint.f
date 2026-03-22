************************************************************************
*Moved from the original directory and added the CVS info
*
*   $Author: bucknerk $
*   $Id: splint.f,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
*   $Log: splint.f,v $
*   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
*   all in one place
*
*   Revision 1.2  2007/07/17 19:12:40  bucknerk
*   Don't need this any more.
*
*   
*
* SUBROUTINE splint
*
* Subroutine for interpolation. 
*
* Input : 
* Output : 
*
************************************************************************

	SUBROUTINE splint(xa, ya, y2a, n, x, y)

	integer n
	real*8 x, y, xa(n), y2a(n), ya(n)
	integer k, khi, klo
	real*8 a, b, h
	klo=1
	khi=n

1	if (khi-klo.gt.1.0d0) then
		k=(khi+klo)/2.0d0
		if (xa(k).gt.x) then
			khi=k
		else
			klo=k
		endif
	goto 1

	endif

	h=xa(khi)-xa(klo)
	if (h.eq.0.0d0) pause 'bad xa input in splint'
	a=(xa(khi)-x)/h
	b=(x-xa(klo))/h
	y=a*ya(klo)+b*ya(khi)+
     |		((a**3.-a)*y2a(klo)+(b**3.-b)*y2a(khi))*(h**2)/6.0d0

	return
	end
