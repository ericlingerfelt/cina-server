#############################################################################
# $Author: bucknerk $
# $Date: 2008/07/16 17:52:15 $
# $Id: Makefile,v 1.4 2008/07/16 17:52:15 bucknerk Exp $
#
# $Log: Makefile,v $
# Revision 1.4  2008/07/16 17:52:15  bucknerk
# changed output to RESULTS=
#
# Revision 1.3  2008/07/10 19:17:28  bucknerk
# updated
#
# Revision 1.2  2008/04/25 16:11:10  bucknerk
# minor adjustments
#
# Revision 1.1  2008/03/25 18:11:43  bucknerk
# new
#
# still new
#
#
#
#############################################################################
#
# Makefile for new bottle-neck code.
# This one creates the fortran executable from Tomomi's code
############################################################################
FFLAGS=-ggdb -Wall
F77=g77







find_bottle_necks: neck_15.f
	$(F77) $(FFLAGS) neck_15.f -o find_bottle_necks
	chmod 0755 find_bottle_necks
	mv -f find_bottle_necks ../

clean: 
	/bin/rm -f ../find_bottle_necks *.o *.mod
tidy: 
	/bin/rm -f *.o *.mod
