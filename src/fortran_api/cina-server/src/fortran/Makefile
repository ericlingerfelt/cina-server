#####################################################################
##
##  Makefile cina-server/src/fortran
##
##  $Author: bucknerk $
##  $Id: Makefile,v 1.4 2008/04/25 17:55:18 bucknerk Exp $
##  $Log: Makefile,v $
##  Revision 1.4  2008/04/25 17:55:18  bucknerk
##  adjusted for RH5
##
##  Revision 1.3  2008/04/25 17:40:24  bucknerk
##  adjusted
##
##  Revision 1.2  2008/04/25 13:14:36  bucknerk
##  xml
##
##  Revision 1.1.1.1  2008/04/22 13:36:36  bucknerk
##  all in one place
##
##  Revision 1.8  2007/08/27 15:51:15  bucknerk
##  Added change to CXXFLAGS for correct DB information
##
##  Revision 1.7  2007/07/20 17:13:02  bucknerk
##  Tested and fixed the all-install.  Added an all-perms to fix potential permission
##  problems.
##
##  Revision 1.6  2007/07/20 12:09:42  bucknerk
##  Updated so that it only uses what it needs, compiles files in the correct places, added tidy
##  target.
##
##  Revision 1.5  2007/07/18 17:52:44  bucknerk
##  Latest and greatest I hope from JPScott personal directory. At least it compiles.
##
##
#####################################################################
# Notes: libmysqlpp is in /usr/local/lib with an older version in /usr/lib
# libmysqlclient is in /usr/lib/mysql
# mysql++/custom.h is in /usr/include and /usr/local/include (same file)
CXX = g++
F90 = ifort
LIB_DIR=../../lib

CFLAGS=-I../.. -I/usr/include/mysql -I/usr/include/mysql++ -Wall

F90FLAGS=-g -module $(LIB_DIR)

# THIS is a sanity check to make sure that it is compiled from the correct place
ifndef DEV
verybad:
	@echo MUST START WITH TOP_LEVEL [CINA] (up three directories) MAKEFILE
	@echo That is where the 'DEV' value is assigned, and it determines 
	@echo whether this is a development version or not!
endif
ifeq (1,$(DEV))
CXXFLAGS=$(CFLAGS) -DUSE_DEVELOPMENTAL=1
else
CXXFLAGS=$(CFLAGS) -DUSE_DEVELOPMENTAL=0
endif

# By putting /usr/local/lib on the command line it is supposed to be searched
# before the default directories.
#

# a path list that make will search for prequisites
VPATH=.:../../include

OBJECT_FILES=F_CINA.o F_accounts.o F_sessions.o F_logs.o  

.SUFFIXES: .cpp .f90


%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.cpp CINA.h utils.h
	$(CXX) $(CXXFLAGS) -c $<


# Default is to make all binaries

all: $(OBJECT_FILES)
	mkdir -p $(LIB_DIR)
	ar rcs $(LIB_DIR)/libF_cina_server.a $^


clean:
	rm -f *.o *.mod $(LIB_DIR)/libF_cina_server.a
	rm -f $(LIB_DIR)/*.mod
tidy:
	rm -f *.o *.mod 
