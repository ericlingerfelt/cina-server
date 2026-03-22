#####################################################################
##
##  Makefile
##
##  $Author: bucknerk $
##  $Id: Makefile,v 1.4 2008/04/25 17:55:17 bucknerk Exp $
##  $Log: Makefile,v $
##  Revision 1.4  2008/04/25 17:55:17  bucknerk
##  adjusted for RH5
##
##  Revision 1.3  2008/04/25 17:40:24  bucknerk
##  adjusted
##
##  Revision 1.2  2008/04/25 13:14:35  bucknerk
##  xml
##
##  Revision 1.1.1.1  2008/04/22 13:36:35  bucknerk
##  all in one place
##
##  Revision 1.16  2007/08/27 15:54:38  bucknerk
##  deleted install, fixed clean
##
##  Revision 1.15  2007/07/20 17:12:58  bucknerk
##  Tested and fixed the all-install.  Added an all-perms to fix potential permission
##  problems.
##
##  Revision 1.14  2007/07/20 12:08:46  bucknerk
##  Added install target
##
##  Revision 1.13  2007/07/20 12:02:14  bucknerk
##  Changed the name of the executable
##
##  Revision 1.12  2007/07/18 17:52:33  bucknerk
##  Latest and greatest I hope from JPScott personal directory. At least it compiles.
##
##
#####################################################################
# Notes: libmysqlpp is in /usr/local/lib with an older version in /usr/lib
# libmysqlclient is in /usr/lib/mysql
# mysql++/custom.h is in /usr/include and /usr/local/include (same file)

CXX = g++
LIB_DIR=../lib

# THIS is a sanity check to make sure that it is compiled from the correct place
ifndef DEV
verybad:
	@echo MUST START WITH TOP_LEVEL [CINA] (up two directories) MAKEFILE
	@echo That is where the 'DEV' value is assigned, and it determines 
	@echo whether this is a development version or not!
endif

FLAGS=  -I../ -I/usr/include/mysql++ -I/usr/include/mysql -Wall -g 
ifeq (1,$(DEV))
CXXFLAGS=$(FLAGS) -DUSE_DEVELOPMENTAL=1
else
CXXFLAGS=$(FLAGS) -DUSE_DEVELOPMENTAL=0
endif

LDFLAGS=-L/usr/local/lib -lmysqlpp -lmysqlclient
VPATH=.:../include

# Unsure what showlist is used for however I assume that it has something
# to do with displaying logs in an html format.  Am changing the binary
# cgimain (which is the name of the source file) to cina$(SUFFIX) because 
# that is the name of the actually installed executable.  The SUFFIX will
# either be empty or 'd' depending on if this is the production version or
# the development version. 04/22/2008 I don't think that showlist is actually 
# used by anything (I'll check though so I have commented it out.

OBJECT_FILES=ErrObj.o datetime.o accounts.o sessions.o logs.o


.SUFFIXES: .cpp .o

# don't like this but it works
%.o: %.cpp %.h
	$(CXX) $(CXXFLAGS) -c $<

.PHONY: all clean install permissions

#all: $(BINARIES)
all: $(LIB_DIR)/libcina_server.a
	@cd fortran && $(MAKE) $@

$(LIB_DIR)/libcina_server.a: $(OBJECT_FILES)
	mkdir -p $(LIB_DIR)
	ar rcs $(LIB_DIR)/libcina_server.a $^


install:
	@echo Nothing to do for install


clean:
	@cd fortran && $(MAKE) clean
	rm -f *.o $(LIB_DIR)/libcina_server.a
tidy:
	@cd fortran && $(MAKE) tidy
	rm -f *.o 


