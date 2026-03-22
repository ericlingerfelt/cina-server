#####################################################
# Makefile cina/src/utilities
#
# This file will combine the 'efforts' of a number fo other files. 
# All the executabels that will be created are dependent on 
# object files from nucastroserver and/or nucastrolib so 
# should just put them all here instead of hiding them in "seldom".
# So what if they are recomiled regularly, its not like they will
# wear out.
#
#####################################################
#
#   $Author: bucknerk $
#   $Id: Makefile,v 1.2 2008/04/25 19:50:13 bucknerk Exp $
#   $Log: Makefile,v $
#   Revision 1.2  2008/04/25 19:50:13  bucknerk
#   now it compiles and installs on the new box "astroport"
#
#   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
#   all in one place
#
#   Revision 1.6  2007/07/20 12:35:39  bucknerk
#   Changed rategen to use the src files instead of compiling objects
#   separately.  This is not expected to change much so won't be a big issue for
#   debugging compilation errors.
#
#   Revision 1.5  2007/07/19 20:43:41  bucknerk
#
#   Updated Makefile should now hadle everything.  Did not really change the
#   others just vi and save
#
#   Revision 1.4  2007/07/19 17:57:08  bucknerk
#   Well I think that this is fimally capable of compiling all the utilities.  Still may need to
#   add something but I don't know what it is yet.
#
#   Revision 1.3  2007/07/19 13:27:40  bucknerk
#   I think that this will now correctly make the utility executables. Have to
#   add "getparm" but that should not be too serious.
#
#   Revision 1.2  2007/07/17 19:12:40  bucknerk
#   Don't need this any more.
#
#   Revision 1.1  2007/07/17 18:40:01  bucknerk
#   New for all the utilities
#
#
#
#####################################################
# Sanity check to make sure that his called from the correct place

ifndef DEV

verybad:	
	@echo "This should only be called from the TOP-LEVEL MAKEFILE"
	@echo "which should be 2 levels above the 'src' directory."
	@echo "If you don't do it correctly, a path constant may not"
	@echo "be correctly set in the cina_core.o"
endif

CINA_SERVER_FILES = $(ROOT)/cina-server/lib

# Shell command to run compiler
FC = ifort
CC = gcc

# Compiler options for warnings (default is to not display them)
W = -W1

# Default compiler options to create executable file
# Please note that there are no optimizations here 
# Also for the ifort 9.1 compiler -cerrs does not seem to be an option
# The -D option should be set in the toplevel makefile.  By default it is 1
# which means the development version is being compiled.  If it is 0 then 
# the production version
#FFLAGS = -g -L$(LIB) -module $(LIB) -DDEV=$(DEV) -L$(CINA_SERVER_FILES) -cerrs $(W) -nbs

FFLAGS = -g -L$(LIB) -L/usr/lib64/mysql -module $(LIB) -DDEV=$(DEV) $(W) 

######
## ASSORTED FLAGS AND VARIABLES FOR EACH EXECUTABLE
######
FMTOUTPUT_DEP=io.o parm_output.o convert.o fileio.o options.o reaction.o math.o \
	reactionstrings.o

IMPORTLIB_DEP=reactionstrings.o io.o math.o reaction.o convert.o \
	fileio.o constants.o nuc_data.o inv_parm.o bind.o \
	cina_core.o rate_man_core.o rate_man.o large_data.o fmt_parm.o

IMPORTLIB_LD_FLAGS=-L$(CINA_SERVER_FILES) -lmysqlpp \
	     -lmysqlclient -lF_cina_server -lcina_server -lstdc++
#	     -lmysqlclient -lxml++-2.6 -lF_cina_server -lcina_server

INPUTREAD_DEP=convert.o fileio.o io.o options.o reactionstrings.o nuc_data.o

INPUTCHECK_DEP=convert.o fileio.o io.o options.o math.o reaction.o \
	 reactionstrings.o nuc_data.o

INVERSE_DEP=bind.o constants.o fileio.o io.o inv_parm.o convert.o io.o nuc_data.o \
	      reactionstrings.o

RATEGEN_DEP=io.o convert.o reaction.o reactionstrings.o fileio.o math.o bind.o

REACINFO_DEP=convert.o reactionstrings.o fileio.o io.o options.o reaction.o math.o \
	       cina_core.o

TEMPERATURE_DEP=bind.o reactionstrings.o fileio.o convert.o io.o reaction.o math.o \
	  nuc_data.o

# Theoretically gnu make searches the VPATH when it is looking for dependencies
############################################################################
VPATH = $(LIB):$(BIN):.
.SUFFIXES:
.SUFFIXES: .f90 .f .o

%.o: %.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $@  $<

%.o: $.f
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $@  $<

############################################################################
.PHONY: all clean vars tidy

all: $(BIN)/fmtoutput $(BIN)/getparm $(BIN)/inputcheck \
	$(BIN)/inputread $(BIN)/inverse $(BIN)/rategen \
	$(BIN)/urlencode $(BIN)/temp_range $(BIN)/use_syslog \
	$(BIN)/reacinfo $(BIN)/importlib $(BIN)/ratecalc

clean tidy:
	@/bin/rm -f *.o *.mod

###############

#fmtoutput: fmtoutput.f90 $(FMTOUTPUT_DEP)
#	$(FC) $(FFLAGS) -o $(BIN)/$@ fmtoutput.f90 $(FMTOUTPUT_DEP) 
#

vars:
	@echo UTILITY VARS
	@echo BIN = $(BIN)
	@echo LIB = $(LIB)
	@echo SRC = $(SRC)
	@echo ROOT = $(ROOT)

$(BIN)/fmtoutput: fmtoutput.f90 $(LIB)/libcina.a
	$(FC) $(FFLAGS) -o $@ fmtoutput.f90 -lcina

$(BIN)/ratecalc: ratecalc.f90 $(LIB)/libcina.a
	$(FC) $(FFLAGS) -o $@ ratecalc.f90 -lcina

$(BIN)/importlib: importlib.f90 importlib_core.f90 $(LIB)/libcina.a
	@echo "Making $@"
	$(FC) $(FFLAGS) -L/usr/local/lib -L/usr/lib64/mysql \
	-L$(CINA_SERVER_FILES) -o $@ importlib_core.f90 \
	importlib.f90 -lcina -lF_cina_server -lcina_server \
	-lmysqlpp -lmysqlclient -lstdc++
#	-lmysqlpp -lmysqlclient -lxml++-2.6 
	
$(BIN)/inputcheck: inputcheck.f90 $(LIB)/libcina.a
	$(FC) $(FFLAGS) -o $@ inputcheck.f90 -lcina

$(BIN)/inputread: inputread.f90 $(LIB)/libcina.a
	$(FC) $(FFLAGS) -o $@ inputread.f90 -lcina

$(BIN)/inverse: inverse.f $(LIB)/libcina.a
	$(FC) $(FFLAGS) -o $@ inverse.f -lcina

$(BIN)/rategen: reaction_main.f derivative.f polint.f splint.f $(LIB)/libcina.a
	$(FC) $(FFLAGS) -o $@ reaction_main.f derivative.f polint.f splint.f \
	-lcina

$(BIN)/reacinfo: reacinfo.f90 $(LIB)/libcina.a
	$(FC) $(FFLAGS) -L$(CINA_SERVER_FILES) -o $@ reacinfo.f90 -lcina \
	  -lF_cina_server -lcina_server -lmysqlpp -lmysqlclient -lstdc++
#	  -lF_cina_server -lcina_server -lmysqlpp -lmysqlclient -lxml++-2.6 

$(BIN)/temp_range: temp_range.f $(LIB)/libcina.a
	$(FC) $(FFLAGS) -o $@ temp_range.f -lcina

$(BIN)/getparm: transform.f90 getparm.f90 pikaia.f subs_funcs.f \
	parmfit.f smartfit.f
	$(FC) $(FFLAGS) -o $@ $^

$(BIN)/use_syslog: use_syslog.c
	$(CC) $(CFLASG) -o $@ use_syslog.c

$(BIN)/urlencode: urlencode.c
	$(CC) $(CFLASG) -o $@ urlencode.c
