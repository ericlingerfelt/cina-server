#############################################################################
# $Author: bucknerk $
# $Date: 2008/04/25 16:11:10 $
# $Id: Makefile,v 1.8 2008/04/25 16:11:10 bucknerk Exp $
#
# $Log: Makefile,v $
# Revision 1.8  2008/04/25 16:11:10  bucknerk
# minor adjustments
#
# Revision 1.7  2008/03/26 20:08:15  bucknerk
# I think I have the waiting point finder giving good responses now and
# that it runs correctly with zones like 01, 05 and so on.
#
# Revision 1.6  2008/01/08 13:53:33  bucknerk
# added read_time_data
#
# Revision 1.5  2007/12/05 19:27:12  bucknerk
# fixed error reporting, fortran file handleing
#
# Revision 1.4  2007/12/03 17:48:50  bucknerk
# merged with branch cina-rel-1-0
#
# Revision 1.3.2.6  2007/12/03 17:43:42  bucknerk
# added parameter.dat to clean
#
# Revision 1.3.2.5  2007/12/03 17:42:27  bucknerk
# fixed clean to remove core
#
# Revision 1.3.2.4  2007/12/03 13:30:18  bucknerk
# finally fixed the segfault.  typically was freeing already freed memory. Cut and
# paste strikes again. Also updated some documentation.
#
# Revision 1.3.2.3  2007/11/28 21:15:13  bucknerk
# a few tweaks for i/o
#
# Revision 1.3.2.2  2007/11/28 18:35:25  bucknerk
# Renamed craz_list list_crazy.dat, updated the makefile for the create_input
# files and added all those create_input files.
#
#
#############################################################################
#
# Makefile for new waiting point code.  This handles the program that reads the 
# simulation data and creates a formatted file for Tomomi Sunayama's code to
# operate on and attempt to determine which isotopes are waiting points for a
# particular simulation.
############################################################################
PP=php
PFLAGS=-l
CC=gcc
CFLAGS=-ggdb3 -Wall
SQLFLAGS=-lmysqlclient -lz
#LDFLAGS=-lefence -lm
LDFLAGS=-lm
DCC=/home/bucknerk/bin/mydoxygen
DOC_DIR=../cina_eval_doc/wait_code
WAITCONF=wait.conf
FFLAGS=-ggdb
F77=g77





OBJECTS=main.o read1.o initialize_loop1.o output_fake.o declaration.o \
	integration_lifetime.o integration_flux.o logic_tree.o write_result.o \
	abundance_test.o abundance_AT.o lifetime_AT.o flux_AT.o \
	final_decision.o

.SUFFIXES: .F .f .c .o

all: newcode create_input read_time_data
	chmod 0755 $^


#this is just here for reference and in case. (of what I don't know)
outcode: main.f read1.f initialize_loop1.f output_fake.f declaration.f integration_lifetime.f integration_flux.f logic_tree.f write_result.f abundance_test.f abundance_AT.f lifetime_AT.f flux_AT.f final_decision.f
	$(F77) $(FFLAGS) -o outcode main.f read1.f initialize_loop1.f output_fake.f declaration.f integration_lifetime.f integration_flux.f logic_tree.f write_result.f abundance_test.f abundance_AT.f lifetime_AT.f flux_AT.f final_decision.f

# the one really being used.
newcode: $(OBJECTS)
	$(F77) $(FFLAGS) -o newcode $(OBJECTS)

create_input: create_input_main.c create_input.h data_file_functions.o rb.o
	$(CC) $(CFLAGS) create_input_main.c  \
	data_file_functions.o rb.o -o create_input $(LDFLAGS)
read_time_data: read_time_data.c create_input.h data_file_functions.o 
	$(CC) $(CFLAGS) read_time_data.c  \
	data_file_functions.o -o read_time_data $(LDFLAGS)

doc: $(WAITCONF)
	$(DCC) $(WAITCONF)
	chmod 0644 $(DOC_DIR)/*

	
%.o:%.c
	$(CC) $(CFLAGS) -c $<
%.o:%.F
	$(F77) $(FFLAGS) -DNEW -c $<
%.o:%.f
	$(F77) $(FFLAGS) -c $<

%: %.php
	$(PP) $(PFLAGS) $<
%: %.inc
	$(PP) $(PFLAGS) $<
%: %.html
	$(PP) $(PFLAGS) $<

clean:
	/bin/rm -f *.o core.* outcode newcode create_input parameter.dat
	/bin/rm -f read_time_data
tidy:
	/bin/rm -f *.o core.*
