#############################################################################
# $Author: bucknerk $
# $Date: 2008/07/10 19:17:28 $
# $Id: Makefile,v 1.6 2008/07/10 19:17:28 bucknerk Exp $
#
# $Log: Makefile,v $
# Revision 1.6  2008/07/10 19:17:28  bucknerk
# updated
#
# Revision 1.5  2008/07/07 19:06:11  bucknerk
# updated
#
# Revision 1.4  2008/06/13 15:32:26  bucknerk
# changed name of main input for create_bw_input target
#
# Revision 1.3  2008/04/25 16:11:09  bucknerk
# minor adjustments
#
# Revision 1.2  2008/03/25 18:09:30  bucknerk
# Modified to create a 'reverse' version, that is the fluxes are for the
# reactions that become the mass not that start from the mass
#
# Revision 1.1  2008/03/20 18:52:50  bucknerk
# still new
#
#
#
#############################################################################
#
# Makefile for new bottle-neck code.  This handles the program that reads the 
# simulation data and creates a formatted file for Tomomi Sunayama's code to
# operate on and attempt to determine which isotopes are waiting points for a
# particular simulation.
############################################################################
PP=php
PFLAGS=-l
CC=gcc
CFLAGS=-ggdb -Wall
SQLFLAGS=-lmysqlclient -lz
#LDFLAGS=-lefence -lm
LDFLAGS=-lm
DCC=/home/bucknerk/bin/mydoxygen
DOC_DIR=../cina_eval_doc/bottle_code
BOTTLCONF=btln.conf
FFLAGS=-ggdb
F77=g77





OBJECTS=

.SUFFIXES: .F .f .c .o

all: create_input read_time_data find_bottle_necks read_steps \
	read_masses
	chmod 0755 find_bottle_necks 


create_input: create_input_main.c create_input.h \
	data_file_functions.o rb.o
	$(CC) $(CFLAGS) create_input_main.c  \
	data_file_functions.o rb.o -o create_input $(LDFLAGS)
	chmod 0775 create_input

read_time_data: read_time_data.c create_input.h data_file_functions.o 
	$(CC) $(CFLAGS) read_time_data.c  \
	data_file_functions.o -o read_time_data $(LDFLAGS)
	chmod 0775 read_time_data

data_file_functions.o: data_file_functions.c create_input.h
	$(CC) $(CFLAGS) -c data_file_functions.c  

read_steps: step.c
	$(CC) $(CFLAGS) step.c -o read_steps
	chmod 0775 read_steps

read_masses: mass.c
	$(CC) $(CFLAGS) mass.c -o read_masses
	chmod 0775 read_masses

doc: $(BOTTLCONF)
	$(DCC) $(BOTTLCONF)
	chmod 0644 $(DOC_DIR)/*

find_bottle_necks: 
	cd fortran && $(MAKE)
	

	
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
	cd fortran && $(MAKE) clean
	/bin/rm -f *.o core.* create_input parameter.dat read_time_data 
	/bin/rm -f read_steps find_bottle_necks
tidy:
	cd fortran && $(MAKE) tidy
	/bin/rm -f *.o core.*
