##############################################################
# Makefile for FullNet and associated programs
# This is from W. Rafael Hix
#
# Note that the net_setup.f is the cina version and the save_em_sim.f is
# really from the same place.
#
# Rafe fixed this so that things are correct. I am going to remove a BUNCH of
# extraneous commented-out lines to reduce confusion.  There should be little
# reason to change this unless the compiler changes or CINA is moved to a
# different machine/OS.
##############################################################
# Architecture and installation specific MACROS
# Compiler options for the Intel FORTRAN compiler
FC 	= ifort          # FORTRAN compiler
LDR	= ifort          # program linker, for precompiled objects
DEBUG   = -g -traceback         # options for debugging 
#PROFILE = -p -pg	 # compiler options for profiling
#PROFILE = -p 	 	# compiler only need -p because it expects to use gproff
PROFILE = 		# nothing really needed, we do not want the prof output
OPT     = -O3 

MKL_PATH= /opt/intel/composer_xe_2015.2.164/mkl/
LAPACK  =  -I$(MKL_PATH)include -L$(MKL_PATH)lib/intel64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
PARDISO =  -I$(MKL_PATH)include -L$(MKL_PATH)lib/intel64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core 

# Active options
FFLAGS 	= $(OPT) -autodouble -align -132 # active compiler options
LDRFLAGS=  -autodouble -align $(OPT) # linker flags
LIBS	=  $(LAPACK) # Libraries need at linking

ifndef DEV
verybad:
	@echo Need to run make from the TOP-LEVEL directory please
endif

ifeq (1,$(DEV))
INSTALL_DIR=$(WEB_ROOT)/cina_files_dev/bin
else
INSTALL_DIR=$(WEB_ROOT)/cina_files/bin
endif

# Default rules for compiling fortran files
.SUFFIXES: .f .f90
# Make target with first .f or .f90 file
.f.o:
	$(FC) $(FFLAGS) $(DEBUG) $(PROFILE) -c $<
.f90.o:
	$(FC) $(FFLAGS) $(DEBUG) $(PROFILE) -c $<

# These are the rules which make uses to build executables
# The default rule is to make xnet

.PHONY: all clean tidy install permissions

all: xnet net_setup 

###
#  This is the version that assumes that the matrix is in fact a dense
#  matrix and does nothing special. 
xnet_dense: data.o full_net.o match.o flux.o eosnom90.o jacobian_dense.o \
	save_em_sim.o net.o
	$(LDR) $(LDRFLAGS) $(DEBUG) $(PROFILE) -o xnet_dense $^ $(LIBS)
###
#  This is the version that assumes that the matrix is sparse, which it is
#  and uses the MA28 sparse solver. 
xnet_ma28: data.o full_net.o match.o flux.o eosnom90.o jacobian_ma28.o \
	  MA28.o save_em_sim.o net.o
	$(LDR) $(LDRFLAGS) $(DEBUG) $(PROFILE) -o xnet_ma28 $^ $(LIBS)
###
#  This is the version that assumes that the matrix is sparse, which it is
#  and uses the PARDISO sparse solver. We are going to call it xnet
xnet: data.o full_net.o match.o flux.o eosnom90.o jacobian_PARDISO.o \
	  save_em_sim.o net.o
	$(LDR) $(LDRFLAGS) $(DEBUG) $(PROFILE) -o xnet $^ $(LIBS) $(PARDISO)
###
#  This is required by xnet to make the 'network' files so it
#  can run the solver on the sparse matrix. The .f file was modified to run
#  with the on-line version of CINA
net_setup: data.o net_setup.o 
	$(LDR)  $(LDRFLAGS) -o net_setup $^
	@echo "To test move/copy net_setup to ./mc"

# Rules for automating routine tasks
tidy :
	rm -f core *.o *.mod
clean:
	rm -f core *.o *.mod
	rm -f abund* gmon.out isomap lastzone net_diag 
	rm -f reacmap step_* thermo* timemap*
	@echo You should also remove fluxN and fluxN_cnt

#  Dependencies for object files
data.o:        data.f
eosnom90.o:    eosnom90.f
flux.o:        flux.f full_net.o data.o match.o
full_net.o:    full_net.f data.o 
match.o:       match.f data.o
net.o:         net.f full_net.o data.o flux.o match.o save_em_sim.o
press.o:       press.f
net_setup.o:   net_setup.f data.o
jacobian_dense.o: jacobian_dense.f
jacobian_ma28.o: jacobian_ma28.f MA28.o
jacobian_PARDISO.o: jacobian_PARDISO.f 
MA28.o:	       MA28.f
save_em_sim.o: save_em_sim.f90 flux.o


install: xnet net_setup
	chmod 0755 xnet
	chmod 0755 net_setup
	cp -u xnet $(INSTALL_DIR)
	cp -u net_setup $(INSTALL_DIR)

# in case 'install' was run by other than root this can be run by root to
# fix the permissions
permissions:
	chmod 0755 $(INSTALL_DIR)/xnet
	chmod 0755 $(INSTALL_DIR)/net_setup
