ifndef DEV

verybad:	
	@echo "This should only be called from the TOP-LEVEL MAKEFILE"
	@echo "which should be 2 levels above the 'src' directory."
	@echo "If you don't do it correctly, a path constant may not"
	@echo "be correctly set in the cina_core.o"
endif

ALL_FILES = $(LIB)/reactionstrings.o $(LIB)/convert.o $(LIB)/fileio.o \
    $(LIB)/cina_core.o $(LIB)/constants.o \
    $(LIB)/io.o $(LIB)/rate_man_core.o \
    $(LIB)/fmt_parm.o $(LIB)/large_data.o $(LIB)/rate_man.o \
    $(LIB)/rategen.o $(LIB)/nuc_data_core.o $(LIB)/nuc_data_man.o \
    $(LIB)/read_em_sim.o $(LIB)/element_sim.o

# Default path to temp directory
TMP = $(ROOT)/tmp

# Default path for cina_server libraries
CINA_SERVER_FILES = $(ROOT)/cina-server/lib
 
FC = ifort

# Compiler options for warnings (default is to not display them)
W = -W1

# Default compiler options to create executable file
# Please note that there are no optimizations here 
# Also for the ifort 9.1 compiler -cerrs does not seem to be an option
# The -D option should be set in the toplevel makefile.  By default it is 1
# which means the development version is being compiled.  If it is 0 then 
# the production version
#FFLAGS = -g -L$(LIB) -module $(LIB) -DDEV=$(DEV) -L$(CINA_SERVER_FILES) -cerrs $(W) -nbs
FFLAGS = -g -I$(CINA_SERVER_FILES) -L$(LIB) -module $(LIB) -DDEV=$(DEV) $(W)

VPATH = $(TMP):$(LIB):$(BIN):.

.SUFFIXES: .f90 .o

.f90.o:
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/$@  $<

.PHONY: all clean tidy vars

all: $(ALL_FILES)

vars:
	@echo NUCASTROSERVER VARS
	@echo BIN = $(BIN)
	@echo LIB = $(LIB)
	@echo SRC = $(SRC)
	@echo ROOT = $(ROOT)

CINA: 
	@echo not really MAKING THE CINA.MOD available

.PHONY : install clean tidy

install:
	@echo "Nothing to do for install"

clean tidy:
	@echo "Nothing to do for $@"

$(LIB)/element_sim.o: element_sim.f90 read_em_sim.mod \
	cina_core.mod rate_man.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/element_sim.o  $<
$(LIB)/read_em_sim.o $(LIB)/read_em_sim.mod: read_em_sim.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/read_em_sim.o  $<
$(LIB)/large_data.o $(LIB)/large_data.mod: large_data.f90 constants.mod 
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/large_data.o  $<
$(LIB)/cina_core.o cina_core.mod: cina_core.f90 reactionstrings.mod \
	convert.mod fileio.mod 
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/cina_core.o  $<
$(LIB)/cinaadmin_core.o: cinaadmin_core.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/cinaadmin_core.o  $<
$(LIB)/rate_man.o rate_man.mod: rate_man.f90 rate_man_core.mod \
	reactionstrings.mod fmt_parm.mod large_data.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/rate_man.o  $<
$(LIB)/rate_man_core.o rate_man_core.mod: rate_man_core.f90 \
	constants.mod cina_core.mod convert.mod io.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/rate_man_core.o  $<
$(LIB)/rategen.o: rategen.f90 constants.mod cina_core.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/rategen.o  $<
$(LIB)/nuc_data_man.o: nuc_data_man.f90 nuc_data_core.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/nuc_data_man.o  $<
$(LIB)/nuc_data_core.o nuc_data_core.mod: nuc_data_core.f90 \
	constants.mod cina_core.mod convert.mod io.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/nuc_data_core.o  $<
