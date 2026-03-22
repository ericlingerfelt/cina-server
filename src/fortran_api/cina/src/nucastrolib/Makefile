# Sanity check to make sure that his called from the correct place
ifndef DEV

verbad:	
	@echo "This should only be called from the TOP-LEVEL MAKEFILE"
	@echo "which should be 2 levels above the 'src' directory."
	@echo "If you don't do it correctly, a path constant may not"
	@echo "be correctly set in the cina_core.o"
endif

# end of sanity check
### 

FC = ifort
# Compiler options for warnings (default is to not display them)
W = -W1

#FFLAGS = -g -I $(LIB) -module $(LIB) -cerrs $(W)
FFLAGS = -g -I$(LIB) -module $(LIB) $(W)

# All fortran files in nucastrolib
ALL_FILES = $(LIB)/bind.o $(LIB)/constants.o $(LIB)/convert.o $(LIB)/fileio.o \
	    $(LIB)/inv_parm.o $(LIB)/io.o $(LIB)/math.o  $(LIB)/nuc_data.o \
	    $(LIB)/options.o $(LIB)/parm_output.o $(LIB)/reaction.o \
	    $(LIB)/reactionstrings.o $(LIB)/fmt_parm.o

VPATH = $(LIB):.

.PHONY: all

all: $(ALL_FILES)

$(LIB)/bind.o $(LIB)/bind.mod: bind.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/bind.o $<

$(LIB)/constants.o $(LIB)/constants.mod: constants.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/constants.o $<

$(LIB)/convert.o $(LIB)/convert.mod: convert.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/convert.o $<

$(LIB)/fileio.o $(LIB)/fileio.mod: fileio.f90 convert.mod io.mod \
	reaction.mod reactionstrings.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/fileio.o $<

$(LIB)/fmt_parm.o: fmt_parm.f90 constants.mod reactionstrings.mod \
	convert.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/fmt_parm.o $<

$(LIB)/inv_parm.o: inv_parm.f90 bind.mod nuc_data.mod \
	reactionstrings.mod constants.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/inv_parm.o $<

$(LIB)/io.o $(LIB)/io.mod: io.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/io.o $<

$(LIB)/math.o $(LIB)/math.mod: math.f90 io.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/math.o $<

$(LIB)/nuc_data.o $(LIB)/nuc_data.mod: nuc_data.f90
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/nuc_data.o $<

$(LIB)/options.o $(LIB)/options.mod: options.f90 constants.mod io.mod \
	fileio.mod convert.mod reactionstrings.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/options.o $<

$(LIB)/parm_output.o: parm_output.f90 constants.mod io.mod \
	options.mod reactionstrings.mod convert.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/parm_output.o $<

$(LIB)/reaction.o $(LIB)/reaction.mod: reaction.f90 math.mod \
	io.o
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/reaction.o $<

$(LIB)/reactionstrings.o $(LIB)/reactionstrings.mod: reactionstrings.f90 \
	nuc_data.mod
	@echo "Making $@"
	$(FC) $(FFLAGS) -c -o $(LIB)/reactionstrings.o $<

.PHONY : install clean

install:
	@echo "Nothing to do for install"

clean:
	@echo "Nothing to do for clean"
