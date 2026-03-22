NUCASTROLIB_DEP = $(LIB)/convert.mod $(LIB)/reactionstrings.mod $(LIB)/fileio.mod \
		  $(LIB)/io.mod $(LIB)/options.mod $(LIB)/reaction.mod $(LIB)/math.mod \
		  $(LIB)/constants.mod $(LIB)/fmt_parm.mod $(LIB)/inv_parm.mod \
		  $(LIB)/nuc_data.mod $(LIB)/bind.mod $(LIB)/parm_output.mod

NUCASTROSERVER_DEP = $(LIB)/cina_core.mod $(LIB)/read_em_sim.mod $(LIB)/element_sim.mod\
		     $(LIB)/rategen.mod $(LIB)/nuc_data_man.mod \
		     $(LIB)/nuc_data_core.mod $(LIB)/rate_man.mod $(LIB)/rate_man_core.mod \
		     $(LIB)/large_data.mod

CINAADMIN_L_DEP = $(NUCASTROLIB_DEP)
CINAADMIN_S_DEP = cinaadmin_core.o $(NUCASTROSERVER_DEP)

CINA_SERVER_FILES = $(ROOT)/cina-server/lib

ifndef DEV
verybad:
	@echo MUST START WITH TOP_LEVEL (up two directories) MAKEFILE
	@echo That is where the 'DEV' value is assigned, and it determines 
	@echo whether this is a development version or not!
endif

ifeq (0,$(DEV))
NAME = cina
ROOT_DIR = $(WEB_ROOT)/cina_files
else
NAME = cinadev
ROOT_DIR = $(WEB_ROOT)/cina_files_dev
endif

ADMIN_NAME = cinaadmin

FC = ifort

# Compiler options for warnings (default is to not display them)
W = -W1

###
# Default compiler options to create executable file (-g removed E. Lingerfelt)
# The -cerrs is not an option for either ifort 9.1 or gfortran
# -nbs same as -assume nbscc where backslash is treated as a regular
# character in literals
###
#FFLAGS = -I $(TMP) -I$(CINA_SERVER_FILES) -I$(MYSQLCLIENT_INC_DIR) -I$(MYSQLPP_INC_DIR) -module $(TMP) -cerrs $(W) -nbs
#FFLAGS = -I $(TMP) -I$(CINA_SERVER_FILES) -I$(MYSQLCLIENT_INC_DIR) -I$(MYSQLPP_INC_DIR) -O3 $W -nbs
FFLAGS = -I$(LIB) -O3 $W

LDFLAGS = -L/usr/local/lib -L$(CINA_SERVER_FILES) -L$(ROOT)/cina/lib \
	  -L/usr/lib64/mysql \
	  -lmysqlpp -lmysqlclient -lF_cina_server \
	  -lcina_server 

VPATH = $(LIB):$(BIN):$(WEB_ROOT):$(WEB_ROOT)/$(ROOT_DIR):.
.SUFFIXES:
.SUFFIXES: .f90 .mod

%.mod:
	@echo $@

.PHONY : all always utility copy

all: always $(LIB)/libcina.a utility $(BIN)/$(NAME) $(BIN)/makemovie 
	@echo "ALL DONE"

always:
	@echo "Making NUCASTROLIB"
	@echo " "
	@cd nucastrolib && $(MAKE) all 
	@echo "SUCCESS:  NUCASTROLIB compiled"
	@echo " "
	@echo "Making NUCASTROSERVER"
	@echo " "
	@cd nucastroserver && $(MAKE) all
	@echo "SUCCESS: NUCASTORSERVER compiled"
	@echo " "

utility:
	@echo "Making UTILITIES"
	@echo " "
	@cd utilities && $(MAKE) all
	@echo "SUCCESS: UTILITIES compiled"
	@echo " "

$(BIN)/makemovie: makemovie
	@echo "COPYING the script"
	cp -uf $(SRC)/makemovie $(BIN)
	/usr/bin/dos2unix $(BIN)/makemovie

$(BIN)/$(NAME): cina.f90 $(LIB)/libcina.a $(CINA_SERVER_FILES)/libcina_server.a \
	$(CINA_SERVER_FILES)/libF_cina_server.a
	@echo "Making CINA"
	@echo " " 
	$(FC) $(FFLAGS) -o $@ cina.f90 $(LIB)/libcina.a $(LDFLAGS) -lstdc++

$(LIB)/libcina.a:
	ar rcs $(LIB)/libcina.a $(?:.mod=.o)

$(LIB)/libcina.a: $(NUCASTROLIB_DEP) $(NUCASTROSERVER_DEP)


dumpUsers: $(LIB)/libcina.a
	@echo "Making $@"
	@cd nucastroserver && $(MAKE) $(CINAADMIN_S_DEP) TMP=$(TMP) \
		LIB=$(LIB) LIB_PREFIX=$(LIB_PREFIX)
	@echo "SUCCESS: nucastroserver compiled"
	cd $(LIB) && $(FC) $(FFLAGS) -o $(BIN)/$@ $(SRC)/dumpUsers.f90 \
		$(CINAADMIN_L_DEP) $(CINAADMIN_S_DEP) $(LDFLAGS)
	@echo "SUCCESS: $@ compiled into $(BIN)"

erase: $(LIB)/libcina.a
	@echo "Making $@"
	cd $(LIB) && $(FC) $(FFLAGS) -o $(BIN)/$@ $(SRC)/erase.f90 \
		$(NUCASTROLIB_DEP) $(NUCASTROSERVER_DEP) 

tfmt: $(LIB)/libcina.a
	@echo "Making $@"
	cd $(LIB) && $(FC) $(FFLAGS) -o $(BIN)/$@ $(SRC)/tfmt.f90 \
		$(NUCASTROLIB_DEP)

foreachlib: $(LIB)/libcina.a dirlist.o
	@echo "Making $@"
	cd $(LIB) && $(FC) $(FFLAGS) -o $(BIN)/$@ $(SRC)/foreachlib.f90 dirlist.o \
		$(NUCASTROLIB_DEP) $(NUCASTROSERVER_DEP) 
	cp -f $(SRC)/ws $(SRC)/makemovie $(BIN)
	@echo "SUCCESS: $@ compiled into $(BIN)"



cinaadmin: cinaadmin.f90 $(LIB)/libcina.a
	@echo "Making CINAADMIN"
	@echo " " 
	cd $(LIB) && $(FC) $(FFLAGS) -o $(BIN)/$@ $(SRC)/cinaadmin.f90 \
		$(CINAADMIN_L_DEP) $(CINAADMIN_S_DEP) $(LDFLAGS)
	@echo "SUCCESS: $@ compiled into $(BIN)"
	@echo " "


dirlist.o: dirlist.c

.PHONY : install installcina installcinaadmin remove removecina \
	removecinaadmin removeall

install: $(NAME) $(WEB_ROOT)/cgi-bin $(ROOT_DIR) \
	$(ROOT_DIR)/adm $(ROOT_DIR)/bin \
	$(ROOT_DIR)/tmp $(ROOT_DIR)/trash \
	$(ROOT_DIR)/PUBLIC/rate_libs $(ROOT_DIR)/SHARED/rate_libs
	@echo "Installing cina into $(WEB_ROOT)"
	@echo "Executable name is $(NAME)"
	@echo " "
	chmod 0755 $(BIN)/$(NAME) $(BIN)/fmtoutput $(BIN)/getparm
	chmod 0755 $(BIN)/inputcheck $(BIN)/inputread
	chmod 0755 $(BIN)/inverse $(BIN)/makemovie
	chmod 0755 $(BIN)/rategen $(BIN)/temp_range
	chmod 0755 $(BIN)/urlencode $(BIN)/use_syslog
	chmod 0755 $(BIN)/ratecalc
	cp -u $(BIN)/$(NAME) $(WEB_ROOT)/cgi-bin
	cp -u $(BIN)/fmtoutput $(BIN)/getparm $(ROOT_DIR)/bin
	cp -u $(BIN)/inputcheck $(BIN)/inputread $(ROOT_DIR)/bin
	cp -u $(BIN)/inverse $(BIN)/makemovie $(ROOT_DIR)/bin
	cp -u $(BIN)/rategen $(BIN)/temp_range $(ROOT_DIR)/bin
	cp -u $(BIN)/urlencode $(BIN)/use_syslog $(ROOT_DIR)/bin
	cp -u $(BIN)/ratecalc $(ROOT_DIR)/bin
# this is not really necessary except that it ensures that if there is
# anything already there, the list is not replaced by an older list, maybe
# AND it creates the possibly empty files for programs that expect them to
# already exist
	touch $(ROOT_DIR)/adm/OpenIDs
	touch $(ROOT_DIR)/adm/OpenIDs_backup
	@echo "SUCCESS: cina installed into $(WEB_ROOT)"
# set permissions for EVERYTHING (default is 0664 with owner root and group
# apache) Now these things will not work correctly unless you execute this
# this as root. 

permissions:
	chmod 2775 $(ROOT_DIR)
	chown root:apache $(ROOT_DIR)
	find $(ROOT_DIR) -type d -exec chmod 2775 {} \;
	find $(ROOT_DIR)/adm -type f -exec chmod 0664 {} \;
	find $(ROOT_DIR)/USER -type f -exec chmod 0664 {} \;
	find $(ROOT_DIR)/PUBLIC -type f -exec chmod 0664 {} \;
	find $(ROOT_DIR)/SHARED -type f -exec chmod 0664 {} \;
	find $(ROOT_DIR)/trash -type f -exec chmod 0664 {} \;
	find $(ROOT_DIR)/tmp -type f -exec chmod 0664 {} \;
	find $(ROOT_DIR)/bin -type f -exec chmod 0755 {} \;


$(WEB_ROOT)/cgi-bin $(ROOT_DIR): 
	@echo "You must create $@ as root"
	@echo "chgrp to apache and chmod 2775"
	$(error)
$(ROOT_DIR)/adm $(ROOT_DIR)/bin:
	@echo "You must create $@ as root"
	@echo "chgrp to apache and chmod 2775"
	$(error)
$(ROOT_DIR)/tmp $(ROOT_DIR)/trash :
	@echo "You must create $@ as root"
	@echo "chgrp to apache and chmod 2775"
	$(error)
$(ROOT_DIR)/PUBLIC/rate_libs :
	@echo "You must create $@ as root"
	@echo "chgrp to apache and chmod 2775"
	$(error)
$(ROOT_DIR)/SHARED/rate_libs :
	@echo "You must create $@ as root"
	@echo "chgrp to apache and chmod 2775"
	$(error)

.PHONY : clean tidy

clean tidy:
	cd nucastroserver && $(MAKE) $@
	cd nucastrolib && $(MAKE) $@
	cd utilities && $(MAKE) $@
	@echo "nothing else to do here (src) for clean"
