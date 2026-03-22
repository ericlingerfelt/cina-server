####### # TOP-LEVEL Makefile for the CINA suite of programs.

###########
# Had to do this to keep the directories straight and prevent
# hardcoding
# this does not do anything but change to cina or cina-server and 
# run make all, install, clean
# ##########

#####
# This keeps everything in the correct place and 
# straight for compiling cina, xnet, and cina all from a top-level 
# directory
# The export command keeps me from having to pass the values
# to each sub-version of Make
#####

## IF YOU HAVE PROBLEMS with the directory being properly created
# you might have to fix this.  after 'shell pwd' there is an extra
# space at the end of the string and the make function 'strip' is 
# supposed to take care of that. 
ROOT = $(strip $(shell pwd))
WEB_ROOT = /var/www

###
# If compiling the PRODUCTION VERSION set DEV to 0 otherwise leave
# it set to 1.  In the nucastroserver/cina_core.f90 it changes the 
# directory used from "cina_files" to "cina_files_dev"
# changes the name of the cina executable, utility install directories
# same for xnet and in cina changes the name of the executables
###

DEV = 0

export ROOT
export WEB_ROOT
export DEV

bad:
	@echo "If making a production version EDIT THIS FILE!!!!"
	@echo "The default is a development version, to create the"
	@echo "production version you need to change the variable"
	@echo "declaration 'DEV = 1' to 'DEV = 0'"
	@echo -e "You can make \n\tall,\n\tall-install,\n\tall-tidy,\n\tall-clean,"
	@echo -e "\tcina,\n\tcina-install,\n\tcina-tidy,\n\tcina-clean,\n\tcina-server,"
	@echo -e "\tcina-server-install,\n\tcina-server-tidy,\n\tcina-server-clean,\n\txnet,"
	@echo -e "\txnet-install,\n\txnet-tidy,\n\txnet-clean"

.PHONY: all all-install all-tidy all-clean cina cina-install cina-tidy cina-clean \
	cina-server cina-server-install cina-server-tidy cina-server-clean \
	xnet xnet-install xnet-tidy xnet-clean

all: cina-server cina xnet

all-install: cina-install cina-server-install xnet-install
	@echo "Test the product and if there is a problem"
	@echo "with file permissions run 'make all-install AFTER"
	@echo "changing to root"

all-clean: cina-clean cina-server-clean xnet-clean

all-tidy: cina-tidy cina-server-tidy xnet-tidy

all-perms: cina-perms cina-server-perms xnet-perms


cina: cina-server
	cd cina && $(MAKE) all
cina-install: cina-server-install
	cd cina && $(MAKE) install
cina-clean: cina-server-clean
	cd cina && $(MAKE) clean
cina-tidy: cina-server-tidy
	cd cina && $(MAKE) tidy
cina-perms:
	cd cina && $(MAKE) permissions


cina-server:
	cd cina-server/src && $(MAKE) all
cina-server-install:
	cd cina-server/src && $(MAKE) install
cina-server-clean:
	cd cina-server/src && $(MAKE) clean
cina-server-tidy:
	cd cina-server && $(MAKE) tidy
cina-server-perms:
	cd cina-server && $(MAKE) permissions



xnet:
	cd xnet_cina && $(MAKE) all
xnet-install:
	cd xnet_cina && $(MAKE) install
xnet-clean:
	cd xnet_cina && $(MAKE) clean
xnet-tidy:
	cd xnet_cina && $(MAKE) tidy
xnet-perms:
	cd xnet_cina && $(MAKE) permissions
