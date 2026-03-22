#***
# Makefile  CINA/cina
# VERSION
#    $Author: bucknerk $
#    $Id: Makefile,v 1.2 2008/04/25 19:50:13 bucknerk Exp $
#    $Log: Makefile,v $
#    Revision 1.2  2008/04/25 19:50:13  bucknerk
#    now it compiles and installs on the new box "astroport"
#
#    Revision 1.1.1.1  2008/04/22 13:36:33  bucknerk
#    all in one place
#
#    Revision 1.10  2007/07/19 20:40:30  bucknerk
#    Update again. Hopefully this means that it will really work. fixed the
#    archive creation.
#
#
# PURPOSE
#    A makefile for compiling, installation, and documentating nucastrodata
#    programs
#    
#***


# Default path to suite files
LOCAL=$(ROOT)/cina
# Default path to source code files
SRC=$(LOCAL)/src


# Default path to compiled libraries
LIB = $(LOCAL)/lib

# Default path to store compiled programs
BIN = $(LOCAL)/bin

export SRC
export LIB
export BIN


# THIS is a sanity check to make sure that it is compiled from the correct place
ifndef DEV
verybad:
	@echo MUST START WITH TOP_LEVEL [CINA] (up one directories) MAKEFILE
	@echo That is where the 'DEV' value is assigned, and it determines 
	@echo whether this is a development version or not!
endif

.PHONY: all install clean tidy

############################################################################
# Executable Management

all: $(BIN) $(LIB)
	@echo Making CINA
	@cd $(SRC) && $(MAKE) all 

############################################################################
# Installation Management

install:
	@echo Making CINA-INSTALL
	@cd $(SRC) && $(MAKE) install



############################################################################
# Clean-up Management

clean:
	@cd $(SRC) && $(MAKE) clean
	rm -rf $(BIN) $(LIB)
tidy:
	@cd $(SRC) && $(MAKE) tidy
	rm -f $(LIB)/*.o

############################################################################

# How to make directory for compiled programs
$(BIN):
	mkdir -p $(BIN)

# How to make directory for compiled libraries
$(LIB):
	mkdir -p $(LIB)

