# $Id: Makefile,v 1.24 2008/01/04 14:32:29 bob Exp $

include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = boost

OPT = -O2      # -DNDEBUG  this option kills benefit of assert

# make sure that boost (with content) is in /usr/local/include

#####################
#
# To build boost (it takes quite a few minutes to build)
#
#	cd /usr/local/src/boost.latest.version
#   	sudo ./configure
#   	sudo make install
#
# These build quite a few libraries in /usr/local/lib.  Access these libraries
# as shown below by setting the appropriate EXTERNAL_USES names. 
#
#####################

# Use this for boost files that need compilation

EXTERNAL_USES = boost_regex-xgcc40-mt-1_38

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = csv_parser.o  regex.o
level_1   =

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

hello:	hello.o
	$(GCC) $<  -o $@
	echo 1 2 3 | ./hello

lambda:	lambda.o
	$(GCC) $<  -o $@
	./lambda 

regex: regex.o
	$(GCC) $< $(LDLIBS) -o $@
	echo "subject: these\n" "from joe\n" "help\n" "Subject: Re:test message\n"| ./regex

csv_parser_demo: csv_parser_demo.o
	$(GCC) $< $(LDLIBS) -o $@
	./csv_parser_demo

csv_parser: csv_parser.o test-data.csv
	$(GCC) $< $(LDLIBS) -o $@
	./csv_parser -o model.dat -f test-data.csv


###########################################################################

include ../rules_for_makefiles

