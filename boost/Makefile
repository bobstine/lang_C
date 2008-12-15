# $Id: Makefile,v 1.24 2008/01/04 14:32:29 bob Exp $

include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = boost

OPT = -O2 -DNDEBUG

# make sure that boost (with content) is in /usr/local/include

EXTERNAL_USES = boost_regex-mt-1_35

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = # regex.o  csv_parser.o
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
	./csv_parser < test-data.csv


###########################################################################

include ../rules_for_makefiles

