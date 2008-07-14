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

level_0   =  regex.o
level_1   =

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

hello:	hello.o
	$(CC) $<  $(LDLIBS) -o $@
	echo 1 2 3 | ./hello

regex: regex.o
	$(CC) $< $(LDLIBS) -o $@
	echo "subject: these\n" "from joe\n" "help\n" "Subject: Re:test message\n"| ./regex

###########################################################################

include ../rules_for_makefiles

