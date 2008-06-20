# $Id: Makefile,v 1.24 2008/01/04 14:32:29 bob Exp $

include ../c_flags

###########################################################################
#
#        Options
#
#   Examples of depending on libaries
#        LDLIBS =  -lthing -lregression 
#        libs_used =  ../lib/libthing.a ../lib/libregression.a 
# 
###########################################################################

INSTALL_PATH = ../utils

PROJECT_NAME = utils

OPT = -O2 -DNDEBUG

LDLIBS   = -lgsl -lm 

INCLUDES = -I. -I-


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = print_utils.o stat_utils.o function_utils.o bennett.o normal.o file_utils.o coding.o cyclic_iterator.o gsl_iterator.o operator_traits.o data_registry.o
level_1   = tags.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################

cols_to_rows: columns_to_rows.o
	$(CC) columns_to_rows.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/ctor

mands: mands.o
	$(CC) mands.o $(LDLIBS) -o $@

cutt: cutt.o
	$(CC) cutt.o $(LDLIBS) -o $@

###########################################################################

include ../rules_for_makefiles

