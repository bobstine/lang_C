# $Id: Makefile,v 1.42 2004/10/15 18:30:06 foster Exp $

include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

# make_range.test.exec apl.test.exec


PROJECT_NAME = ranges

OPT      =  -O4  -DNDEBUG

USES = utils random


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

#           TESTED					UNTESTED
level_0   = range.o                               composer.o evaluator.o 
level_1   = range_traits.o sparse_iterator.o
level_2   = function_iterators.o 
level_3   = range_ops.o
level_4   = range_stats.o anonymous_iterator.o

# the above levels should now be "set in concrete" and not change very much.
level_5   = vectorize.o  matrix.o
level_6	  = apl.o
level_7   = 

all:  sparse_iterator.test.exec     range.test.exec      range_traits.test.exec function_iterators.test.exec  range_ops.test.exec  range_stats.test.exec

pair: pair-test.o
	$(CC) pair-test.o $(LDLIBS) -o $@

iterators.test.exec: iterators.test.o
	$(CC) iterators.test.o $(above_1) $(LDLIBS) -o $@

speed.test.exec: speed.test.o
	$(CC) speed.test.o $(above_4) $(LDLIBS) -o $@

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################

include ../rules_for_makefiles

