# $Id: Makefile,v 1.39 2004/08/27 18:29:10 bob Exp $

include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

C_PATH = $(HOME)/C

all:  sparse_iterator.test.exec     range.test.exec      range_traits.test.exec\
      function_iterators.test.exec  range_ops.test.exec  range_stats.test.exec
# make_range.test.exec apl.test.exec


PROJECT_NAME = ranges

OPT      =  -O2  -DNDEBUG

INCLUDES = -I. -I$(C_PATH)/utils -I$(C_PATH)/random -I-

LDLIBS   = -lm -lutils -lrandom
LDFLAGS  = -L$(C_PATH)/utils -L$(C_PATH)/random


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

