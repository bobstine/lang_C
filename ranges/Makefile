include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

# make_range.test.exec apl.test.exec


PROJECT_NAME = ranges

USES = utils random

OPT = -Ofast -mfpmath=sse -msse3 -m64 -march=native

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

#           TESTED					UNTESTED
level_0   = range.o                               composer.o evaluator.o 
level_1   = range_traits.o column.o # ysparse_iterator.o 
level_2   = column_stream.o
level_3   = function_iterators.o 
level_4   = range_ops.o
level_5   = range_stats.o anonymous_iterator.o
# prior levels are pretty good
level_6   = vectorize.o  matrix.o
level_7	  = apl.o
level_8   = 

default: range_traits.test

all:  sparse_iterator.test     range.test   column.test    range_traits.test function_iterators.test  range_ops.test  range_stats.test

pair: pair-test.o
	$(CC) pair-test.o $(LDLIBS) -o $@

lambda: lambda.o
	$(CC) lambda.o $(LDLIBS) -o $@

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

