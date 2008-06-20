# $Id: Makefile,v 1.1 2005/06/13 20:47:50 bob Exp $

#------------------------------------------------------------------------------
#
#  Makefile notes
#	target : dependencies
#		commmand...
#	
#	$@ refers to current target name
#	$? are more recent dependencies
#	$< is name of the dependency (as in rules with % matching) 
#	$* matches the stem of a matched dependency
#
#	Rules	The `target'.c files are automatically included as dependency.
#		The wildcard `*' does not expand when used in dependencies.
#			(use `$(wildcard *.c)' to get the desired effect)
#
#------------------------------------------------------------------------------

################################################################################
#
#    Definitions of the compiler, loader
#
#  need option? -D__STL_USE_NAMESPACES
#

INCLUDES = -I /Users/bob/C/random -I /Users/bob/C/ranges

CC = g++ $(INCLUDES) $(LDFLAGS) 

# slow and careful
#OPT = -O0 
# fast and who givs a damn about errors?
OPT = -O2 -DNDEBUG

LDLIBS = -lm

################################################################################
# -Wall                 : the first set of primary warnings
# -Wold-style-cast      : generates way to many errors in the g++ library code
# -Weffc++              : cool, but turn on FEWER_WARN to avoid extranious warnings
# -W                    : turns on a few extras
# -Werror               : treats all warnings as errors
# -Woverloaded-virtual  : catches mismatched signatures in virtual overloads
# -Winline		: says if a function can't be inlined

CFLAGS = -finline-functions -gstabs+ -Wall -Werror -Winline $(OPT)

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#

level_0   = matrix.o variable_abc.o statistic_base.o  
level_1   = variable.o dataset.o			$(level_0)
level_2   = statistic.o					$(level_1)
level_3   = gsmodel.o					$(level_2)


top_level = $(level_2)

OBJECTS = $(top_level)

SOURCES = $(OBJECTS:.o=.cc)

TEST_SOURCES = $(wildcard *.test.cc)

###########################################################################
#
#	Main targets
#
###########################################################################

ranges: range.test range_ops.test range_traits.test

regr: $(OBJECTS)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $(OBJECTS)

.PHONY : clean
clean:
	-rm -f sweeper driver *.o *.d
	-rm core



bennett.test: bennett.test.o bennett.o
	$(CC) $(LDLIBS) -o $@ $@.o bennett.o | c++filt

range.test: range.test.o range.o $(level_0)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_0) | c++filt

speed.test: speed.test.o range.o $(level_0)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_0) | c++filt

matrix.test: matrix.test.o matrix.o $(level_0)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_0) | c++filt

random.test: random.test.o random.o $(level_0)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_0) | c++filt

variable.test: variable.test.o $(level_2)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_2) | c++filt

dataset.test: dataset.test.o $(level_3)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_3) | c++filt

statistic.test: statistic.test.o $(level_3)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_3) | c++filt

gsmodel.test: gsmodel.test.o $(level_4)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_4) | c++filt


%.test.o: %.test.cc
	$(CC) $(CFLAGS) -c $< -o $@ 

%.o: %.cc
	$(CC) $(CFLAGS) -c $< -o $@ 


############################################################################
#
#			DEPENDENCIES
#
# From the GNU make info node and _Gnu make_ book (neither worked unmodified)
# book version:	$(CC) -M $(CPPFLAGS) $< | sed '\''s/\($*\)\.o[ :]*/\1 $@/g'\'' > $@
# info version:	$(CC) -M $(CPPFLAGS) $< | sed 's/$*.o/& $@/g' > $@
#
#   Using MM option suppresses the system dependencies (use M otherwise)
#

%.test.d: %.test.cc
	$(CC) -MM $(CPPFLAGS) $< | sed 's/$*\.test\.o/& $@/g' > $@

%.d: %.cc
	$(CC) -MM $(CPPFLAGS) $< | sed 's/$*\.o/& $@/g' > $@

include $(SOURCES:.cc=.d)
include $(TEST_SOURCES:.cc=.d)

#####################  EOF  ###############################################
