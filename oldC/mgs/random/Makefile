# $Id: Makefile,v 1.3 2002/06/17 14:32:50 bob Exp $

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

# CFLAGS = -finline-functions -gstabs+ -Wall -Werror -Winline $(OPT)
CFLAGS =  -gstabs+ -Wall -Werror 

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#

level_0   = utility.o
level_1   = random.o		$(level_0)
level_2	  = 			$(level_1)
level_3   = 		  	$(level_2)
level_4   = 			$(level_3)
level_5   = 			$(level_4)
level_6   = 			$(level_5)


top_level = $(level_1)

OBJECTS = $(top_level)

SOURCES = $(OBJECTS:.o=.cc)

TEST_SOURCES = $(wildcard *.test.cc)

###########################################################################
#
#	Main targets
#
###########################################################################


.PHONY : clean
clean:
	-rm -f sweeper driver *.o *.d
	-rm core

random.test: random.test.o $(level_1)
	$(CC) $(LDLIBS) -o $@ $@.o $(level_1) | c++filt

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
