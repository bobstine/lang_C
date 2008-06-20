# $Id: Makefile,v 1.7 2001/09/26 12:19:00 bob Exp $

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

CC = g++ $(INCLUDES) $(LDFLAGS) 
OPT = -O2
LDLIBS = -lm

################################################################################
# -Wall                 : the first set of primary warnings
# -Wold-style-cast      : generates way to many errors in the g++ library code
# -Weffc++              : cool, but turn on FEWER_WARN to avoid extranious warnings
# -W                    : turns on a few extras
# -Werror               : treats all warnings as errors
# -Woverloaded-virtual  : catches mismatched signatures in virtual overloads
#

CFLAGS = -gstabs+ -Wall -Werror $(OPT)
# CFLAGS = -gstabs+ -Wall $(OPT)


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#

level_0   = random.o observation.o
level_1   = dataset.o 				$(level_0)
level_2   = matrix.o				$(level_1)
level_3   = raggedArray.o			$(level_2)
level_4   = dataArray.o  			$(level_3)
level_5   = gsModel.o                   	$(level_4)
level_6   = driver.o                            $(level_5)

top_level = $(level_5)

OBJECTS = $(top_level)

SOURCES = $(OBJECTS:.o=.c)

TEST_SOURCES = $(wildcard *.test.c)

###########################################################################
#
#	Main targets
#
###########################################################################

gs: $(OBJECTS)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $(OBJECTS)

.PHONY : clean
clean:
	-rm -f sweeper driver *.o *.d
	-rm core

coding.test: coding.test.o $(level_0)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $@.o $(level_0)

dataset.test: dataset.test.o $(level_1)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $@.o $(level_1) | c++filt

operator.test: operator.test.o $(level_1)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $@.o $(level_1)

raggedArray.test: raggedArray.test.o $(level_3)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $@.o $(level_3)

dataArray.test: dataArray.test.o $(level_4)
	@echo Making $@ from $(level_4)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $@.o $(level_4)

gsModel.test: gsModel.test.o $(level_5)
	@echo Making $@ from $(level_5)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $@.o $(level_5)

driver: driver.o $(level_5)
	@echo Making $@ from $(level_5)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $@.o $(level_5)


%.test.o: %.test.c
	$(CC) $(CFLAGS) -c $< -o $@ 

%.o: %.c
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

%.test.d: %.test.c
	$(CC) -MM $(CPPFLAGS) $< | sed 's/$*\.o/& $@/g' > $@

%.test.d: %.test.c
	$(CC) -MM $(CPPFLAGS) $< | sed 's/$*\.o/& $@/g' > $@

%.d: %.c %.h
	$(CC) -MM $(CPPFLAGS) $< | sed 's/$*\.o/& $@/g' > $@

# include $(SOURCES:.c=.d)

# include $(TEST_SOURCES:.c=.d)

#####################  EOF  ###############################################
