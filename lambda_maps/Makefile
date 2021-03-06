# 

COMPILER = gcc

LIBTOOL_CMD = libtool  -static -o $@ $(above_infinity)

CFLAGS = -g -Wall -USINGLE -W -Woverloaded-virtual $(OPT) -fmessage-length=0
GCC = g++ $(INCLUDES) $(LDFLAGS) 
CC  = g++ $(INCLUDES) $(LDFLAGS)


###########################################################################
#
#        Options
#
###########################################################################


PROJECT_NAME = map_lambda

OPT      =  -O4  -DNDEBUG


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

#           TESTED					UNTESTED
level_0   = range.o                               composer.o evaluator.o 
level_1   = range_traits.o
level_2   = function_iterators.o 



all: function_iterators.test function_iterators.test.o
	./function_iterators.test

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################

above_1 =              $(level_0)  
above_2 = $(above_1)   $(level_1)  
above_3 = $(above_2)   $(level_2)  
above_4 = $(above_3)   $(level_3)  
above_5 = $(above_4)   $(level_4)  
above_6 = $(above_5)   $(level_5)  
above_7 = $(above_6)   $(level_6)  
above_8 = $(above_7)   $(level_7)  
above_9 = $(above_8)   $(level_8)  

above_infinity = $(above_9)

$(level_0:.o=.test):              
$(level_1:.o=.test):  $(above_1)  
$(level_2:.o=.test):  $(above_2)  
$(level_3:.o=.test):  $(above_3)  
$(level_4:.o=.test):  $(above_4)  
$(level_5:.o=.test):  $(above_5) 
$(level_6:.o=.test):  $(above_6) 
$(level_7:.o=.test):  $(above_7) 
$(level_8:.o=.test):  $(above_8) 
$(level_9:.o=.test):  $(above_9) 

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

vpath %.a $(patsubst %, ../%, $(USES))

INCLUDES = -I. -I /usr/local/include $(patsubst %,-I%,$(patsubst %, %, $(HEADERS)))  $(patsubst %,-I%,$(patsubst %, ../%, $(USES)))
# names of libraries
LDLIBS = $(patsubst %,-l%,$(USES)) $(patsubst %,-l%,$(EXTERNAL_USES)) -lm 
# paths
LDFLAGS = $(patsubst %,-L%,$(patsubst %, ../%, $(USES)))

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

sources = $(addsuffix .cc, $(basename $(above_infinity)))
test_sources = $(wildcard *.test.cc)

-include $(sources:.cc=.dd)
-include $(sources:.cc=.test.dd)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#
#           Targets
# 
#  Order shouldn't matter--but for Gnu-Make it does.
#
#	-u will only copy newer files, so make copying times faster and make times are shorter.
#       -p will keep the time stop the same.  So it makes the -u unneed.
#	cp -uf *.h ../Include
#
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

.PHONY : clean
clean:
	-rm -f     *.o *.d *.dd *.exec
	-rm        core


test: test.o $(above_infinity)
	$(GCC) -o test $(LDLIBS) test.o $(above_infinity) $(USELIBS) | c++filt

%.test: %.test.o %.o
	@echo Includes are: $(INCLUDES)
	@echo Load libs are: $(LDLIBS)
	@echo Making executable test program. $@
	$(GCC) $^ $(LDLIBS) -o  $@

%.o: %.cc %.h
	$(GCC) $(CFLAGS) -c $< -o $@ 

# naked version without a header
%.o: %.cc
	$(GCC) $(CFLAGS) -c $< -o $@ 


%.test.o: %.test.cc  # does not require a .h file
	$(GCC) $(CFLAGS) -c $< -o $@ 


install:  $(INSTALL_PATH)/Include  lib$(PROJECT_NAME).a 

$(INSTALL_PATH)/Include:
	mkdir $(INSTALL_PATH)/Include

$(INSTALL_PATH)/lib/lib$(PROJECT_NAME).a: lib$(PROJECT_NAME).a 
	-chmod +w $(INSTALL_PATH)/Include/*.h
	-cp -pf *.h $(INSTALL_PATH)/Include
	-chmod -w $(INSTALL_PATH)/Include/*.h
	-cp -f $< $@


lib: lib$(PROJECT_NAME).a

lib$(PROJECT_NAME).a: $(above_infinity) 
	-rm lib$(PROJECT_NAME).a
	$(LIBTOOL_CMD)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#
#               DEPENDENCIES
#
# From the GNU make info node and _Gnu make_ book (neither worked unmodified)
# book version:	$(CC) -M $(CPPFLAGS) $< | sed '\''s/\($*\)\.o[ :]*/\1 $@/g'\'' > $@
# info version:	$(CC) -M $(CPPFLAGS) $< | sed 's/$*.o/& $@/g' > $@
#
# The -rm kills off any .d file that is made with zero length.  It probably
# could be made shorter and clearer.
#
# Using MM option suppresses the system dependencies (use M otherwise)

# these work--but the two below are more "modern"
#
# %.test.d: %.test.cc %.h
#	$(GCC) -M $(CPPFLAGS) $< | sed 's/$*\.o/& $@/g' > $@
#	-rm `ls -lF *.d | grep " 0 " | sed "s/.* //g" | tr "\n" " "`
#
# %.d: %.cc %.h
#	$(GCC) -M $(CPPFLAGS) $< | sed 's/$*\.o/& $@/g' > $@
#	-rm `ls -lF *.d | grep " 0 " | sed "s/.* //g" | tr "\n" " "`
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

%.dd: %.cc
	set -e; $(GCC) $(CFLAGS) -MM $(CPPFLAGS) $*.cc   \
        | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $@; [ -s $@ ] || rm -f $@

%.test.dd: %.test.cc
	set -e; $(GCC) $(INCLUDES) -MM $(CPPFLAGS) $< \
       | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $@; [ -s $@ ] || rm -f $@


##	set -e; $(GCC) $(INCLUDES) -MM $(CPPFLAGS) $< | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $@; [ -s $@ ] || rm -f $@
