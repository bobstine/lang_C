##
##  Makefile for GNU sci lib functions
##

CC = gcc $(INCLUDES) $(LDFLAGS) 

# slow and careful
#OPT = -O0 
# fast and who gives a damn about errors?
OPT = -O2 -DNDEBUG

# LDLIBS =  -lgsl -lgslcblas -lm

# use ATLAS cblas routines
LDLIBS =  -lgsl -lcblas -latlas -lm 


################################################################################
# -Wall                 : the first set of primary warnings
# -Wold-style-cast      : generates way to many errors in the g++ library code
# -Weffc++              : cool, but turn on FEWER_WARN to avoid extranious warnings
# -W                    : turns on a few extras
# -Werror               : treats all warnings as errors
# -Woverloaded-virtual  : catches mismatched signatures in virtual overloads
# -Winline		: says if a function can't be inlined

CFLAGS = -Wall -Werror -Winline $(OPT)

#########################################################

.PHONY : clean
clean:
	-rm -f sweeper driver *.o *.d
	-rm core

%.test.o: %.test.cc
	$(CC) $(CFLAGS) -c $< -o $@ 

%.o: %.cc
	$(CC) $(CFLAGS) -c $< -o $@ 

%.exec: %.o
	$(CC) $^ $(LDLIBS) -o $@ $(NAME)