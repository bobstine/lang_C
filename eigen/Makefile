include ../c_flags

##
##  Makefile for testing Eigen functions
##

PROJECT_NAME = eigen

USES = 


# slow and careful
OPT = -O0 
# fast and who gives a damn about errors?
# OPT = -O2 -DNDEBUG


######################################################
include ../rules_for_makefiles


%.test.o: %.test.cc
	$(CC) $(CFLAGS) -c $< -o $@ 

%.o: %.cc
	$(CC) $(CFLAGS) -c $< -o $@ 

%.exec: %.o
	$(CC) $^ $(LDLIBS) -o $@ $(NAME)

