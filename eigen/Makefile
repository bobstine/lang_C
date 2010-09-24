include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = eigen

# slow and careful
# OPT = -O0 
# fast and who gives a damn about errors?

# OPT = -O3 -DNDEBUG

USES = random utils

EXTERNAL_USES = 


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = eigen_svd.o  regression.o


############################################################################
#
#        Rules and definitions
#
############################################################################

include ../rules_for_makefiles


rand_projection.o: rand_projection.cc 
	$(GCC) $(CFLAGS) -std=c++0x -c $^ -o $@

rand_projection: rand_projection.o eigen_svd.o
	$(GCC) $^ $(LDLIBS)  -o  $@


