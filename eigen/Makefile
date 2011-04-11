include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = eigen

USES = random utils

OPT = -std=c++0x -O1

# EXTERNAL_USES = 


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   =  fstatistic.o 
level_1   =  eigen_svd.o  regression.o

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


