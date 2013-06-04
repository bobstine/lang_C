include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = eigen

USES = random utils

OPT = -O3 -fopenmp -std=c++11

EXTERNAL_USES = gomp


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   =  fstatistic.o stoc_grad.o
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

stoc_grad: stoc_grad.o
	$(GCC) $^ $(LDLIBS) -o $@

stoc_grad_test: stoc_grad
	rm -rf estimates.*
	./stoc_grad -n    1000 -p   100 -k 1  > estimates.1
	./stoc_grad -n  100000 -p  1000 -k 1  > estimates.2
	./stoc_grad -n 1000000 -p 10000 -k 1  > estimates.3
