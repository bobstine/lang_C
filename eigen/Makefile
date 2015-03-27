include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = eigen

USES = random utils

OPT = -O3  -std=c++11

# EXTERNAL_USES = boost_system boost_thread gomp
EXTERNAL_USES = 

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   =  little_functions.o fstatistic.o eigen_iterator.o confusion_matrix.o
level_1   =  linear_regression.o
level_2   =  validated_regression.o eigen_svd.o 

############################################################################
#
#        Rules and definitions
#
############################################################################

include ../rules_for_makefiles


tutorial: tutorial.o
	$(GCC) $^ $(LDLIBS)  -o  $@

perm_test: perm_test.o
	$(GCC) $^ $(LDLIBS)  -o  $@

rand_projection.o: rand_projection.cc 
	$(GCC) $(CFLAGS) -std=c++0x -c $^ -o $@

rand_projection: rand_projection.o eigen_svd.o
	$(GCC) $^ $(LDLIBS)  -o  $@

vectorize.s: vectorize.cc
	$(GCC) -mfpmath=sse -msse2  $^ -O2 -c -S -o vectorize.s  # comment no longer appears in asm?


stoc_grad: stoc_grad.o
	$(GCC) $^ $(LDLIBS) -o $@

stoc_grad_test: stoc_grad
	rm -rf estimates.*
	./stoc_grad -n    1000 -p   100 -k 1  > estimates.1
	./stoc_grad -n  100000 -p  1000 -k 1  > estimates.2
	./stoc_grad -n 1000000 -p 10000 -k 1  > estimates.3
