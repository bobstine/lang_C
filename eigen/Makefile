include ../c_flags

###########################################################################
#
#        Options
#
#     -Ofast         same as "-O3 -ffast-math" enables optimizations (eg floating point reassociation)
#     -flto          enable link time optimizations  (failed in subsequent link?)
#     -mfpmath=sse   enables use of XMM registers in floating point instructions (instead of stack in x87 mode)
#     -funroll-loops enables loop unrolling
#
###########################################################################

PROJECT_NAME = eigen

USES = random utils

# OPT = -Ofast -mfpmath=sse -msse3 -m64 -march=native

OPT = -O0

EXTERNAL_USES = 

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   =  little_functions.o fstatistic.o confusion_matrix.o
level_1   =  eigen_iterator.o 
level_2   =  linear_regression.o eigen_svd.o 
level_3   =  validated_regression.o 

############################################################################
#
#        Rules and definitions
#
############################################################################

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

##############################################################################

include ../rules_for_makefiles

