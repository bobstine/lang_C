# $Id: Makefile,v 1.8 2003/07/21 21:57:50 bob Exp $

PROJECT_NAME = simulate

OPT      = -O2 -DNDEBUG

INCLUDES = -I. -I$(C_PATH)utils -I$(C_PATH)random -I$(C_PATH)ranges -I$(C_PATH)seq_regr -I$(C_PATH)batch_regr -I-

# ranges do not generate object code, so nothing to add to lib

LDLIBS   = -lm -lseq_regr -lbatch_regr -lgsl -lgslcblas -lutils -lrandom
LDFLAGS  = -L$(C_PATH)utils -L$(C_PATH)random -L$(C_PATH)seq_regr -L$(C_PATH)batch_regr


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = simulate.o generate-x.o


############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

include ../rules_for_makefiles

generate-x.exec: $(above_infinity)
	$(CC) generate-x.o $(LDLIBS) -o $@

double-boot.exec: double-boot.o
	$(CC) double-boot.o $(LDLIBS) -o $@


simulate.exec: $(above_infinity) ../batch_regr/libbatch_regr.a ../seq_regr/libseq_regr.a
	$(CC) simulate.o $(LDLIBS) -o $@

bigx.dat: generate-x.exec
	 generate-x.exec -n 500 -p 100 -s 27534 -o test/bigx.dat

simres: $(above_infinity) simulate.exec test/bigx.dat Makefile
	simulate.exec -v .2 -k 20     -c a    -p .80      -r 500  -s 23432  -x test/bigx.dat      -o simres

#####
##  Options:
##            v population R2
##            k number of non-zero betas
##	      p permutation fraction for scrambling X's
##            c criterion (adaptive, bonferroni)
##            r simulated replications
##            s seed
##            x file with x values (also gives n and p=number of X cols)
#####

mands: simres
	../utils/mands < simres > mands





