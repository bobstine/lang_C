# $Id: Makefile,v 1.1 2005/06/13 20:47:50 bob Exp $

PROJECT_NAME = simulate

OPT      = -O2 -DNDEBUG

INCLUDES = -I. -I$../utils -I$../random -I$../ranges -I$../seq_regr -I$../batch_regr -I-

# ranges do not generate object code, so nothing to add to lib

LDLIBS   = -lm -lseq_regr -lbatch_regr -lgsl -lgslcblas -lutils -lrandom
LDFLAGS  = -L$../utils -L$../random -L$../seq_regr -L$../batch_regr


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = generate-x.o
level_1   =   simulate.o

###########################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

generate-x.o: generate-x.cc          # does not use .h file
	$(CC) $(CFLAGS) -c $< -o $@ 

generate-x.exec: generate-x.o
	$(CC) $< $(LDLIBS) -o $@ 


simulate.o: simulate.cc          # does not use .h file
	$(CC) $(CFLAGS) -c $< -o $@ 

simulate.exec: simulate.o ../batch_regr/libbatch_regr.a ../seq_regr/libseq_regr.a
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

#########################################################
include /Users/bob/C/rules_for_makefiles
#########################################################  EOF
