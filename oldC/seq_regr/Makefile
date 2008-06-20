# $Id: Makefile,v 1.38 2004/08/04 02:59:20 bob Exp $

###########################################################################
#
#        Options
#
#   Examples of depending on libaries.  Add names here
#        LDLIBS =  -lthing -lregression 
#   and give the LDFLAGS to local dir if needed
#        LDFLAGS = -Llib 
# 
###########################################################################

C_PATH = $(HOME)/C

PROJECT_NAME = seq_regr

OPT      = -O2 -DNDEBUG

INCLUDES = -I. -I$(C_PATH)/spline -I$(C_PATH)/utils -I$(C_PATH)/random  -I$(C_PATH)/ranges -I-

# libs from top down
LDLIBS   =  -lspline -lranges -lutils -lrandom -lgsl -lgslcblas -lm

LDFLAGS  = -L$(C_PATH)/spline -L$(C_PATH)/utils -L$(C_PATH)/random -L$(C_PATH)/ranges -L/usr/local/lib/


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = gsl_regr.o  threshold.o log_regr.o  c45_syntax.o column.o
level_1   = transpose.o seq_regr.o 
level_2   = 

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

include ../rules_for_makefiles

seq_regr: seq_regr_cmd.o $(above_infinity)
	g++ $(CFLAGS) $(INCLUDES) seq_regr_cmd.o $(above_infinity) -o $@

transpose: transpose.o column.o 
	g++ $(CFLAGS) $(INCLUDES) transpose.o column.o -o $@

# Analysis
# bank_small is test file with y + 20 predictors

bank_post45.dat:
	-rm -f bank_post45.dat bank_post45.rows bank_post45.names
	c45_syntax.test.exec --input=test/bank -v > bank_post45.rows
	c45_syntax.test.exec --input=test/bank -n > bank_post45.names
	c45_syntax.test.exec --input=test/bank -r > bank_post45.dat

bank_small.dat: bank_post45.dat Makefile                           # 203 gives 8th power
	head -n 203 bank_post45.dat > bank_small.dat               # 2 lines per feature + 1 for n + 2 for y
	wc bank_small.dat

c45_test:
	-rm -f test/c45_test.names test/c45_test.rows
	c45_syntax.test.exec --input=test/c45_good -n  >  test/c45_test.names
	c45_syntax.test.exec --input=test/c45_good -v  >  test/c45_test.rows

bank:   bank_post45.dat log_regr.test.exec c45_syntax.test.exec
	cat bank_post45.dat | log_regr.test.exec  -p .01

time:   bank_small.dat log_regr.test.exec c45_syntax.test.exec
	time cat bank_small.dat | log_regr.test.exec  -p .5       # was .01 for runs

jmp_test: test/lregr.dat log_regr.test.exec                       # cmp to JMP results from test.jmp
	log_regr.test.exec -f test/lregr.dat -p .3                # signs should be reversed from JMP, but rest agree


# All run under emacs, 50 preds,  iMacG4 school
#                   cin                                     scanf
#            O 0             O 2                     O 0             O 2
# real	38.954 43.011   32.996 36.109         1m15.087 1m9.15    27.66 28.20  
# user	23.160 22.950   20.890 20.850	        45.450  45.67	 12.14 12.14
# sys	 0.85s  1.06s	 1.04s	0.85s	         1.31s   1.2s	  0.70s	1.21

# G17, some with ibm compiler
# real	0m16.300s 0m14.002
# user	0m9.640s  0m9.590s
# sys	0m0.290s  0m0.550s
