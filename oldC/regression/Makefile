# $Id: Makefile,v 1.17 2003/12/03 16:41:02 bob Exp $


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

PROJECT_NAME = regression

OPT      = -O2 -DNDEBUG

INCLUDES = -I. -I$(C_PATH)/utils -I$(C_PATH)/random  -I$(C_PATH)/ranges  -I- # -I$(C_PATH)/gsl-1.1.1/gsl/ -I$(C_PATH)/gsl-1.1.1/

# ranges do not generate object code, so nothing to add to lib

# use these if atlas is around
#  LDLIBS   = -lm -lgsl -lcblas -latlas -lutils -lrandom
LDLIBS   = -lm -lgsl -lgslcblas -lutils -lrandom
LDFLAGS  =  -L. -L$(C_PATH)/utils -L$(C_PATH)/random # -L$(C_PATH)/gsl-1.1.1/.libs/ -L$(C_PATH)/gsl-1.1.1/cblas/.libs/


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = bennett.o               # envelope.o  observation.o 
level_1   = gsl_regr.o              # dataset.o
level_2	  = variable.o      
level_3   = model.o                 # statistic.o

# level_5   = controller.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

include ../rules_for_makefiles


controller.exec: $(above_infinity)
	$(CC) $(above_infinity) $(LDLIBS) -o $@
