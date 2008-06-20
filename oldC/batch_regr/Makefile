# $Id: Makefile,v 1.4 2003/08/23 21:56:58 bob Exp $


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

PROJECT_NAME = batch_regr

OPT      = -O2 -DNDEBUG

INCLUDES = -I. -I$(C_PATH)/utils  -I$(C_PATH)/ranges -I-

# ranges do not generate object code, so nothing to add to lib

LDLIBS   = -lm  -lutils 
LDFLAGS  = -L$(C_PATH)/utils


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = datasets.o
level_1   = covariance_matrix.o
level_2   = sweep_matrix.o
level_3   = projector.o
level_4   = raters.o 

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

include ../rules_for_makefiles


controller.exec: $(above_infinity)
	$(CC) $(above_infinity) $(LDLIBS) -o $@
