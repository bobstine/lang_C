# $Id: Makefile,v 1.4 2004/08/17 20:59:42 bob Exp $

###########################################################################
#
#        Options
#
#   Examples of depending on libaries
#        LDLIBS =  -lthing -lregression 
#        libs_used =  ../lib/libthing.a ../lib/libregression.a 
# 
###########################################################################

PROJECT_NAME = spline

C_PATH = $(HOME)/C

INSTALL_PATH = $(C_PATH)/$(PROJECT_NAME)

# OPT = -O2 -DNDEBUG

INCLUDES = -I. -I$(C_PATH)/ranges -I$(C_PATH)/random -I$(C_PATH)/utils -I-

# libs in order of top level then down
LDLIBS   =  -lrandom -lutils -lm
LDFLAGS  =  -L$(C_PATH)/random -L$(C_PATH)/utils 

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = linpack.o
level_1   = smoothing_spline.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
#
###########################################################################

include $(C_PATH)/rules_for_makefiles

splsm: splsm.cc smoothing_spline.o
	g++ $(CFLAGS) $(INCLUDES) splsm.cc $(above_infinity) -o $@


