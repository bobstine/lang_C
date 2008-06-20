# $Id: Makefile,v 1.1 2004/08/06 17:57:54 bob Exp $

###########################################################################
#
#        Options
#
#   Examples of depending on libaries
#        LDLIBS =  -lthing -lregression 
#        libs_used =  ../lib/libthing.a ../lib/libregression.a 
# 
###########################################################################

PROJECT_NAME = calibrators

C_PATH = $(HOME)/C

INSTALL_PATH = $(C_PATH)/$(PROJECT_NAME)

OPT = -O2 -DNDEBUG

INCLUDES = -I. -I$(C_PATH)/random -I$(C_PATH)/ranges -I-

# libs in order of top level then down
LDLIBS   =  -lrandom  -lgsl -lgslcblas -lm

LDFLAGS  =  -L$(C_PATH)/random -L$(C_PATH)/ranges -L/usr/local/lib/

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = calibrators.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
#
###########################################################################

include ../rules_for_makefiles


