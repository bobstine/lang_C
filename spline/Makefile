# $Id: Makefile,v 1.7 2005/06/14 22:10:27 bob Exp $

include ../c_flags

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

USES = ranges random utils

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

include ../rules_for_makefiles

splsm: splsm.cc smoothing_spline.o
	g++ $(CFLAGS) $(INCLUDES) splsm.cc $(above_infinity) -o $@
