
# $Id: Makefile,v 3.3 2006/03/05 02:57:40 foster Exp $

include c_flags

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

PROJECT_NAME = rkhs_svd

OPT      = -O2 -DNDEBUG

USES = gsl gslcblas

all: svd svd_sample random_svd


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = svd.o svd_sample.o random_svd.o
level_1   = 
level_2   = 

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

include rules_for_makefiles


c_flags: 
	ln -s ../c_flags

rules_for_makefiles:
	ln -s ../rules_for_makefiles
