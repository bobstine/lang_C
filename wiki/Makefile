# $Id: Makefile,v 1.24 2008/01/04 14:32:29 bob Exp $

include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = wiki

OPT = -O2      # -DNDEBUG  this option kills benefit of assert


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################


level_0 = prepare_data.o


############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################


all: wiki.sdata

prepare_data: prepare_data.o
	$(GCC) $< $(LDLIBS) -o $@

wiki.sdata: prepare_data the_vs_her.txt
	./prepare_data --keyword-file=BNC_word_list.txt --min-frequency=1000 --num-tokens=49 --num-sentences=40 < the_vs_her.txt > $@

###########################################################################

include ../rules_for_makefiles

