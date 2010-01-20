# $Id: Makefile,v 1.24 2008/01/04 14:32:29 bob Exp $

include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = wiki

OPT = -O2      # -DNDEBUG  this option kills benefit of assert

USES = utils 


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

# wiki.sdata: prepare_data the_vs_her.txt
#	./prepare_data --keyword-file=BNC_word_list.txt --min-frequency=1000 --num-tokens=49 --num-sentences=40 < the_vs_her.txt > $@

wiki.sdata: prepare_data red_vs_blue.txt
	./prepare_data --target-word=blue --keyword-file=BNC_word_list.txt --min-frequency=1000 --num-tokens=49 --num-sentences=1800 < red_vs_blue.txt > $@

###########################################################################

include ../rules_for_makefiles

