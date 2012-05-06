include ../c_flags

###########################################################################
#
#        Options
#   Examples of depending on libaries
#        LDLIBS =  -lthing -lregression 
#        libs_used =  ../lib/libthing.a ../lib/libregression.a 
# 
###########################################################################


PROJECT_NAME = bellman

OPT = -O3 -std=c++0x -DNDEBUG

# OPT =  -std=c++0x

USES = utils random

# EXTERNAL_USES = gsl

level_0 = wealth.o
level_1 = utility.o
level_2 = bellman.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################


# TAGS 
# find . | grep ".*\.\(h\|cc\)" | xargs etags -f TAGS

# -------------------------------------------------------------------
# bellman recursion for competitive value
#
# constrained:  univ univ 2 0.05   50   0.5 7     -0.015669 0.015669 0.015669
# uncons       uncon univ 2 0.05   50   0.05 7     18.1857 22.6992 2.25676

# -------------------------------------------------------------------

constrained_test: bellman
	./bellman --gamma 2 --rounds 50 --oracle u --oracleprob 0.5 --bidder u --bidderprob 0.5 --write   

unconstrained_test: bellman
	./bellman --gamma 2 --rounds 50                             --bidder u --bidderprob 0.5 --write   



bellman_main.o: bellman_main.cc

bellman: bellman.o wealth.o utility.o bellman_main.o
	$(GCC) $^ $(LDLIBS) -o  $@

bellman_test: bellman
	./bellman --gamma 2.0 --rounds 250 --prob u --write    # add   --write    for details

# Unconstrained 0 2.5 0.05 7 0.5 1.5 6.5 -0.0691835 0.068553 0.0550946
bellman_check: bellman
	./bellman --constrain 0 --gamma 2.5 --rounds 7  --prob u --spend 0.5 --write

# ---  $^ are prereq    $@ is target    $* is stem
#      change n to change path, file names, and the length of run;  gp is path
#      Once run, cat combines these lines to show all of the results.

n = 250
expert_gamma = 0.80
eg = 080

#  --- below here is automagic, building output in runs/   start by defining subdirectory path gp
gp = runs/reject_g$(eg)_$(n)

$(gp)/.directory_built: 
	echo Building directory for $(gp)
	mkdir $(gp)
	touch $@

runs/summary.reject_g$(eg)_$(n): bellman bellman.sh $(gp)/0.5 $(gp)/0.55 $(gp)/0.6 $(gp)/0.65 $(gp)/0.675 $(gp)/0.7 $(gp)/0.705 $(gp)/0.710 $(gp)/0.715 $(gp)/0.720 $(gp)/0.725 $(gp)/0.7275 $(gp)/0.7285 $(gp)/0.7290 $(gp)/0.7295 $(gp)/0.73 $(gp)/0.7305 $(gp)/0.731 $(gp)/0.7315 $(gp)/0.7320 $(gp)/0.7325 $(gp)/0.735 $(gp)/0.740 $(gp)/0.745 $(gp)/0.75 $(gp)/0.755 $(gp)/0.760 $(gp)/0.765 $(gp)/0.770 $(gp)/0.775 $(gp)/0.8 $(gp)/0.825  $(gp)/0.85 $(gp)/0.875 $(gp)/0.9 $(gp)/0.925 $(gp)/0.95 $(gp)/0.975 $(gp)/0.990 $(gp)/1.0 $(gp)/1.01 $(gp)/1.05 $(gp)/1.1 $(gp)/1.15 $(gp)/1.2 $(gp)/1.3 $(gp)/1.4 $(gp)/1.5 $(gp)/1.6 $(gp)/1.7 $(gp)/2.0 $(gp)/2.5 $(gp)/3.0 $(gp)/3.5 $(gp)/4.0  $(gp)/4.5  $(gp)/5.0  $(gp)/5.5  $(gp)/6.0  $(gp)/6.5  $(gp)/7.0 $(gp)/7.5 $(gp)/8.0  $(gp)/9.0 $(gp)/10.0 $(gp)/15.0 $(gp)/20.0
	rm -f $@
	cat $(filter $(gp)/%,$^) >> $@

$(gp)/%: bellman bellman.sh $(gp)/.directory_built
	./bellman --gamma $* --constrain $(expert_gamma) --prob u --rounds $(n) > $@


# ---  unconstrained
#   solutions for testing
#      2.5 25 1 u 0.5 -0.0332263 0.181596 0.0859291
#      2.5 10 1 u 0.5 -0.0651093 0.086978 0.0608351 	
bellman_results.u500: bellman bellman.sh  
	rm -f bellman_results.500
	./bellman.sh u 500 >  bellman_results.500
	./bellman.sh e 500 >> bellman_results.500
	./bellman.sh g 500 >> bellman_results.500


bellman_results.u1000: bellman bellman.sh
	rm -f bellman_results.1000
	./bellman.sh u 1000 >  bellman_results.1000
	./bellman.sh e 1000 >> bellman_results.1000
	./bellman.sh g 1000 >> bellman_results.1000


bellman_results.u2000: bellman bellman.sh
	rm -f bellman_results.2000
	./bellman.sh u 2000 >  bellman_results.2000
	./bellman.sh e 2000 >> bellman_results.2000
	./bellman.sh g 2000 >> bellman_results.2000





###########################################################################
include ../rules_for_makefiles

