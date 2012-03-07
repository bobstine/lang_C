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

# OPT = -O3 -std=c++0x

OPT =

# OPT = -O3 -DNDEBUG

USES = utils random

# EXTERNAL_USES = gsl

level_0 = bellman.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################


# -------------------------------------------------------------------
# bellman recursion for competitive value
# -------------------------------------------------------------------

bellman_solver.o: bellman_solver.cc

bellman: bellman.o bellman_solver.o
	$(GCC) $^ $(LDLIBS) -o  $@

bellman_test: bellman
	./bellman --gamma 2.0 --rounds 100 --prob u     # add   --write    for details

# Unconstrained 0 2.5 0.05 7 0.5 1.5 6.5 -0.0691835 0.068553 0.0550946
bellman_check: bellman
	./bellman --constrain 0 --gamma 2.5 --rounds 7  --prob u --spend 0.5 --write

# ---  $^ are prereq    $@ is target    $* is stem
#      change n to change path, file names, and the length of run;  gp is gamma path
#      need to make that before run this

n = 500
gp = gamma.$(n)/

#  --- lists of gp arguments
#  $(gp)0.6 $(gp)0.65 $(gp)0.7 $(gp)0.75 $(gp)0.8 $(gp)0.85 $(gp)0.9 $(gp)0.95 $(gp)0.960 $(gp)0.970 $(gp)0.980 $(gp)0.990 $(gp)1.0 $(gp)1.05 $(gp)1.1 $(gp)1.15 $(gp)1.2 $(gp)1.3 $(gp)1.4 $(gp)1.5 $(gp)1.6 $(gp)1.7
#  $(gp)0.91 $(gp)0.92 $(gp)0.93 $(gp)0.94 $(gp)0.95 $(gp)0.96 $(gp)0.97 $(gp)0.98 $(gp)0.99
#  $(gp)0.952 $(gp)0.954 $(gp)0.956 $(gp)0.958  $(gp)0.951 $(gp)0.953 $(gp)2.0 $(gp)2.2
#   $(gp)0.6  $(gp)0.7  $(gp)0.8  $(gp)0.9  $(gp)1.0  $(gp)1.1  $(gp)1.2 $(gp)1.3 $(gp)1.4 $(gp)1.5
#   $(gp)4.0  $(gp)4.5  $(gp)5.0  $(gp)5.5  $(gp)6.0  $(gp)6.5  $(gp)7.0 $(gp)7.5 $(gp)8.0 $(gp)8.5 $(gp)9.0 $(gp)9.5 $(gp)10.0
#   $(gp)12   $(gp)14   $(gp)16   $(gp)18   $(gp)20   $(gp)25   $(gp)30  $(gp)35  $(gp)40  $(gp)50  $(gp)60  $(gp)70  $(gp)80  $(gp)90 $(gp)100

bellman_results.reject_$(n): bellman bellman.sh $(gp)200  $(gp)300  $(gp)400  $(gp)500  $(gp)600  $(gp)700 $(gp)800 $(gp)900 $(gp)1000
	cat $(filter $(gp)%,$^) >> $@

$(gp)%: bellman bellman.sh
	./bellman --gamma $* --prob u --rounds $(n) > $@


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

